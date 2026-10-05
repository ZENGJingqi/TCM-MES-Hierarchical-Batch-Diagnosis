options(warn=1)
suppressPackageStartupMessages({library(dplyr);library(readr);library(tidyr);library(glmnet)})
stage<-Sys.getenv('TCM_STATE_STAGE');stopifnot(nzchar(stage));out<-file.path(stage,'output');dir.create(out,showWarnings=FALSE)
# Load only functions; do not execute any earlier analysis.
for(e in parse(file.path(stage,'quality.R')))if(is.call(e)&&identical(e[[1]],as.name('<-'))&&as.character(e[[2]])[1]%in%c('design','fit','auc'))eval(e)
x<-read_csv(file.path(stage,'joint.csv'),show_col_types=FALSE) |>
 mutate(mes_production_date=as.Date(mes_production_date),d2_observation_date=as.Date(d2_observation_date)) |>
 arrange(mes_production_date,finished_batch) |> mutate(id=sprintf('S-%04d',row_number()),target=disintegration_issue)
mes<-c('coating_yield_pct','final_blend_moisture_pct','coating_mass_balance_pct','compression_yield_pct','compression_hardness_mean_n','coated_tablet_weight_mean_g','final_blend_lt_100_mesh_pct','granulation_discharge_moisture_pct_mean','core_tablet_weight_mean_g')
starts<-as.Date(c('2026-01-01','2026-03-01','2026-05-01','2026-07-01'));ends<-as.Date(c('2026-02-28','2026-04-30','2026-06-30','2026-07-31'))
pred<-list();aud<-list();idx<-1;ai<-1
for(delay in c(0,14)){
 d<-x;d$available<-d$d2_observation_date+delay;d$available[d$d2_observation_date<d$mes_production_date]<-as.Date(NA)
 d$drift<-NA_real_;d$recent_rate<-NA_real_;d$recent_n<-0
 for(v in mes)d[[paste0(v,'_z')]]<-NA_real_
 for(i in 1:nrow(d)){
  t<-d$mes_production_date[i];pp<-which(d$mes_production_date<t)
  qq<-which(d$mes_production_date<t & !is.na(d$available) & d$available<t)
  rr<-qq[d$available[qq]>=t-60]
  if(length(qq))d$recent_rate[i]<-(sum(d$target[if(length(rr))rr else qq])+.5)/(if(length(rr))length(rr)+1 else length(qq)+1)
  d$recent_n[i]<-length(rr)
  if(length(pp)>=30){
   for(v in mes){mu<-median(d[[v]][pp],na.rm=TRUE);ss<-mad(d[[v]][pp],na.rm=TRUE);if(!is.finite(ss)||ss==0)ss<-sd(d[[v]][pp],na.rm=TRUE);if(!is.finite(ss)||ss==0)ss<-1;d[[paste0(v,'_z')]][i]<-(d[[v]][i]-mu)/ss}
   d$drift[i]<-mean(abs(as.numeric(d[i,paste0(mes,'_z')])))
  }
  stopifnot(all(d$mes_production_date[pp]<t),all(d$available[qq]<t))
 }
 for(v in mes){d[[paste0(v,'_drift')]]<-d[[paste0(v,'_z')]]*d$drift;d[[paste0(v,'_risk')]]<-d[[paste0(v,'_z')]]*d$recent_rate}
 vs<-c(mes,'drift','recent_rate','recent_n',paste0(mes,'_drift'),paste0(mes,'_risk'))
 write_csv(d |> select(id,mes_production_date,available,drift,recent_rate,recent_n),file.path(out,paste0('state_features_delay',delay,'.csv')))
 ref<-d |> filter(mes_production_date<starts[1],available<starts[1]);p0<-(sum(ref$target)+.5)/(nrow(ref)+1)
 sf<-fit(ref,d,mes,'binomial',20260907);d$static<-pmin(pmax(sf$p,1e-6),1-1e-6)
 for(s in 1:4){
  tr<-d |> filter(mes_production_date<starts[s],available<starts[s]);te<-d |> filter(mes_production_date>=starts[s],mes_production_date<=ends[s])
  retr<-fit(tr,te,mes,'binomial',20260907);state<-fit(tr,te,vs,'binomial',20260907)
  lp<-qlogis(tr$static);cf<-glm(tr$target~1+offset(lp),family=binomial());shift<-unname(coef(cf)[1])
  rate<-pmin(pmax(ifelse(is.na(te$recent_rate),p0,te$recent_rate),.005),.995)
  ps<-list(static=te$static,recalibration=plogis(qlogis(te$static)+shift),retraining=retr$p,state_interaction=state$p,state_prior=plogis(qlogis(te$static)+qlogis(rate)-qlogis(p0)))
  for(nm in names(ps)){pred[[idx]]<-tibble(delay=delay,window=s,id=te$id,model=nm,y=te$target,p=ps[[nm]]);idx<-idx+1}
  aud[[ai]]<-tibble(delay=delay,window=s,train_n=nrow(tr),train_events=sum(tr$target),test_n=nrow(te),static_status=sf$status,retrain_status=retr$status,state_status=state$status);ai<-ai+1
 }
}
p<-bind_rows(pred);stopifnot(all(is.finite(p$p)))
m<-p |> group_by(delay,window,model) |> group_modify(~{y<-.x$y;pr<-.x$p;n<-length(y);k<-ceiling(.2*n);cut<-sort(pr,decreasing=TRUE)[k];ab<-pr>cut;eq<-pr==cut;hit<-sum(y[ab])+(k-sum(ab))/sum(eq)*sum(y[eq]);tibble(n=n,events=sum(y),AUC=auc(y,pr),Brier=mean((pr-y)^2),capture20=hit/sum(y),random_capture=k/n)}) |> ungroup()
write_csv(p,file.path(out,'predictions.csv'));write_csv(bind_rows(aud),file.path(out,'fit_audit.csv'));write_csv(m,file.path(out,'window_metrics.csv'))
pool<-m |> group_by(delay,model) |> summarise(Brier=weighted.mean(Brier,n),capture20=weighted.mean(capture20,events),random_capture=weighted.mean(random_capture,events),n_total=sum(n),.groups='drop')
stopifnot(all(pool$n_total==755));write_csv(pool,file.path(out,'pooled_metrics.csv'))
# Dynamic programming, same binomial likelihood and parameter count as original exhaustive implementation.
monthly<-read_csv(file.path(stage,'monthly.csv'),show_col_types=FALSE)
solve_dp<-function(d,minlen,cap,criterion){
 n<-nrow(d);K<-min(cap,floor(n/minlen));dp<-matrix(-Inf,K,n);paths<-vector('list',K*n)
 seg<-function(a,b){ev<-sum(d$event_n[a:b]);nn<-sum(d$total_n[a:b]);r<-min(max(ev/nn,1e-8),1-1e-8);ev*log(r)+(nn-ev)*log1p(-r)}
 for(j in minlen:n){dp[1,j]<-seg(1,j);paths[[j]]<-integer(0)}
 if(K>=2)for(k in 2:K)for(j in (k*minlen):n){ends<-((k-1)*minlen):(j-minlen);scores<-sapply(ends,function(t)dp[k-1,t]+seg(t+1,j));q<-which.max(scores);t<-ends[q];dp[k,j]<-scores[q];paths[[(k-1)*n+j]]<-c(paths[[(k-2)*n+t]],t)}
 score<- -2*dp[,n]+(2*(1:K)-1)*if(criterion=='BIC')log(sum(d$total_n)) else 2
 k<-which.min(score);cuts<-paths[[(k-1)*n+n]]
 tibble(selected_segments=k,criterion_value=score[k],cut_after_months=paste(substr(d$observation_month[cuts],1,7),collapse=';'),at_effective_cap=k==K)
}
grid<-expand_grid(threshold_min=c(9,10,11),min_segment_months=c(1,2,3),criterion=c('AIC','BIC'),max_segments=3:8)
res<-bind_rows(lapply(1:nrow(grid),function(i){g<-grid[i,];d<-monthly |> filter(threshold_min==g$threshold_min) |> arrange(observation_month);bind_cols(g,solve_dp(d,g$min_segment_months,g$max_segments,g$criterion))}))
old<-read_csv(file.path(stage,'oldgrid.csv'),show_col_types=FALSE)
check<-res |> filter(max_segments==5) |> inner_join(old,by=c('threshold_min','min_segment_months','criterion'),suffix=c('_new','_old'))
stopifnot(nrow(check)==18,all(check$selected_segments==check$segment_n),all(check$cut_after_months_new==check$cut_after_months_old),max(abs(check$criterion_value_new-check$criterion_value_old))<1e-6)
write_csv(res,file.path(out,'changepoint_max_segments_grid.csv'))
writeLines(c('PASS: state features use only earlier production records and assumed available outcomes; no later reference scaling.','PASS: nested time tuning includes label availability and fold-local preprocessing.','PASS: 5 strategies x 2 hypothetical delays x 4 windows; 755 test batches each.','PASS: 108 segmentation configurations; max=5 exactly reproduces 18 previous configurations.','CAVEAT: all result-availability delays are hypothetical; current-batch MES features require process completion.','CAVEAT: retrospective QC change segments are not online physical state labels.','CAVEAT: model classes were revised; differences from legacy outputs combine tuning and feature changes.','CAVEAT: state-prior updates within window while fitted coefficients stay frozen; full-window review budget is retrospective.'),file.path(out,'QA.txt'))
capture.output(sessionInfo(),file=file.path(out,'sessionInfo.txt'));print(pool);print(res |> filter(threshold_min==10,min_segment_months==2,criterion=='BIC'))
