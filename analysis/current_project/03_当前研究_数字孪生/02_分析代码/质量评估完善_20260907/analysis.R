options(warn=1)
suppressPackageStartupMessages({library(dplyr);library(readr);library(glmnet);library(tidyr)})
stage<-Sys.getenv('TCM_QUALITY_STAGE');stopifnot(nzchar(stage))
out<-file.path(stage,'output');dir.create(out,showWarnings=FALSE)
x<-read_csv(file.path(stage,'joint.csv'),show_col_types=FALSE) |>
 mutate(mes_production_date=as.Date(mes_production_date),d2_observation_date=as.Date(d2_observation_date)) |>
 arrange(mes_production_date,finished_batch) |>
 mutate(id=sprintf('Q-%04d',row_number()),month=format(mes_production_date,'%Y-%m'),lag=as.integer(d2_observation_date-mes_production_date))
stopifnot(nrow(x)==1477,!anyDuplicated(x$finished_batch),all(is.finite(x$disintegration_time_min)))
# Continuous target is the mean of repeated QC measurements per batch, as defined by the authoritative matrix builder.
mes<-c('coating_yield_pct','final_blend_moisture_pct','coating_mass_balance_pct','compression_yield_pct','compression_hardness_mean_n','coated_tablet_weight_mean_g','final_blend_lt_100_mesh_pct','granulation_discharge_moisture_pct_mean','core_tablet_weight_mean_g')
up<-c('extract_total_ash_pct','extract_extract_pct','chenpi_hesperidin_pct_mean','chenpi_moisture_pct_mean','yam_rejected_material_rate_pct','yam_through_120_mesh_mean_pct')
miss<-paste0(up,'_missing');x[miss]<-lapply(x[up],function(v)as.integer(is.na(v)))
sets<-list(MES=mes,material=up,MES_material=c(mes,up),MES_missingness=c(mes,miss))
write_csv(x |> group_by(month) |> summarise(n=n(),median=median(disintegration_time_min),q25=quantile(disintegration_time_min,.25),q75=quantile(disintegration_time_min,.75),min=min(disintegration_time_min),max=max(disintegration_time_min),mean=mean(disintegration_time_min),sd=sd(disintegration_time_min),any_record_gt10=mean(disintegration_issue),mean_gt9=mean(disintegration_time_min>9),mean_gt10=mean(disintegration_time_min>10),mean_gt11=mean(disintegration_time_min>11),.groups='drop'),file.path(out,'01_monthly_continuous_quality.csv'))
write_csv(tibble(variable=up,n_missing=sapply(x[up],function(z)sum(is.na(z)))),file.path(out,'02_material_missingness.csv'))
# All medians/scales are estimated from training data. Missingness flags are added for every input feature, independently of test data.
design<-function(tr,te,vs){
 a<-as.matrix(tr[vs]);b<-as.matrix(te[vs]);storage.mode(a)<-'double';storage.mode(b)<-'double'
 ia<-is.na(a)*1;ib<-is.na(b)*1
 for(j in seq_along(vs)){med<-median(a[,j],na.rm=TRUE);if(!is.finite(med))med<-0;a[is.na(a[,j]),j]<-med;b[is.na(b[,j]),j]<-med;mu<-mean(a[,j]);ss<-sd(a[,j]);if(!is.finite(ss)||ss==0)ss<-1;a[,j]<-(a[,j]-mu)/ss;b[,j]<-(b[,j]-mu)/ss}
 list(a=cbind(a,ia),b=cbind(b,ib))
}
# Fixed ridge/elastic-net mixture and inner chronological validation; preprocessing is recomputed inside each inner split.
fit<-function(tr,te,vs,family,seed){
 y<-tr$target;lam<-10^seq(1,-4,length.out=35)
 if(family=='binomial' && (length(unique(y))<2 || min(table(y))<3))return(list(p=rep((sum(y)+.5)/(length(y)+1),nrow(te)),status='smoothed_prior_insufficient_events'))
 dates<-sort(unique(tr$mes_production_date));cuts<-unique(as.Date(quantile(as.numeric(dates),c(.5,.65,.8)),origin='1970-01-01'))
 losses<-matrix(NA_real_,length(cuts),length(lam))
 for(i in seq_along(cuts)){
  aa<-tr[tr$mes_production_date<cuts[i] & tr$available<cuts[i],];bb<-tr[tr$mes_production_date>=cuts[i],]
  if(nrow(aa)<30||nrow(bb)<10||sd(aa$target)==0)next
  if(family=='binomial' && min(table(aa$target))<3)next
  mm<-design(aa,bb,vs);f<-glmnet(mm$a,aa$target,family=family,alpha=.5,lambda=lam,standardize=FALSE)
  pp<-predict(f,newx=mm$b,s=lam,type='response')
  losses[i,]<-colMeans((pp-bb$target)^2)
 }
 valid<-colSums(is.finite(losses))>0
 if(!any(valid))return(list(p=rep(if(family=='binomial')(sum(y)+.5)/(length(y)+1) else mean(y),nrow(te)),status='prior_no_valid_inner_time_split'))
 score<-colMeans(losses,na.rm=TRUE);best<-which.min(score)
 mm<-design(tr,te,vs);f<-glmnet(mm$a,y,family=family,alpha=.5,lambda=lam,standardize=FALSE)
 list(p=as.numeric(predict(f,newx=mm$b,s=lam[best],type='response')),status=paste0('nested_time_lambda_',signif(lam[best],3)))
}
starts<-as.Date(c('2026-01-01','2026-03-01','2026-05-01','2026-07-01'));ends<-as.Date(c('2026-02-28','2026-04-30','2026-06-30','2026-07-31'))
pred<-list();audit<-list();z<-1;a<-1
for(delay in c(0,14))for(endpoint in c('continuous_mean','binary_mean_gt9','binary_mean_gt10','binary_mean_gt11')){
 d<-x;d$available<-d$d2_observation_date+delay;d$available[d$lag<0]<-as.Date(NA)
 d$target<-if(endpoint=='continuous_mean')d$disintegration_time_min else as.numeric(d$disintegration_time_min>as.numeric(sub('binary_mean_gt','',endpoint)))
 family<-if(endpoint=='continuous_mean')'gaussian' else 'binomial'
 for(s in 1:4){
 tr<-d |> filter(mes_production_date<starts[s],available<starts[s]);te<-d |> filter(mes_production_date>=starts[s],mes_production_date<=ends[s]);recent<-tr |> filter(available>=starts[s]-60)
 stopifnot(all(tr$available<starts[s]),all(tr$mes_production_date<starts[s]))
 prior<-if(family=='gaussian')mean(tr$target) else (sum(tr$target)+.5)/(nrow(tr)+1)
 recentp<-if(nrow(recent)==0)prior else if(family=='gaussian')mean(recent$target) else (sum(recent$target)+.5)/(nrow(recent)+1)
 for(nm in c('historical_mean','recent60_mean',names(sets))){
 rr<-if(nm=='historical_mean')list(p=rep(prior,nrow(te)),status='historical_constant') else if(nm=='recent60_mean')list(p=rep(recentp,nrow(te)),status='recent_constant') else fit(tr,te,sets[[nm]],family,20260907)
 pred[[z]]<-tibble(delay_days=delay,endpoint=endpoint,window=s,id=te$id,month=te$month,model=nm,y=te$target,p=rr$p);z<-z+1
 audit[[a]]<-tibble(delay_days=delay,endpoint=endpoint,window=s,model=nm,train_n=nrow(tr),test_n=nrow(te),train_events=if(family=='binomial')sum(tr$target) else NA_real_,fit_status=rr$status);a<-a+1
 }
 }
}
p<-bind_rows(pred);au<-bind_rows(audit);stopifnot(all(is.finite(p$p)),nrow(au)==192)
auc<-function(y,p){n1<-sum(y==1);n0<-sum(y==0);if(n1*n0==0)return(NA_real_);(sum(rank(p)[y==1])-n1*(n1+1)/2)/(n1*n0)}
met<-p |> group_by(delay_days,endpoint,window,model) |> group_modify(~tibble(n=nrow(.x),MAE=mean(abs(.x$p-.x$y)),MSE=mean((.x$p-.x$y)^2),AUC=if(all(.x$y %in% 0:1))auc(.x$y,.x$p) else NA_real_)) |> ungroup()
pool<-met |> group_by(delay_days,endpoint,model) |> summarise(MAE=weighted.mean(MAE,n),RMSE=sqrt(weighted.mean(MSE,n)),n_total=sum(n),.groups='drop')
write_csv(p,file.path(out,'03_predictions.csv'));write_csv(au,file.path(out,'04_fit_audit.csv'));write_csv(met,file.path(out,'05_window_metrics.csv'));write_csv(pool,file.path(out,'06_pooled_metrics.csv'))
# Conditional paired uncertainty: resample production months for fixed out-of-time predictions; does not refit or account for shared material identities.
delta<-list();j<-1;set.seed(20260907)
for(dd in c(0,14)){
 w<-p |> filter(delay_days==dd,endpoint=='continuous_mean') |> select(id,month,y,model,p) |> pivot_wider(names_from=model,values_from=p)
 for(pair in list(c('MES','historical_mean'),c('MES','recent60_mean'),c('MES_material','MES'),c('MES_material','MES_missingness'))){
  v<-abs(w[[pair[1]]]-w$y)-abs(w[[pair[2]]]-w$y);months<-unique(w$month)
  boots<-replicate(2000,{ms<-sample(months,length(months),replace=TRUE);idx<-unlist(lapply(ms,function(m)which(w$month==m)));mean(v[idx])})
  delta[[j]]<-tibble(delay_days=dd,comparison=paste(pair,collapse=' minus '),delta_MAE=mean(v),low=quantile(boots,.025),high=quantile(boots,.975),month_clusters=length(months));j<-j+1
 }
}
write_csv(bind_rows(delta),file.path(out,'07_paired_month_bootstrap.csv'))
writeLines(c('PASS: 1477 unique batch records; all continuous targets finite.','PASS: 192 model/window/endpoint/delay comparisons; finite predictions.','PASS: fixed 755 test batches per endpoint/delay/model.','Preprocessing learned within each inner chronological training fold; no test-driven feature selection.','Material models are retrospective information comparisons only: upstream actual availability is unverified.','Binary threshold endpoints use batch mean, unlike legacy any-record endpoint; do not mix metrics.','Initial low-event or constant-target models explicitly fall back to prior; see fit audit.','Month-bootstrap intervals conditional on fixed predictions, 7 test-month clusters; not external validation.','Date delay scenarios remain assumptions, not actual result-return dates.'),file.path(out,'QA.txt'))
stopifnot(all(pool$n_total==755));capture.output(sessionInfo(),file=file.path(out,'sessionInfo.txt'))
print(pool |> filter(endpoint=='continuous_mean'));print(bind_rows(delta))
