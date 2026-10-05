suppressPackageStartupMessages({library(readr);library(dplyr);library(tidyr)})
s<-Sys.getenv('TCM_MATCH_STAGE');out<-file.path(s,'output');dir.create(out,showWarnings=FALSE)
x<-read_csv(file.path(s,'joint.csv'),show_col_types=FALSE) |> mutate(prod=as.Date(mes_production_date),obs=as.Date(d2_observation_date)) |> arrange(prod,finished_batch) |> mutate(id=sprintf('S-%04d',row_number()),y=disintegration_issue)
old<-read_csv(file.path(s,'predictions.csv'),show_col_types=FALSE)
starts<-as.Date(c('2026-01-01','2026-03-01','2026-05-01','2026-07-01'));ends<-as.Date(c('2026-02-28','2026-04-30','2026-06-30','2026-07-31'))
rows<-list();aud<-list();z<-1;h<-1
for(dd in c(0,14)){
 av<-x$obs+dd;av[x$obs<x$prod]<-as.Date(NA)
 for(w in 1:4)for(i in which(x$prod>=starts[w]&x$prod<=ends[w])){
  for(clock in c('frozen','daily')){
   t<-if(clock=='frozen')starts[w] else x$prod[i]
   ii<-which(x$prod<t & !is.na(av) & av<t);rr<-ii[av[ii]>=t-60];use<-if(length(rr))rr else ii
   stopifnot(length(ii)>0,all(av[ii]<t),all(x$prod[ii]<t))
   p_all<-(sum(x$y[ii])+.5)/(length(ii)+1);p_recent<-(sum(x$y[use])+.5)/(length(use)+1)
   pp<-c(history=p_all,recent60=p_recent);if(clock=='daily')pp<-c(pp,recent60_clipped=min(max(p_recent,.005),.995))
   for(nm in names(pp)){rows[[z]]<-tibble(delay=dd,window=w,id=x$id[i],model=paste(clock,nm,sep='_'),y=x$y[i],p=unname(pp[nm]));z<-z+1}
   aud[[h]]<-tibble(delay=dd,window=w,id=x$id[i],clock=clock,cutoff=t,history_n=length(ii),recent_n=length(rr),history_events=sum(x$y[ii]));h<-h+1
  }
 }
}
b<-bind_rows(rows);a<-bind_rows(aud);p<-bind_rows(old,b)
stopifnot(!anyDuplicated(p[c('delay','model','id')]),all(is.finite(p$p)))
eq<-inner_join(filter(p,model=='state_prior'),filter(p,model=='daily_recent60_clipped'),by=c('delay','window','id'),suffix=c('_state','_base'))
stopifnot(nrow(eq)==1510,all(eq$y_state==eq$y_base))
equiv<-eq |> group_by(delay) |> summarise(n=n(),max_abs_difference=max(abs(p_state-p_base)),.groups='drop')
m<-p |> group_by(delay,window,model) |> group_modify(~{y<-.x$y;pr<-.x$p;n<-length(y);k<-ceiling(.2*n);cut<-sort(pr,decreasing=TRUE)[k];ab<-pr>cut;tie<-pr==cut;hit<-sum(y[ab])+(k-sum(ab))/sum(tie)*sum(y[tie]);tibble(n=n,events=sum(y),Brier=mean((pr-y)^2),capture20=hit/sum(y),random_capture=k/n)}) |> ungroup()
pool<-m |> group_by(delay,model) |> summarise(Brier=weighted.mean(Brier,n),capture20=weighted.mean(capture20,events),n=sum(n),.groups='drop')
stopifnot(all(pool$n==755))
pairs<-list(c('retraining','frozen_recent60'),c('recalibration','frozen_history'),c('state_interaction','daily_recent60'),c('state_prior','daily_recent60_clipped'))
diff<-bind_rows(lapply(c(0,14),function(dd)bind_rows(lapply(pairs,function(pair){v<-inner_join(filter(p,delay==dd,model==pair[1]),filter(p,delay==dd,model==pair[2]),by=c('delay','window','id'),suffix=c('_a','_b'));stopifnot(nrow(v)==755,all(v$y_a==v$y_b));tibble(delay=dd,comparison=paste(pair,collapse=' minus '),n=nrow(v),delta_Brier=mean((v$p_a-v$y_a)^2-(v$p_b-v$y_b)^2))}))))
write_csv(p,file.path(out,'predictions_with_matched_baselines.csv'));write_csv(a,file.path(out,'information_cutoff_audit.csv'));write_csv(m,file.path(out,'window_metrics.csv'));write_csv(pool,file.path(out,'pooled_metrics.csv'));write_csv(diff,file.path(out,'matched_comparisons.csv'));write_csv(equiv,file.path(out,'state_prior_equivalence.csv'))
stopifnot(all(equiv$max_abs_difference<1e-12))
writeLines(c('PASS: same 755 test batches and outcomes in each of 8 paired comparisons.','PASS: all baseline history production and hypothetical availability dates strictly precede cutoff.','PASS: state-prior predictions equal clipped daily recent-risk baseline within 1e-12.','No refits; information frequency matched, not identical feature sets.','All timing assumptions and full-window retrospective budget limitations remain.'),file.path(out,'QA.txt'))
capture.output(sessionInfo(),file=file.path(out,'sessionInfo.txt'));print(pool);print(diff);print(equiv)
