# Independent R recomputation, without importing the Python baseline helper.
args<-commandArgs(trailingOnly=TRUE);stopifnot(length(args)==1)
stage<-normalizePath(args[1],winslash='/',mustWork=TRUE)
x<-read.csv(file.path(stage,'joint.csv'),fileEncoding='UTF-8-BOM',colClasses=c(finished_batch='character'))
x$prod<-as.Date(x$mes_production_date);x$obs<-as.Date(x$d2_observation_date)
x<-x[order(x$prod,x$finished_batch),];x$id<-sprintf('Q-%04d',seq_len(nrow(x)))
starts<-as.Date(c('2026-01-01','2026-03-01','2026-05-01','2026-07-01'))
ends<-as.Date(c('2026-02-28','2026-04-30','2026-06-30','2026-07-31'))
x$window<-0L
for(w in 1:4)x$window[x$prod>=starts[w]&x$prod<=ends[w]]<-w
te<-x[x$window>0,];stopifnot(nrow(x)==1477,nrow(te)==755)
rows<-list()
for(delay in c(0,14)){
 avail<-x$obs+delay;avail[x$obs<x$prod]<-as.Date(NA)
 for(L in c(30,60,90))for(freq in c('frozen','daily'))for(i in seq_len(nrow(te))){
  cut<-if(freq=='frozen')starts[te$window[i]] else te$prod[i]
  hist<-which(!is.na(avail)&avail<cut&x$prod<cut)
  recent<-hist[avail[hist]>=cut-L]
  chosen<-if(length(recent)>0)recent else hist
  stopifnot(length(chosen)>0,all(avail[chosen]<cut),all(x$prod[chosen]<cut))
  rows[[length(rows)+1]]<-data.frame(delay_days=delay,lookback_days=L,frequency=freq,
    window=te$window[i],id=te$id[i],cut=as.character(cut),y=te$disintegration_time_min[i],
    p=mean(x$disintegration_time_min[chosen]),history_n=length(hist),recent_n=length(recent),
    fallback=length(recent)==0)
 }
}
p<-do.call(rbind,rows);stopifnot(nrow(p)==9060,all(is.finite(p$p)))
write.csv(p,file.path(stage,'R_independent_predictions.csv'),row.names=FALSE)
capture.output(sessionInfo(),file=file.path(stage,'R_sessionInfo.txt'))
message('Independent R: all 9060 predictions computed.')
