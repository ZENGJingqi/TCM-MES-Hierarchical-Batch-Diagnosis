# Four prespecified exploratory experiments; no changes to prior outputs or locked inputs.
options(warn=1)
suppressPackageStartupMessages({library(dplyr);library(readr);library(glmnet);library(rpart)})
args<-commandArgs(trailingOnly=TRUE);stopifnot(length(args)==1)
stage<-normalizePath(args[1],winslash='/',mustWork=TRUE)
out<-file.path(stage,'output');dir.create(out,showWarnings=FALSE)
# Import only pure helpers, not the prior analysis's execution or writes.
helpers<-c('mes','lambda','design','recent','cuts','fit_original','fit_residual')
for(e in parse(file.path(stage,'v4_helpers.R'))){
 if(is.call(e)&&as.character(e[[1]])%in%c('<-','=')&&is.symbol(e[[2]])&&as.character(e[[2]])%in%helpers)eval(e)
}
stopifnot(all(vapply(helpers,exists,logical(1))))
x<-read_csv(file.path(stage,'joint.csv'),show_col_types=FALSE)|>
 mutate(mes_production_date=as.Date(mes_production_date),d2_observation_date=as.Date(d2_observation_date))|>
 arrange(mes_production_date,finished_batch)|>
 mutate(id=sprintf('Q-%04d',row_number()),month=format(mes_production_date,'%Y-%m'),
        lag=as.integer(d2_observation_date-mes_production_date),target=disintegration_time_min)
stopifnot(nrow(x)==1477,!anyDuplicated(x$id),all(is.finite(x$target)))
starts<-as.Date(c('2026-01-01','2026-03-01','2026-05-01','2026-07-01'))
ends<-as.Date(c('2026-02-28','2026-04-30','2026-06-30','2026-07-31'))
x$window<-NA_integer_
for(w in 1:4)x$window[x$mes_production_date>=starts[w]&x$mes_production_date<=ends[w]]<-w
old<-read_csv(file.path(stage,'old_predictions.csv'),show_col_types=FALSE)|>filter(endpoint=='continuous_mean')
resold<-read_csv(file.path(stage,'residual_predictions.csv'),show_col_types=FALSE)
grid<-expand.grid(cp=c(.001,.01,.05),depth=c(2L,3L))
tree_fit<-function(tr,te,base){
 rr<-tr|>filter(prior_n>=30,is.finite(residual));cs<-cuts(tr)
 loss<-matrix(NA_real_,length(cs),nrow(grid))
 one<-function(a,b,k){
  mm<-design(a,b,mes);aa<-as.data.frame(mm$a);aa$response<-a$residual
  f<-rpart(response~.,data=aa,method='anova',control=rpart.control(cp=grid$cp[k],maxdepth=grid$depth[k],minbucket=20,minsplit=40,xval=0,maxsurrogate=0))
  list(p=as.numeric(predict(f,as.data.frame(mm$b))),leaves=sum(f$frame$var=='<leaf>'))
 }
 for(i in seq_along(cs)){
  a<-tr|>filter(mes_production_date<cs[i],available<cs[i],prior_n>=30,is.finite(residual))
  b<-tr|>filter(mes_production_date>=cs[i]);rb<-recent(tr,cs[i])
  if(nrow(a)<30||nrow(b)<10||sd(a$residual)==0||!is.finite(rb$value))next
  for(k in seq_len(nrow(grid)))loss[i,k]<-mean((one(a,b,k)$p+rb$value-b$target)^2)
 }
 if(!any(is.finite(loss)))return(list(p=rep(base+mean(rr$residual),nrow(te)),cp=NA_real_,depth=NA_integer_,leaves=1L,status='residual_intercept_no_valid_inner_split'))
 k<-which.min(colMeans(loss,na.rm=TRUE));f<-one(rr,te,k)
 list(p=base+f$p,cp=grid$cp[k],depth=grid$depth[k],leaves=f$leaves,status='nested_time_tree')
}
score_range<-function(rr,te){
 mm<-design(rr,te,mes);a<-sqrt(rowMeans(mm$a[,seq_along(mes),drop=FALSE]^2));b<-sqrt(rowMeans(mm$b[,seq_along(mes),drop=FALSE]^2))
 q<-as.numeric(quantile(a,.95,type=7));list(score=b,threshold=q,flag=b>q)
}
predict_at<-function(d,cut,te,delay,frequency,w){
 tr<-d|>filter(mes_production_date<cut,available<cut)
 rr<-tr|>filter(prior_n>=30,is.finite(residual));rb<-recent(d,cut)
 if(nrow(tr)<30||nrow(rr)<30)return(NULL)
 stopifnot(all(tr$mes_production_date<cut),all(tr$available<cut))
 f<-fit_residual(tr,te,mes,rb$value);t<-tree_fit(tr,te,rb$value);rg<-score_range(rr,te)
 pp<-bind_rows(lapply(c('recent_mean','residual_intercept','MES_residual','tree_residual'),function(nm){
  val<-switch(nm,recent_mean=rep(rb$value,nrow(te)),residual_intercept=rep(rb$value+mean(rr$residual),nrow(te)),MES_residual=f$p,tree_residual=t$p)
  data.frame(delay_days=delay,window=te$window,id=te$id,date=as.character(te$mes_production_date),month=te$month,
             available=as.character(te$available),frequency=frequency,model=nm,y=te$target,p=val,cut=as.character(cut),
             range_score=rg$score,range_threshold=rg$threshold,range_flag=rg$flag)
 }))
 audit<-data.frame(delay_days=delay,window=w,frequency=frequency,cut=as.character(cut),train_n=nrow(tr),residual_n=nrow(rr),test_n=nrow(te),
      recent_n=rb$recent_n,baseline=rb$value,residual_mean=mean(rr$residual),max_production=as.character(max(tr$mes_production_date)),
      max_available=as.character(max(tr$available)),lambda=f$lambda,nonzero=sum(abs(f$coef$coefficient[f$coef$term!='(Intercept)'])>1e-10),
      cp=t$cp,depth=t$depth,leaves=t$leaves,MES_status=f$status,tree_status=t$status,range_threshold=rg$threshold)
 list(p=pp,audit=audit)
}
misalign<-function(dat,train=FALSE){
 block<-floor(as.numeric(dat$mes_production_date-as.Date('2025-04-01'))/7)
 mask<-apply(is.na(as.matrix(dat[mes])),1,paste0,collapse='')
 stratum<-paste(block,mask,sep=':')
 if(train){
  stratum<-paste(stratum,dat$prior_n>=30 & is.finite(dat$residual),sep=':')
  for(cut in cuts(dat))stratum<-paste(stratum,dat$mes_production_date<cut & dat$available<cut,sep=':')
 }
 donor<-seq_len(nrow(dat));sizes<-integer(nrow(dat))
 for(g in split(seq_len(nrow(dat)),stratum)){
  sizes[g]<-length(g)
  if(length(g)>1){s<-g[sample.int(length(g))];donor[s]<-c(s[-1],s[1])}
 }
 altered<-dat;altered[mes]<-dat[donor,mes]
 stopifnot(identical(is.na(as.matrix(dat[mes])),is.na(as.matrix(altered[mes]))),all(stratum==stratum[donor]),all(abs(dat$mes_production_date-dat$mes_production_date[donor])<=6))
 changed<-rowSums(abs(as.matrix(dat[mes])-as.matrix(altered[mes]))>1e-12,na.rm=TRUE)>0
 list(dat=altered,map=data.frame(id=dat$id,donor=dat$id[donor],stratum=stratum,layer_n=sizes,
              date=as.character(dat$mes_production_date),donor_date=as.character(dat$mes_production_date[donor]),
              index_moved=donor!=seq_len(nrow(dat)),values_changed=changed),moved=sum(donor!=seq_len(nrow(dat))),changed=sum(changed))
}
pred<-list();fits<-list();hist<-list();repro<-list();neg<-list();maps<-list();neg_aud<-list()
set.seed(20261001)
for(delay in c(0,14)){
 d<-x;d$available<-d$d2_observation_date+delay;d$available[d$lag<0]<-as.Date(NA)
 r<-lapply(seq_len(nrow(d)),function(i)recent(d,d$mes_production_date[i]))
 d$prior_baseline<-vapply(r,function(z)z$value,numeric(1));d$prior_n<-vapply(r,function(z)z$n,integer(1))
 d$residual<-d$target-d$prior_baseline
 # Daily historical predictions are generated before interval calibration, without any test-outcome tuning.
 for(k in sort(unique(as.numeric(d$mes_production_date)))){
  cut<-as.Date(k,origin='1970-01-01');te<-d|>filter(mes_production_date==cut)
  f<-predict_at(d,cut,te,delay,'daily',if(all(is.na(te$window)))NA_integer_ else te$window[1])
  if(is.null(f))next
  hist[[length(hist)+1]]<-f$p;fits[[length(fits)+1]]<-f$audit
  if(any(!is.na(te$window)))pred[[length(pred)+1]]<-f$p
 }
 message('Daily replay done: delay ',delay)
 for(w in 1:4){
  tr<-d|>filter(mes_production_date<starts[w],available<starts[w]);te<-d|>filter(window==w)
  f<-predict_at(d,starts[w],te,delay,'frozen',w);stopifnot(!is.null(f),nrow(te)==c(226,230,226,73)[w])
  pred[[length(pred)+1]]<-f$p;fits[[length(fits)+1]]<-f$audit
  original<-fit_original(tr,te,mes)
  for(nm in c('original_MES','MES_residual','residual_intercept','recent_mean')){
   ref<-if(nm=='original_MES')old|>filter(delay_days==delay,window==w,model=='MES') else if(nm=='recent_mean')old|>filter(delay_days==delay,window==w,model=='recent60_mean') else resold|>filter(delay_days==delay,window==w,model==if(nm=='MES_residual')'recent_residual_MES' else 'recent_residual_intercept')
   ref<-ref[match(te$id,ref$id),];pv<-if(nm=='original_MES')original$p else f$p$p[f$p$model==nm]
   stopifnot(!anyNA(ref$id),max(abs(ref$p-pv))<1e-8)
   repro[[length(repro)+1]]<-data.frame(delay_days=delay,window=w,model=nm,max_difference=max(abs(ref$p-pv)))
  }
  rb<-recent(tr,starts[w]);realres<-f$p$p[f$p$model=='MES_residual']
  neg[[length(neg)+1]]<-bind_rows(data.frame(delay_days=delay,window=w,repeat_id=0,model='original_MES',n=nrow(te),MAE=mean(abs(original$p-te$target))),
                                data.frame(delay_days=delay,window=w,repeat_id=0,model='MES_residual',n=nrow(te),MAE=mean(abs(realres-te$target))))
  for(rep in 1:100){
   a<-misalign(tr,TRUE);b<-misalign(te,FALSE)
   if(rep==1){maps[[length(maps)+1]]<-a$map|>mutate(delay_days=delay,window=w,role='train');maps[[length(maps)+1]]<-b$map|>mutate(delay_days=delay,window=w,role='test')}
   ff<-fit_original(a$dat,b$dat,mes);fr<-fit_residual(a$dat,b$dat,mes,rb$value)
   neg[[length(neg)+1]]<-bind_rows(data.frame(delay_days=delay,window=w,repeat_id=rep,model='original_MES',n=nrow(te),MAE=mean(abs(ff$p-te$target))),
                                 data.frame(delay_days=delay,window=w,repeat_id=rep,model='MES_residual',n=nrow(te),MAE=mean(abs(fr$p-te$target))))
   neg_aud[[length(neg_aud)+1]]<-data.frame(delay_days=delay,window=w,repeat_id=rep,train_n=nrow(tr),train_moved=a$moved,train_changed=a$changed,test_n=nrow(te),test_moved=b$moved,test_changed=b$changed)
  }
  message('Negative controls done: delay ',delay,', window ',w)
 }
}
pp<-bind_rows(pred);hh<-bind_rows(hist);fa<-bind_rows(fits)
stopifnot(nrow(pp)==12080,all(is.finite(pp$p)))
write_csv(pp,file.path(out,'01_predictions.csv'));write_csv(hh,file.path(out,'02_historical_daily_predictions.csv'))
write_csv(fa,file.path(out,'03_fit_audit.csv'));write_csv(bind_rows(repro),file.path(out,'04_V4_reproduction.csv'))
write_csv(bind_rows(neg),file.path(out,'05_negative_controls.csv'));write_csv(bind_rows(neg_aud),file.path(out,'06_negative_control_audit.csv'))
write_csv(bind_rows(maps),file.path(out,'07_negative_control_first_repeat_mapping.csv'))
source(file.path(stage,'calibrate.R'))
calibrate(pp,hh,out)
capture.output(sessionInfo(),file=file.path(out,'sessionInfo.txt'))
message('All four experiments complete: ',nrow(pp),' test predictions.')
