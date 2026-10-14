# Added retrospective scenarios. Original saved predictions are never changed.
options(warn=1)
suppressPackageStartupMessages({library(readr);library(dplyr);library(glmnet);library(jsonlite);library(digest)})
args<-commandArgs(trailingOnly=TRUE);stopifnot(length(args)==1)
stage<-normalizePath(args[1],winslash='/',mustWork=TRUE)
out<-file.path(stage,'output');stopifnot(!dir.exists(out));dir.create(out)
q<-read_csv(file.path(stage,'qc_pool.csv'),show_col_types=FALSE) |>
 mutate(obs=as.Date(obs),mes_date=as.Date(mes_date))
x<-read_csv(file.path(stage,'joint.csv'),show_col_types=FALSE) |>
 mutate(mes_production_date=as.Date(mes_production_date),d2_observation_date=as.Date(d2_observation_date),
        target=disintegration_time_min,month=format(mes_production_date,'%Y-%m'))
stopifnot(nrow(q)==4296,nrow(x)==1477)
exprs<-parse(file.path(stage,'v4_helpers.R'),encoding='UTF-8')
for(e in exprs)if(is.call(e)&&as.character(e[[1]])%in%c('<-','=')&&as.character(e[[2]])%in%c('mes','lambda','design'))eval(e)
cache<-new.env(parent=emptyenv());sets<-list();members<-list();fits<-list();coefs<-list();preps<-list();innerlog<-list();trainmembers<-list();pred<-list()
history<-function(pool,method,cut,delay){
 cut<-as.Date(cut,origin='1970-01-01');key<-paste(pool,method,cut,delay,sep='|')
 if(exists(key,cache,inherits=FALSE))return(get(key,cache))
 ok<-q$obs+delay<cut & !q$invalid_observation_before_production & (is.na(q$mes_date)|q$mes_date<cut)
 if(pool=='linked')ok<-ok&q$linked
 h<-q[which(ok),];hh<-h
 if(method=='mean60'){rr<-h[h$obs+delay>=cut-60,];if(nrow(rr)>0)hh<-rr;value<-if(nrow(hh)>0)mean(hh$target)else NA_real_}
 else {days<-hh |> mutate(available=obs+delay) |> group_by(available) |> summarise(y=mean(target),.groups='drop') |> arrange(available)
  value<-NA_real_;if(nrow(days)>0){value<-days$y[1];if(nrow(days)>1)for(i in 2:nrow(days))value<-.2*days$y[i]+.8*value}}
 hid<-sprintf('H-%05d',length(sets)+1);ids<-sort(hh$qc_id)
 z<-list(value=value,n=nrow(h),used_n=nrow(hh),id=hid)
 sets[[length(sets)+1]]<<-data.frame(history_set_id=hid,pool=pool,method=method,cut=as.character(cut),delay_days=delay,eligible_n=nrow(h),used_n=nrow(hh),value=value,member_sha256=digest(paste(ids,collapse='\n'),algo='sha256',serialize=FALSE),max_observation=if(nrow(hh))as.character(max(hh$obs))else NA_character_,max_assumed_available=if(nrow(hh))as.character(max(hh$obs+delay))else NA_character_)
 members[[hid]]<<-ids;assign(key,z,cache);z
}
fit<-function(tr,te,base,delay,window,frequency,cut){
 rr<-tr |> filter(prior_n>=30,is.finite(residual));stopifnot(nrow(rr)>=30)
 cs<-unique(as.Date(floor(quantile(as.numeric(sort(unique(tr$mes_production_date))),c(.5,.65,.8))),origin='1970-01-01'))
 loss<-matrix(NA_real_,length(cs),length(lambda));fid<-sprintf('F-%04d',length(fits)+1)
 for(i in seq_along(cs)){
  aa<-tr |> filter(mes_production_date<cs[i],available<cs[i],prior_n>=30,is.finite(residual));bb<-tr |> filter(mes_production_date>=cs[i]);hb<-history('full','mean60',cs[i],delay)
  innerlog[[length(innerlog)+1]]<<-data.frame(fit_id=fid,cut=as.character(cs[i]),train_n=nrow(aa),validation_n=nrow(bb),history_set_id=hb$id,baseline=hb$value)
  if(nrow(aa)<30||nrow(bb)<10||sd(aa$residual)==0||!is.finite(hb$value))next
  mm<-design(aa,bb,mes);f<-glmnet(mm$a,aa$residual,alpha=.5,lambda=lambda,standardize=FALSE)
  loss[i,]<-colMeans((predict(f,newx=mm$b,s=lambda)+hb$value-bb$target)^2)
 }
 status<-'nested_time';chosen<-NA_real_;intercept<-mean(rr$residual)
 if(!any(is.finite(loss))){cf<-data.frame(term='(Intercept)',coefficient=intercept);p<-rep(base+intercept,nrow(te));status<-'intercept_fallback'}
 else {best<-which.min(colMeans(loss,na.rm=TRUE));chosen<-lambda[best];mm<-design(rr,te,mes)
  f<-glmnet(mm$a,rr$residual,alpha=.5,lambda=lambda,standardize=FALSE);cm<-as.matrix(coef(f,s=chosen));cf<-data.frame(term=rownames(cm),coefficient=cm[,1]);p<-base+as.numeric(predict(f,newx=mm$b,s=chosen))
  for(v in mes){a<-rr[[v]];med<-median(a,na.rm=TRUE);if(!is.finite(med))med<-0;a[is.na(a)]<-med;s<-sd(a);if(!is.finite(s)||s==0)s<-1
   preps[[length(preps)+1]]<<-data.frame(fit_id=fid,term=v,median=med,center=mean(a),scale=s)}
 }
 coefs[[length(coefs)+1]]<<-transform(cf,fit_id=fid)
 trainmembers[[fid]]<<-sort(rr$id)
 fits[[length(fits)+1]]<<-data.frame(fit_id=fid,delay_days=delay,window=window,frequency=frequency,cut=as.character(cut),train_n=nrow(tr),residual_n=nrow(rr),test_n=nrow(te),max_production=as.character(max(tr$mes_production_date)),max_assumed_available=as.character(max(tr$available)),lambda=chosen,residual_intercept=intercept,status=status,train_member_sha256=digest(paste(sort(rr$id),collapse='\n'),algo='sha256',serialize=FALSE))
 list(p=p,intercept=rep(base+intercept,nrow(te)),id=fid)
}
starts<-as.Date(c('2026-01-01','2026-03-01','2026-05-01','2026-07-01'));ends<-as.Date(c('2026-02-28','2026-04-30','2026-06-30','2026-07-31'))
for(delay in c(0,14)){
 d<-x;d$available<-d$d2_observation_date+delay;d$available[d$d2_observation_date<d$mes_production_date]<-as.Date(NA)
 h<-lapply(seq_len(nrow(d)),function(i)history('full','mean60',d$mes_production_date[i],delay))
 d$prior_n<-vapply(h,function(z)z$n,integer(1));d$prior_base<-vapply(h,function(z)z$value,numeric(1));d$residual<-d$target-d$prior_base
 write_csv(d |> select(id,mes_production_date,available,prior_n,prior_base,residual),file.path(out,paste0('training_history_',delay,'.csv')))
 for(w in 1:4){
  te0<-d |> filter(mes_production_date>=starts[w],mes_production_date<=ends[w]);stopifnot(nrow(te0)==c(226,230,226,73)[w])
  for(freq in c('frozen','daily')){
   cuts<-if(freq=='frozen')starts[w]else sort(unique(te0$mes_production_date))
   for(ci in seq_along(cuts)){
    cut<-cuts[ci];te<-if(freq=='frozen')te0 else te0 |> filter(mes_production_date==cut)
    for(pool in c('linked','full'))for(method in c('mean60','ewma')){
     hb<-history(pool,method,cut,delay)
     pred[[length(pred)+1]]<-data.frame(delay_days=delay,window=w,id=te$id,date=as.character(te$mes_production_date),month=te$month,frequency=freq,pool=pool,model=method,y=te$target,p=hb$value,cut=as.character(cut),history_set_id=hb$id,fit_id=NA_character_)
    }
    hb<-history('full','mean60',cut,delay);tr<-d |> filter(mes_production_date<cut,available<cut)
    f<-fit(tr,te,hb$value,delay,w,freq,cut)
    for(nm in c('MES_residual','residual_intercept'))pred[[length(pred)+1]]<-data.frame(delay_days=delay,window=w,id=te$id,date=as.character(te$mes_production_date),month=te$month,frequency=freq,pool='full',model=nm,y=te$target,p=if(nm=='MES_residual')f$p else f$intercept,cut=as.character(cut),history_set_id=hb$id,fit_id=f$id)
   }
  }
  cat('Completed delay',delay,'window',w,'\n')
 }
}
pp<-bind_rows(pred);stopifnot(nrow(pp)==18120,all(is.finite(pp$p)))
old<-read_csv(file.path(stage,'legacy_predictions.csv'),show_col_types=FALSE) |> filter(model=='recent_mean')
repro<-pp |> filter(pool=='linked',model=='mean60') |> inner_join(old,by=c('delay_days','window','id','frequency'),suffix=c('','.old'))
stopifnot(nrow(repro)==3020,max(abs(repro$p-repro$p.old))<1e-10)
write_csv(pp,file.path(out,'01_predictions.csv'));write_csv(bind_rows(sets),file.path(out,'02_history_sets.csv'))
write_json(members,file.path(out,'03_history_members.json'),auto_unbox=FALSE)
write_csv(bind_rows(fits),file.path(out,'04_fits.csv'));write_csv(bind_rows(coefs),file.path(out,'05_coefficients.csv'))
write_csv(bind_rows(preps),file.path(out,'06_preprocessing.csv'));write_csv(bind_rows(innerlog),file.path(out,'07_inner_splits.csv'))
write_json(trainmembers,file.path(out,'08_training_members.json'),auto_unbox=FALSE)
write_json(list(new_predictions=nrow(pp),legacy_mean_reproduced=nrow(repro),max_difference=max(abs(repro$p-repro$p.old)),inner_cut_rule='calendar_day_floor',created_utc=format(Sys.time(),tz='UTC')),file.path(out,'run.json'),pretty=TRUE,auto_unbox=TRUE)
capture.output(sessionInfo(),file=file.path(out,'sessionInfo.txt'));cat('Saved',nrow(pp),'predictions\n')
