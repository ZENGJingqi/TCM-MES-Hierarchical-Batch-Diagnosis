# Exploratory frozen-window residual models. Original analyses and locked data are read-only.
options(warn=1)
suppressPackageStartupMessages({library(dplyr);library(readr);library(glmnet);library(tidyr)})
args<-commandArgs(trailingOnly=TRUE);stopifnot(length(args)==1)
stage<-normalizePath(args[1],winslash='/',mustWork=TRUE)
out<-file.path(stage,'output');dir.create(out,recursive=TRUE,showWarnings=FALSE)
x<-read_csv(file.path(stage,'joint.csv'),show_col_types=FALSE) |>
 mutate(mes_production_date=as.Date(mes_production_date),d2_observation_date=as.Date(d2_observation_date)) |>
 arrange(mes_production_date,finished_batch) |>
 mutate(id=sprintf('Q-%04d',row_number()),month=format(mes_production_date,'%Y-%m'),lag=as.integer(d2_observation_date-mes_production_date),target=disintegration_time_min)
stopifnot(nrow(x)==1477,!anyDuplicated(x$finished_batch),all(is.finite(x$target)))
mes<-c('coating_yield_pct','final_blend_moisture_pct','coating_mass_balance_pct','compression_yield_pct','compression_hardness_mean_n','coated_tablet_weight_mean_g','final_blend_lt_100_mesh_pct','granulation_discharge_moisture_pct_mean','core_tablet_weight_mean_g')
up<-c('extract_total_ash_pct','extract_extract_pct','chenpi_hesperidin_pct_mean','chenpi_moisture_pct_mean','yam_rejected_material_rate_pct','yam_through_120_mesh_mean_pct')
lambda<-10^seq(1,-4,length.out=35)
design<-function(tr,te,vs){
 a<-as.matrix(tr[vs]);b<-as.matrix(te[vs]);storage.mode(a)<-'double';storage.mode(b)<-'double'
 ia<-is.na(a)*1;ib<-is.na(b)*1
 for(j in seq_along(vs)){med<-median(a[,j],na.rm=TRUE);if(!is.finite(med))med<-0;a[is.na(a[,j]),j]<-med;b[is.na(b[,j]),j]<-med;mu<-mean(a[,j]);ss<-sd(a[,j]);if(!is.finite(ss)||ss==0)ss<-1;a[,j]<-(a[,j]-mu)/ss;b[,j]<-(b[,j]-mu)/ss}
 a<-cbind(a,ia);b<-cbind(b,ib);colnames(a)<-colnames(b)<-c(vs,paste0(vs,'__missing'))
 list(a=a,b=b)
}
recent<-function(dat,cut){
 h<-dat |> filter(mes_production_date<cut,available<cut)
 rr<-h |> filter(available>=cut-60)
 list(value=if(nrow(rr)>0)mean(rr$target) else if(nrow(h)>0)mean(h$target) else NA_real_,n=nrow(h),recent_n=nrow(rr))
}
cuts<-function(dat)unique(as.Date(quantile(as.numeric(sort(unique(dat$mes_production_date))),c(.5,.65,.8)),origin='1970-01-01'))
fit_original<-function(tr,te,vs){
 cs<-cuts(tr);loss<-matrix(NA_real_,length(cs),length(lambda))
 for(i in seq_along(cs)){
  aa<-tr |> filter(mes_production_date<cs[i],available<cs[i]);bb<-tr |> filter(mes_production_date>=cs[i])
  if(nrow(aa)<30||nrow(bb)<10||sd(aa$target)==0)next
  mm<-design(aa,bb,vs);f<-glmnet(mm$a,aa$target,alpha=.5,lambda=lambda,standardize=FALSE)
  loss[i,]<-colMeans((predict(f,newx=mm$b,s=lambda)-bb$target)^2)
 }
 if(!any(is.finite(loss)))return(list(p=rep(mean(tr$target),nrow(te)),lambda=NA_real_,coef=data.frame(term='(Intercept)',coefficient=mean(tr$target)),status='prior_no_valid_inner_time_split'))
 best<-which.min(colMeans(loss,na.rm=TRUE));mm<-design(tr,te,vs)
 f<-glmnet(mm$a,tr$target,alpha=.5,lambda=lambda,standardize=FALSE)
 cf<-as.matrix(coef(f,s=lambda[best]));list(p=as.numeric(predict(f,newx=mm$b,s=lambda[best])),lambda=lambda[best],coef=data.frame(term=rownames(cf),coefficient=cf[,1]),status='nested_time')
}
fit_residual<-function(tr,te,vs,frozen_base){
 cs<-cuts(tr);loss<-matrix(NA_real_,length(cs),length(lambda));inner<-list()
 for(i in seq_along(cs)){
  aa<-tr |> filter(mes_production_date<cs[i],available<cs[i],prior_n>=30,is.finite(residual));bb<-tr |> filter(mes_production_date>=cs[i])
  r<-recent(tr,cs[i]);inner[[i]]<-data.frame(cut=as.character(cs[i]),train_n=nrow(aa),validation_n=nrow(bb),baseline=r$value)
  if(nrow(aa)<30||nrow(bb)<10||sd(aa$residual)==0||!is.finite(r$value))next
  mm<-design(aa,bb,vs);f<-glmnet(mm$a,aa$residual,alpha=.5,lambda=lambda,standardize=FALSE)
  # Inner validation uses a baseline frozen at its own cut, matching the outer information frequency.
  loss[i,]<-colMeans((predict(f,newx=mm$b,s=lambda)+r$value-bb$target)^2)
 }
 rr<-tr |> filter(prior_n>=30,is.finite(residual));stopifnot(nrow(rr)>=30)
 if(!any(is.finite(loss)))return(list(p=rep(frozen_base+mean(rr$residual),nrow(te)),lambda=NA_real_,coef=data.frame(term='(Intercept)',coefficient=mean(rr$residual)),status='residual_intercept_no_valid_inner_split',train_n=nrow(rr),inner=bind_rows(inner)))
 best<-which.min(colMeans(loss,na.rm=TRUE));mm<-design(rr,te,vs)
 f<-glmnet(mm$a,rr$residual,alpha=.5,lambda=lambda,standardize=FALSE);cf<-as.matrix(coef(f,s=lambda[best]))
 list(p=frozen_base+as.numeric(predict(f,newx=mm$b,s=lambda[best])),lambda=lambda[best],coef=data.frame(term=rownames(cf),coefficient=cf[,1]),status='nested_time_residual',train_n=nrow(rr),inner=bind_rows(inner))
}
starts<-as.Date(c('2026-01-01','2026-03-01','2026-05-01','2026-07-01'));ends<-as.Date(c('2026-02-28','2026-04-30','2026-06-30','2026-07-31'))
pred<-list();audit<-list();coeff<-list();inners<-list();history<-list();repro<-list()
old<-read_csv(file.path(stage,'old_predictions.csv'),show_col_types=FALSE) |> filter(endpoint=='continuous_mean')
for(delay in c(0,14)){
 d<-x;d$available<-d$d2_observation_date+delay;d$available[d$lag<0]<-as.Date(NA)
 r<-lapply(seq_len(nrow(d)),function(i)recent(d,d$mes_production_date[i]))
 d$prior_baseline<-vapply(r,function(z)z$value,numeric(1));d$prior_n<-vapply(r,function(z)z$n,integer(1));d$prior_recent_n<-vapply(r,function(z)z$recent_n,integer(1));d$residual<-d$target-d$prior_baseline
 history[[length(history)+1]]<-d |> transmute(delay_days=delay,id,mes_production_date,available,prior_baseline,prior_n,prior_recent_n,residual,eligible=prior_n>=30 & is.finite(residual))
 for(w in 1:4){
  tr<-d |> filter(mes_production_date<starts[w],available<starts[w]);te<-d |> filter(mes_production_date>=starts[w],mes_production_date<=ends[w]);rb<-recent(tr,starts[w])
  stopifnot(all(tr$available<starts[w]),all(tr$mes_production_date<starts[w]),nrow(te)==c(226,230,226,73)[w])
  rr<-tr |> filter(prior_n>=30,is.finite(residual))
  for(nm in c('MES','MES_material')){
   f<-fit_original(tr,te,if(nm=='MES')mes else c(mes,up));ref<-old |> filter(delay_days==delay,window==w,model==nm);ref<-ref[match(te$id,ref$id),]
   stopifnot(!anyNA(ref$id),max(abs(ref$p-f$p))<1e-8)
   repro[[length(repro)+1]]<-data.frame(delay_days=delay,window=w,model=nm,n=nrow(te),max_prediction_difference=max(abs(ref$p-f$p)),lambda=f$lambda,nonzero_terms=sum(abs(f$coef$coefficient[f$coef$term!='(Intercept)'])>1e-10))
   coeff[[length(coeff)+1]]<-f$coef |> mutate(delay_days=delay,window=w,model=nm,lambda=f$lambda)
  }
  for(nm in c('recent_residual_intercept','recent_residual_MES','recent_residual_MES_material')){
   f<-if(nm=='recent_residual_intercept')list(p=rep(rb$value+mean(rr$residual),nrow(te)),lambda=NA_real_,coef=data.frame(term='(Intercept)',coefficient=mean(rr$residual)),status='residual_intercept_control',train_n=nrow(rr)) else fit_residual(tr,te,if(nm=='recent_residual_MES')mes else c(mes,up),rb$value)
   pred[[length(pred)+1]]<-data.frame(delay_days=delay,endpoint='continuous_mean',window=w,id=te$id,month=te$month,model=nm,y=te$target,p=f$p)
   audit[[length(audit)+1]]<-data.frame(delay_days=delay,window=w,model=nm,cut=as.character(starts[w]),available_train_n=nrow(tr),residual_train_n=f$train_n,test_n=nrow(te),frozen_recent_mean=rb$value,residual_training_mean=mean(rr$residual),lambda=f$lambda,nonzero_terms=sum(abs(f$coef$coefficient[f$coef$term!='(Intercept)'])>1e-10),fit_status=f$status,max_train_production=as.character(max(tr$mes_production_date)),max_train_available=as.character(max(tr$available)))
   coeff[[length(coeff)+1]]<-f$coef |> mutate(delay_days=delay,window=w,model=nm,lambda=f$lambda)
   if(!is.null(f$inner))inners[[length(inners)+1]]<-f$inner |> mutate(delay_days=delay,window=w,model=nm)
  }
 }
}
pp<-bind_rows(pred);stopifnot(nrow(pp)==4530,all(is.finite(pp$p)))
write_csv(pp,file.path(out,'01_residual_predictions.csv'))
write_csv(bind_rows(audit),file.path(out,'02_residual_fit_audit.csv'))
write_csv(bind_rows(coeff),file.path(out,'03_coefficients.csv'))
write_csv(bind_rows(inners),file.path(out,'04_inner_split_audit.csv'))
write_csv(bind_rows(history),file.path(out,'05_training_history_baselines.csv'))
write_csv(bind_rows(repro),file.path(out,'06_original_model_reproduction.csv'))
capture.output(sessionInfo(),file=file.path(out,'sessionInfo.txt'))
print(bind_rows(audit) |> select(delay_days,window,model,residual_train_n,lambda,nonzero_terms));print(bind_rows(repro))
