# Identical past-only order-statistic rule; parse date strings once, not in each row.
calibrate<-function(pp,hh,out){
 pd<-as.Date(hh$date);av<-as.Date(hh$available)
 rows<-which(pp$frequency=='daily' & pp$model!='residual_intercept');intervals<-vector('list',length(rows))
 for(ii in seq_along(rows)){
  z<-pp[rows[ii],];cut<-as.Date(z$cut)
  prior<-which(hh$delay_days==z$delay_days & hh$model==z$model & !is.na(av) & pd<cut & av<cut)
  rr<-prior[av[prior]>=cut-60];method<-'recent60'
  if(length(rr)<30){rr<-prior;method<-'all_past'}
  n<-length(rr);index<-ceiling(.9*(n+1));q<-if(n>=30&&index<=n)sort(abs(hh$p[rr]-hh$y[rr]))[index] else NA_real_
  z$calibration_n<-n;z$calibration_rule<-method;z$order_index<-index;z$half_width<-q;z$lower<-z$p-q;z$upper<-z$p+q
  z$calibration_max_production<-if(n>0)max(hh$date[rr]) else NA_character_
  z$calibration_max_available<-if(n>0)max(hh$available[rr]) else NA_character_
  intervals[[ii]]<-z
 }
 readr::write_csv(dplyr::bind_rows(intervals),file.path(out,'08_prediction_intervals.csv'))
}
if(sys.nframe()==0){
 args<-commandArgs(trailingOnly=TRUE);out<-file.path(args[1],'output')
 pp<-readr::read_csv(file.path(out,'01_predictions.csv'),show_col_types=FALSE)
 hh<-readr::read_csv(file.path(out,'02_historical_daily_predictions.csv'),show_col_types=FALSE)
 calibrate(pp,hh,out);capture.output(sessionInfo(),file=file.path(out,'sessionInfo.txt'))
}
