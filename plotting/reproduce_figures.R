# Assemble the 13 statistical figures from authorized, frozen panel objects.
# This is an I/O adaptation of the local October 5 reproduction script.
# It does not fit models, change panel data/style, or generate Figure 1.
if (.Platform$OS.type=='windows') invisible(suppressWarnings(try(
  Sys.setlocale('LC_CTYPE','Chinese_China.utf8'),silent=TRUE)))
args <- commandArgs(trailingOnly=TRUE)
usage <- paste(
  'Usage: Rscript plotting/reproduce_figures.R <panel_root> <NEW_output_dir> [dpi]',
  '       Rscript plotting/reproduce_figures.R --check-inputs <panel_root>',
  '       Rscript plotting/reproduce_figures.R --help', sep='\n')
if (identical(args, '--help')) { cat(usage, '\n'); quit(status=0) }
check_only <- length(args)==2 && args[1]=='--check-inputs'
if (!check_only && !(length(args) %in% c(2,3))) stop(usage)
src <- normalizePath(args[if(check_only) 2 else 1], winslash='/', mustWork=TRUE)
panels <- list(Figure_2=c('A','B','C'), Figure_3=c('A','B','C'),
  Figure_4=c('A','B','C','D'), Figure_5=c('A','B','C','D'), Figure_6=c('A','B'),
  Figure_S1=c('A','B','C','D'), Figure_S2=c('A','B'), Figure_S3='',
  Figure_S4=c('A','B','C'), Figure_S5=c('A','B','C','D'),
  Figure_S6=c('A','B'), Figure_S7=c('A','B'), Figure_S8=c('A','B'))
panel_file <- function(fid,id) file.path(src,fid,paste0(fid,if(nzchar(id))paste0('_',id)else '', '.rds'))
required <- unlist(lapply(names(panels),function(fid) vapply(panels[[fid]],function(id)panel_file(fid,id),character(1))),use.names=FALSE)
if (any(!file.exists(required))) stop('Missing authorized panel objects; no output was created.')
if (check_only) { cat('All 36 required panel files exist; no objects loaded or outputs created.\n'); quit(status=0) }
dpi <- if(length(args)==3) suppressWarnings(as.numeric(args[3])) else 300
if (length(dpi)!=1 || !is.finite(dpi) || dpi<72 || dpi>600) stop('dpi must be between 72 and 600; default 300.')
suppressPackageStartupMessages({library(ggplot2);library(patchwork);library(pdftools)})
out <- args[2]
if (file.exists(out) || dir.exists(out)) stop('Output must be a new directory; refusing to overwrite.')
if (!dir.create(out,recursive=TRUE)) stop('Cannot create output directory.')
get <- function(fid,ids) setNames(lapply(ids,function(id)readRDS(panel_file(fid,id))),ids)
checks <- list()
save <- function(fid,p,w,h) {
  pdf <- file.path(out,paste0(fid,'.pdf'))
  ggsave(pdf,p,width=w,height=h,device=cairo_pdf,bg='white',limitsize=FALSE)
  invisible(pdf_convert(pdf,format='png',pages=1,dpi=dpi,
    filenames=file.path(out,paste0(fid,'.png')),verbose=FALSE))
  checks[[length(checks)+1]] <<- data.frame(figure=fid,width_in=w,height_in=h,png_dpi=dpi)
}
p<-get('Figure_2',c('A','B','C'));save('Figure_2',p$A/(p$B+p$C)+plot_layout(heights=c(1.15,1)),26,17)
p<-get('Figure_3',c('A','B','C'));save('Figure_3',p$A/p$B/p$C+plot_layout(heights=c(1,1,1.45),guides='keep'),28,24)
for(fid in c('Figure_4','Figure_5','Figure_S1','Figure_S5')){
 p<-get(fid,c('A','B','C','D'));h<-c(Figure_4=21,Figure_5=24,Figure_S1=20,Figure_S5=19)[fid]
 save(fid,(p$A+p$B)/(p$C+p$D),28,h)
}
p<-get('Figure_6',c('A','B'));save('Figure_6',p$A/p$B+plot_layout(heights=c(2.7,1)),25,28)
p<-get('Figure_S2',c('A','B'));save('Figure_S2',p$A/p$B,26,21)
p<-get('Figure_S3','');save('Figure_S3',p[[1]],24,16)
p<-get('Figure_S4',c('A','B','C'));save('Figure_S4',(p$A+p$B)/p$C+plot_layout(heights=c(1,1.12)),28,26)
p<-get('Figure_S6',c('A','B'));save('Figure_S6',p$A/free(p$B,side='l'),26,21)
p<-get('Figure_S7',c('A','B'));save('Figure_S7',p$A/p$B,26,22)
p<-get('Figure_S8',c('A','B'));save('Figure_S8',p$A/p$B+plot_layout(heights=c(1.6,1)),26,25)
write.csv(do.call(rbind,checks),file.path(out,'export_dimensions.csv'),row.names=FALSE)
capture.output(sessionInfo(),file=file.path(out,'R_sessionInfo.txt'))
