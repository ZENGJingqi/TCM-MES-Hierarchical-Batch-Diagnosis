suppressPackageStartupMessages({library(dplyr);library(tidyr);library(stringr);library(readxl);library(readr)})
s<-Sys.getenv('TCM_LINEAGE_STAGE');data_dir<-s;joint_path<-file.path(s,'joint.csv');out<-file.path(s,'output');dir.create(out,showWarnings=FALSE)
# Reuse only original input/relationship construction, never execute fits or figure builders.
active<-FALSE
for(e in parse(file.path(s,'p1.R'))){
 if(!is.call(e)||!identical(e[[1]],as.name('<-')))next
 nm<-as.character(e[[2]])[1]
 if(nm %in% c('choose_file','norm_batch','parse_numeric_vector','parse_numeric_mean','split_batch_vector','safe_mean','safe_first'))eval(e)
 if(nm=='d2_raw')active<-TRUE
 if(active)eval(e)
 if(nm=='yam_long')break
}
stopifnot(nrow(joint)==1477,n_distinct(extract_long$extract_batch)==241,n_distinct(yam_long$yam_batch)==235,n_distinct(chenpi_long$chenpi_batch)==32)
j<-read_csv(joint_path,show_col_types=FALSE) |> mutate(finished_batch=norm_batch(finished_batch),prod=as.Date(mes_production_date),obs=as.Date(d2_observation_date)) |> arrange(prod,finished_batch) |> mutate(id=sprintf('L-%04d',row_number()))
starts<-as.Date(c('2026-01-01','2026-03-01','2026-05-01','2026-07-01'));ends<-as.Date(c('2026-02-28','2026-04-30','2026-06-30','2026-07-31'))
edges<-list(extract=d3_extract_long |> transmute(finished_batch,upstream=extract_batch),yam=d3_yam_long |> transmute(finished_batch,upstream=yam_batch))
rows<-list();z<-1
for(dd in c(0,14))for(w in 1:4){
 av<-j$obs+dd;av[j$obs<j$prod]<-as.Date(NA);tr<-j$finished_batch[j$prod<starts[w]&!is.na(av)&av<starts[w]];te<-which(j$prod>=starts[w]&j$prod<=ends[w])
 for(layer in names(edges)){
  ed<-edges[[layer]];seen<-unique(ed$upstream[ed$finished_batch%in%tr])
  for(i in te){ids<-unique(ed$upstream[ed$finished_batch==j$finished_batch[i]]);n<-length(ids);ns<-sum(ids%in%seen);status<-if(n==0)'unknown' else if(ns==n)'all_seen' else if(ns==0)'all_unseen' else 'mixed'
   rows[[z]]<-tibble(delay=dd,window=w,layer=layer,id=j$id[i],upstream_n=n,seen_n=ns,status=status);z<-z+1
  }
 }
}
r<-bind_rows(rows);counts<-r |> count(delay,window,layer,status,name='finished_n');stopifnot(all((r |> count(delay,layer))$n==755))
write_csv(r,file.path(out,'test_batch_identity_status.csv'));write_csv(counts,file.path(out,'identity_status_by_window.csv'));write_csv(r |> count(delay,layer,status,name='finished_n'),file.path(out,'identity_status_pooled.csv'))
layers<-list(extract=extract_long |> transmute(up=extract_batch,down=finished_batch),chenpi=chenpi_long |> transmute(up=chenpi_batch,down=finished_batch),yam=yam_long |> transmute(up=yam_batch,down=finished_batch))
summ<-list();components<-list()
for(layer in names(layers)){
 e<-distinct(layers[[layer]]);uu<-sort(unique(e$up));dd<-sort(unique(e$down));a<-matrix(0,length(uu),length(dd));a[cbind(match(e$up,uu),match(e$down,dd))]<-1
 shared<-tcrossprod(a);diag(shared)<-0;adj<-shared>0;parent<-seq_along(uu)
 root<-function(i){while(parent[i]!=i)i<-parent[i];i}
 pairs<-which(upper.tri(adj)&adj,arr.ind=TRUE)
 if(nrow(pairs))for(k in 1:nrow(pairs)){u<-root(pairs[k,1]);v<-root(pairs[k,2]);if(u!=v)parent[v]<-u}
 groups<-sapply(seq_along(uu),root);sizes<-as.integer(table(groups));mult<-colSums(a)
 summ[[layer]]<-tibble(layer=layer,upstream_n=length(uu),finished_n=length(dd),edge_n=nrow(e),finished_shared_by_multiple_upstream=sum(mult>1),upstream_with_shared_descendant=sum(rowSums(adj)>0),overlapping_upstream_pairs=nrow(pairs),components=length(sizes),largest_component=max(sizes),max_shared_descendants=if(nrow(pairs))max(shared[pairs]) else 0)
 components[[layer]]<-tibble(layer=layer,component_seq=seq_along(sizes),upstream_n=sizes)
 stopifnot(sum(sizes)==length(uu),sum(mult)==nrow(e))
}
su<-bind_rows(summ);write_csv(su,file.path(out,'P1_shared_descendant_summary.csv'));write_csv(bind_rows(components),file.path(out,'P1_component_sizes.csv'))
writeLines(c('PASS: current matrix 1477 batches; each delay/layer contains identical 755 test batches.','PASS: P1 reconstructed upstream counts extract241/chenpi32/yam235.','PASS: distinct edges; component sizes reconcile; no raw batch identifiers exported.','Seen means connected to a label-eligible training batch at window start, not physically first used or QC available.','Unknown means absent parsed reference; partial omitted references cannot be detected.','P1 components reflect shared descendants within each layer, not full independence across time or other layers.','No model refits, no revised confidence intervals.'),file.path(out,'QA.txt'))
print(r |> count(delay,layer,status));print(su,width=Inf);capture.output(sessionInfo(),file=file.path(out,'sessionInfo.txt'))
