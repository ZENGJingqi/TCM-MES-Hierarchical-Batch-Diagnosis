suppressPackageStartupMessages({library(dplyr);library(tidyr);library(stringr);library(readxl)})
s <- Sys.getenv('TCM_P1_DEP_STAGE')
if (!nzchar(s)) stop('TCM_P1_DEP_STAGE is required')
data_dir <- s
joint_path <- file.path(s, 'joint.csv')
out <- file.path(s, 'output')
dir.create(out, showWarnings=FALSE)

# Execute the original P1 input construction only, through its upstream summaries.
active <- FALSE
for (expr in parse(file.path(s, 'p1.R'))) {
  if (!is.call(expr) || !identical(expr[[1]], as.name('<-'))) next
  name <- as.character(expr[[2]])[1]
  if (name %in% c('choose_file','norm_batch','parse_numeric_vector','parse_numeric_mean',
                  'split_batch_vector','safe_mean','safe_first','partial_spearman_once')) eval(expr)
  if (name == 'd2_raw') active <- TRUE
  if (active) eval(expr)
  if (name == 'labels') break
}
stopifnot(nrow(joint)==1477, nrow(extract_batch_summary)==241,
          nrow(chenpi_batch_summary)==32, nrow(yam_batch_summary)==235)
original <- read.csv(file.path(s,'original.csv'), check.names=FALSE, stringsAsFactors=FALSE)
finished <- finished_outcome |> arrange(finished_batch)
stopifnot(nrow(finished)==1477, !anyDuplicated(finished$finished_batch))
y <- as.numeric(finished$disintegration_issue)
month_id <- as.integer(factor(finished$production_month))
month_groups <- split(seq_len(nrow(finished)), month_id)
month_n <- length(month_groups)

sum_by <- function(values, group, n) {
  z <- rowsum(values, group, reorder=TRUE)
  ans <- numeric(n)
  ans[as.integer(rownames(z))] <- z[,1]
  ans
}
expected_rate <- function(weights) {
  counts <- sum_by(weights, month_id, month_n)
  positives <- sum_by(weights*y, month_id, month_n)
  original_rate <- sum_by(y, month_id, month_n)/sum_by(rep(1,nrow(finished)), month_id, month_n)
  n <- counts[month_id]
  q <- positives[month_id]
  ifelse(n>1, (q-y)/(n-1), ifelse(n==1, q, original_rate[month_id]))
}
specs <- list(
  list(layer='Extract-powder batch',edge=extract_long,up='extract_batch',summary=extract_batch_summary,vars=extract_vars),
  list(layer='Chenpi batch',edge=chenpi_long,up='chenpi_batch',summary=chenpi_batch_summary,vars=chenpi_vars),
  list(layer='Yam-powder batch',edge=yam_long,up='yam_batch',summary=yam_batch_summary,vars=yam_vars)
)
for (k in seq_along(specs)) {
  a <- specs[[k]]
  a$up_index <- match(a$edge[[a$up]], a$summary[[a$up]])
  a$down_index <- match(a$edge$finished_batch, finished$finished_batch)
  stopifnot(!anyNA(a$up_index), !anyNA(a$down_index),
            !anyDuplicated(paste(a$up_index,a$down_index,sep=':')))
  specs[[k]] <- a
}
stopifnot(nrow(specs[[1]]$edge)==804,nrow(specs[[2]]$edge)==869,nrow(specs[[3]]$edge)==1645)

measure <- function(x, yy, zz, rr, method) {
  if (method=='unadjusted') {v<-yy;ok<-is.finite(x)&is.finite(v)}
  else if (method=='residual_adjusted') {v<-rr;ok<-is.finite(x)&is.finite(v)}
  else {v<-yy;ok<-is.finite(x)&is.finite(v)&is.finite(zz)}
  if (sum(ok)<10 || length(unique(x[ok]))<3 || length(unique(v[ok]))<3) return(NA_real_)
  if (method=='time_adjusted') {
    if (length(unique(zz[ok]))<2) return(NA_real_)
    return(partial_spearman_once(x[ok],yy[ok],zz[ok]))
  }
  suppressWarnings(unname(cor(x[ok],v[ok],method='spearman')))
}
aggregate_layer <- function(a, weights, exp_rate) {
  w <- weights[a$down_index]
  n <- nrow(a$summary)
  den <- sum_by(w,a$up_index,n)
  issue <- sum_by(w*y[a$down_index],a$up_index,n)/den
  calendar <- sum_by(w*exp_rate[a$down_index],a$up_index,n)/den
  residual <- sum_by(w*(y[a$down_index]-exp_rate[a$down_index]),a$up_index,n)/den
  list(y=issue,z=calendar,r=residual)
}
methods <- c('unadjusted','residual_adjusted','time_adjusted')
keys <- unlist(lapply(specs,function(a) unlist(lapply(a$vars,function(v) paste(v,methods,sep='__')))))
stopifnot(length(keys)==39, !anyDuplicated(keys), nrow(original)==13)
calculate <- function(weights, with_aggregates=FALSE, upstream_draws=NULL) {
  er <- expected_rate(weights)
  result <- numeric(0)
  aggs <- vector('list',length(specs))
  for (k in seq_along(specs)) {
    a <- specs[[k]]
    agg <- aggregate_layer(a,weights,er)
    aggs[[k]] <- agg
    pick <- if (is.null(upstream_draws)) seq_len(nrow(a$summary)) else upstream_draws[[k]]
    for (v in a$vars) for (m in methods)
      result <- c(result,measure(as.numeric(a$summary[[v]])[pick],agg$y[pick],agg$z[pick],agg$r[pick],m))
  }
  names(result) <- keys
  if (with_aggregates) list(values=result,aggregates=aggs) else result
}

base <- calculate(rep(1,nrow(finished)),TRUE)
stopifnot(max(abs(expected_rate(rep(1,nrow(finished)))-finished$expected_month_rate_loo))<1e-12)
original_map <- setNames(seq_len(nrow(original)),original$variable)
numeric_comparison <- list();d<-1L
for (a in specs) for (v in a$vars) for (m in methods) {
  recorded <- original[[paste0('rho_',m)]][original_map[[v]]]
  rebuilt <- base$values[[paste(v,m,sep='__')]]
  numeric_comparison[[d]] <- data.frame(variable=v,method=m,original=recorded,weighted_rebuild=rebuilt,absolute_difference=abs(recorded-rebuilt))
  d<-d+1L
}
numeric_comparison<-bind_rows(numeric_comparison)
write.csv(numeric_comparison,file.path(out,'original_reconciliation.csv'),row.names=FALSE,na='')
message('Original versus weighted all-one reconstruction maximum rho difference: ',max(numeric_comparison$absolute_difference))
if (max(numeric_comparison$absolute_difference)>0.01) stop('Original point estimates materially differ from weighted reconstruction')
# Keep the original P1 point estimates and original aggregation as the reference.
# Near-ties in its calendar-adjusted ranks can be broken by floating-point summation order.
for (k in seq_along(specs)) {
  z<-specs[[k]]$summary
  base$aggregates[[k]]<-list(y=z$descendant_issue_rate,z=z$descendant_expected_month_rate,r=z$descendant_month_residual_mean)
}
for (a in specs) for (v in a$vars) for (m in methods)
  base$values[[paste(v,m,sep='__')]]<-original[[paste0('rho_',m)]][original_map[[v]]]

B <- 1200L
set.seed(20260923)
within <- matrix(NA_real_,B,length(keys),dimnames=list(NULL,keys))
blocks <- matrix(NA_real_,B,length(keys),dimnames=list(NULL,keys))
for (b in seq_len(B)) {
  sampled <- unlist(lapply(month_groups,function(idx) idx[sample.int(length(idx),length(idx),replace=TRUE)]),use.names=FALSE)
  within[b,] <- calculate(tabulate(sampled,nbins=nrow(finished)))
  drawn_month <- sample.int(month_n,month_n,replace=TRUE)
  blocks[b,] <- calculate(tabulate(drawn_month,nbins=month_n)[month_id])
  if (b %% 200L == 0L) message('Completed bootstrap pair ',b,'/',B)
}
write.csv(data.frame(replicate=seq_len(B),within,check.names=FALSE),file.path(out,'within_month_replicates.csv'),row.names=FALSE,na='')
write.csv(data.frame(replicate=seq_len(B),blocks,check.names=FALSE),file.path(out,'month_block_replicates.csv'),row.names=FALSE,na='')
set.seed(20260924)
two_way <- matrix(NA_real_,B,length(keys),dimnames=list(NULL,keys))
for (b in seq_len(B)) {
  sampled <- unlist(lapply(month_groups,function(idx) idx[sample.int(length(idx),length(idx),replace=TRUE)]),use.names=FALSE)
  draws <- lapply(specs,function(a) sample.int(nrow(a$summary),nrow(a$summary),replace=TRUE))
  two_way[b,] <- calculate(tabulate(sampled,nbins=nrow(finished)),upstream_draws=draws)
}
write.csv(data.frame(replicate=seq_len(B),two_way,check.names=FALSE),file.path(out,'two_way_replicates.csv'),row.names=FALSE,na='')
interval <- function(z) {
  good <- z[is.finite(z)]
  if (length(good)<0.9*B) c(valid=length(good),low=NA_real_,high=NA_real_)
  else c(valid=length(good),low=unname(quantile(good,.025)),high=unname(quantile(good,.975)))
}
rows <- list();j<-1L
for (a in specs) for (v in a$vars) for (m in methods) {
  key <- paste(v,m,sep='__'); orig <- original[original$variable==v,,drop=FALSE]
  wi <- interval(within[,key]); bl <- interval(blocks[,key]); tw <- interval(two_way[,key])
  rows[[j]] <- data.frame(layer=a$layer,variable=v,method=m,upstream_n=nrow(a$summary),
    original_rho=base$values[[key]],original_batch_ci_low=orig[[paste0('ci_low_',m)]],
    original_batch_ci_high=orig[[paste0('ci_high_',m)]],original_batch_fdr=orig[[paste0('fdr_',m)]],
    within_month_valid=unname(wi['valid']),within_month_ci_low=unname(wi['low']),within_month_ci_high=unname(wi['high']),
    month_block_valid=unname(bl['valid']),month_block_ci_low=unname(bl['low']),month_block_ci_high=unname(bl['high']),
    two_way_valid=unname(tw['valid']),two_way_ci_low=unname(tw['low']),two_way_ci_high=unname(tw['high']))
  j<-j+1L
}
comparison<-bind_rows(rows)
stopifnot(nrow(comparison)==39)
write.csv(comparison,file.path(out,'dependency_interval_comparison.csv'),row.names=FALSE,na='')

# Delete each same-layer connected component once; output component sizes only.
component_rows <- list();j<-1L
for (k in c(2L,3L)) {
  a <- specs[[k]]; nu <- nrow(a$summary)
  incidence <- matrix(0L,nu,nrow(finished))
  incidence[cbind(a$up_index,a$down_index)] <- 1L
  shared <- tcrossprod(incidence)>0
  diag(shared)<-FALSE
  parent<-seq_len(nu)
  root<-function(i){while(parent[i]!=i)i<-parent[i];i}
  pair<-which(upper.tri(shared)&shared,arr.ind=TRUE)
  if(nrow(pair)) for(q in seq_len(nrow(pair))){u<-root(pair[q,1]);v<-root(pair[q,2]);if(u!=v)parent[v]<-u}
  groups<-split(seq_len(nu),vapply(seq_len(nu),root,integer(1)))
  expected_comp<-if(k==2L) 16L else 13L
  stopifnot(length(groups)==expected_comp)
  agg<-base$aggregates[[k]]
  for (g in seq_along(groups)) {
    keep<-setdiff(seq_len(nu),groups[[g]])
    for (v in a$vars) for(m in methods) {
      rho<-measure(as.numeric(a$summary[[v]])[keep],agg$y[keep],agg$z[keep],agg$r[keep],m)
      component_rows[[j]]<-data.frame(layer=a$layer,component_seq=g,removed_upstream_n=length(groups[[g]]),
        variable=v,method=m,remaining_upstream_n=length(keep),rho=rho)
      j<-j+1L
    }
  }
}
component_table<-bind_rows(component_rows)
write.csv(component_table,file.path(out,'component_leaveout.csv'),row.names=FALSE,na='')
influence<-component_table |> group_by(layer,variable,method) |>
  summarise(components=n(),valid=sum(is.finite(rho)),rho_min=if(all(!is.finite(rho))) NA_real_ else min(rho,na.rm=TRUE),
            rho_max=if(all(!is.finite(rho))) NA_real_ else max(rho,na.rm=TRUE),
            same_sign=sum(is.finite(rho)&sign(rho)==sign(base$values[[paste(first(variable),first(method),sep='__')]])),.groups='drop')
write.csv(influence,file.path(out,'component_influence_summary.csv'),row.names=FALSE,na='')

qa<-c(
  paste('PASS: original P1 points retained for',length(keys),'combinations; maximum weighted reconstruction rank-tie difference',format(max(numeric_comparison$absolute_difference),digits=5),'.'),
  paste('PASS: original cohort 1477; upstream batches 241/32/235; distinct edges 804/869/1645; production months',month_n),
  paste('PASS: fixed seed 20260923; paired within-month and month-block replicates',B,'each.'),
  paste('Within-month interval unavailable:',sum(is.na(comparison$within_month_ci_low)),'of 39.'),
  paste('Month-block interval unavailable:',sum(is.na(comparison$month_block_ci_low)),'of 39.'),
  paste('Two-way interval unavailable:',sum(is.na(comparison$two_way_ci_low)),'of 39.'),
  'Shared finished-product draws are applied to every connected upstream unit; all source batch IDs stay out of outputs.',
  'New intervals are exploratory conditional resampling diagnostics, not causal evidence or a replacement FDR test.'
)
writeLines(qa,file.path(out,'QA.txt'))
capture.output(sessionInfo(),file=file.path(out,'sessionInfo.txt'))
message(paste(qa,collapse='\n'))
