#!/usr/bin/env Rscript
# Step 3.1: closed-form reference calculations, never production helpers.
# Run from the manuscript root: Rscript --vanilla scripts/validate-scalar-fpc.R
args <- commandArgs(trailingOnly=FALSE)
script_arg <- sub('^--file=', '', args[grepl('^--file=',args)][1L])
root <- normalizePath(file.path(dirname(script_arg), '..'), mustWork=TRUE)
setwd(root)
outdir <- file.path(root,'qa','validation')
dir.create(outdir,recursive=TRUE,showWarnings=FALSE)
lib <- normalizePath(file.path(root,'qa','r-library'),mustWork=TRUE)
.libPaths(c(lib,.libPaths()))
stopifnot(normalizePath(find.package('sitemix'))==normalizePath(file.path(lib,'sitemix')))
stopifnot(as.character(utils::packageVersion('sitemix'))=='0.3.1')
library(sitemix)
identity <- jsonlite::read_json('qa/software-identity.json',simplifyVector=TRUE)
manifest <- jsonlite::read_json('qa/software-identity/source-files.json',simplifyVector=TRUE)
verify_source <- function() {
  observed <- vapply(file.path('qa/software-source',manifest$path),
    function(p) digest::digest(file=p,algo='sha256',serialize=FALSE),character(1))
  stopifnot(identical(unname(observed),manifest$sha256))
  TRUE
}
start <- Sys.time(); source_before <- verify_source()
cat('Step 3.1 scalar/FPC verification\nPackage:',find.package('sitemix'),'\nVersion:',as.character(packageVersion('sitemix')),'\n')
cat('Independent oracle: base-R score quadratic, augmented counts, empirical sample variance, and closed-form delta rules.\n')

# Distinct derivation: solve the Wilson score quadratic and divide its full
# width by 2z. No .sm_* helper and no test-suite oracle is called.
score_radius_over_z <- function(C,n,z) {
  A <- n+z*z; B <- -(2*C+z*z); D <- C*C/n
  discriminant <- B*B-4*A*D
  roots <- sort(c((-B-sqrt(discriminant))/(2*A),(-B+sqrt(discriminant))/(2*A)))
  (roots[2]-roots[1])/(2*z)
}
reference <- function(C,n,N,scale,boundary,correction) {
  p <- C/n; edge <- C==0 || C==n; z <- qnorm(.975)
  y <- c(rep(1,C),rep(0,n-C))
  # The population (divisor n) central moment estimates Bernoulli variance.
  raw_variance <- mean((y-mean(y))^2)/n
  corrected <- correction=='bc' && !edge
  if (corrected) raw_variance <- stats::var(y)/n
  raw_method <- if (corrected) 'binomial_bc' else 'binomial'
  if (edge && boundary=='wilson_floor') {
    raw_variance <- score_radius_over_z(C,n,z)^2
    raw_method <- 'wilson_boundary_surrogate'
  }
  if (edge && boundary=='agresti_coull') {
    adjusted_success <- C+z^2/2
    adjusted_failure <- n-C+z^2/2
    total <- adjusted_success+adjusted_failure
    raw_variance <- adjusted_success*adjusted_failure/total^3
    raw_method <- 'agresti_coull_boundary_surrogate'
  }
  q <- if (is.na(N)) 1 else if (N==n) 0 else (N-n)/(N-1)
  applied <- if (is.na(N)) 1 else if (corrected) (N-n)/N else q
  raw_variance <- raw_variance*applied
  if (scale=='raw') {
    eta <- p; variance <- raw_variance; method <- raw_method; scale_name <- 'none'
  } else if (scale=='arcsine') {
    eta <- asin(sqrt(p)); scale_name <- 'arcsine'
    variance <- if(corrected) raw_variance/(4*p*(1-p)) else q/(4*n)
    method <- if(corrected) 'arcsine_delta_binomial_bc' else 'arcsine_vst'
  } else if (scale=='logit') {
    stopifnot(!edge)
    eta <- log(p)-log1p(-p); variance <- raw_variance/(p^2*(1-p)^2)
    method <- if(corrected) 'logit_delta_binomial_bc' else 'logit_delta'; scale_name <- 'logit'
  } else {
    stopifnot(scale=='anscombe',correction=='none',boundary!='agresti_coull')
    eta <- asin(sqrt((C+3/8)/(n+3/4)))
    variance <- q/(4*(n+1/2)); method <- 'arcsine_anscombe'; scale_name <- 'arcsine_anscombe'
  }
  values <- c(theta_raw=p,theta_hat=eta,se_raw=sqrt(raw_variance),se=sqrt(variance),
    n_eff=n+if(scale=='anscombe') .5 else 0)
  meta <- c(estimate_scale=scale_name,var_method=method,flag_zero_cell=as.character(edge))
  if (!is.na(N)) {
    values <- c(values,population_size=N,sampling_fraction=n/N,fpc_variance_multiplier=q,
      fpc_se_multiplier=sqrt(q),variance_multiplier_applied=applied,se_multiplier_applied=sqrt(applied))
    meta <- c(meta,sampling_design='SRSWOR',variance_rule=if(corrected) 'design_corrected' else 'plugin')
  }
  list(values=values,meta=meta)
}
counts <- function(C,n) data.frame(site_id='S',year=2026L,n_jt=as.integer(n),c_jt_x=as.integer(C))
public_estimate <- function(C,n,N=NA_real_,scale='raw',boundary='wilson_floor',correction='none',...) {
  sm_estimate_from_counts(counts(C,n),family='binomial',indicator='x',
    vst=switch(scale,raw='none',anscombe='arcsine',scale),anscombe=scale=='anscombe',
    boundary_method=boundary,bias_correction=if(correction=='bc') 'binomial_bc' else NULL,
    fpc=if(is.na(N)) NULL else N,min_n=1L,...)
}
# Purposeful cells, not a performance/coverage study. Each design is repeated
# without FPC, under SRSWOR, and as an exact census including N=n=1.
base_cells <- data.frame(C=c(0,1,0,10,1,3,1,99,50),n=c(1,1,10,10,2,8,100,100,100))
configurations <- list()
for(i in seq_len(nrow(base_cells))) {
  C <- base_cells$C[i]; n <- base_cells$n[i]; edge <- C==0 || C==n
  for(N in c(NA_real_,2*n+3,n)) {
    for(scale in c('raw','arcsine')) for(boundary in if(edge) c('none','wilson_floor','agresti_coull') else 'none')
      for(correction in c('none','bc')) configurations[[length(configurations)+1L]] <-
        list(C=C,n=n,N=N,scale=scale,boundary=boundary,correction=correction)
    configurations[[length(configurations)+1L]] <- list(C=C,n=n,N=N,scale='anscombe',boundary='wilson_floor',correction='none')
    if(!edge) for(correction in c('none','bc')) configurations[[length(configurations)+1L]] <-
      list(C=C,n=n,N=N,scale='logit',boundary='none',correction=correction)
  }
}
# Explicit boundary-policy nonactivation checks in an interior cell.
for(boundary in c('wilson_floor','agresti_coull')) configurations[[length(configurations)+1L]] <-
  list(C=3,n=8,N=20,scale='raw',boundary=boundary,correction='none')
num <- list(); meta <- list(); unexpected <- list(); warnings_seen <- character()
absolute_tolerance <- 1e-12; relative_tolerance <- 1e-10
capture <- function(expr) withCallingHandlers(expr,warning=function(w) {
  warnings_seen <<- c(warnings_seen,conditionMessage(w)); invokeRestart('muffleWarning')
})
for(i in seq_along(configurations)) {
  cfg <- configurations[[i]]; id <- sprintf('S%03d',i)
  ref <- do.call(reference,cfg)
  got <- tryCatch(capture(do.call(public_estimate,cfg)),error=base::identity)
  if(inherits(got,'error')) {
    unexpected[[length(unexpected)+1L]] <- list(case_id=id,config=cfg,class=class(got),message=conditionMessage(got)); next
  }
  for(field in names(ref$values)) {
    expected <- unname(ref$values[field]); actual <- as.numeric(got[[field]][1])
    tol <- absolute_tolerance+relative_tolerance*abs(expected); err <- abs(actual-expected)
    num[[length(num)+1L]] <- data.frame(case_id=id,as.data.frame(cfg),field=field,expected=expected,actual=actual,
      absolute_error=err,tolerance=tol,pass=is.finite(err)&&err<=tol)
  }
  for(field in names(ref$meta)) meta[[length(meta)+1L]] <- data.frame(case_id=id,field=field,
    expected=unname(ref$meta[field]),actual=as.character(got[[field]][1]),
    pass=identical(unname(ref$meta[field]),as.character(got[[field]][1])))
}
numeric_results <- do.call(rbind,num); metadata_results <- do.call(rbind,meta)
write.csv(numeric_results,file.path(outdir,'scalar-fpc.csv'),row.names=FALSE,na='NA')
write.csv(metadata_results,file.path(outdir,'scalar-fpc-metadata.csv'),row.names=FALSE,na='NA')

# Exact finite-population enumeration: a genuinely independent sampling oracle.
# Enumerate all 70 unordered samples of size 4 from eight fixed binary units.
Y <- c(1,1,1,0,0,0,0,0); sample_n <- 4L; pop_N <- length(Y)
subsets <- combn(seq_along(Y),sample_n)
sample_means <- apply(subsets,2,function(index) mean(Y[index]))
true_p <- mean(Y); enumerated_variance <- mean((sample_means-true_p)^2)
closed_form_variance <- true_p*(1-true_p)/sample_n*(pop_N-sample_n)/(pop_N-1)
# Compare every API design-corrected variance with the empirical sample-variance
# estimator. A boundary_method='none' keeps exact 0 in all-zero/all-one samples.
design <- lapply(seq_len(ncol(subsets)),function(k) {
  y <- Y[subsets[,k]]; C <- sum(y)
  expected <- (1-sample_n/pop_N)*var(y)/sample_n
  got <- capture(public_estimate(C,sample_n,pop_N,boundary='none',correction='bc'))$se_raw^2
  data.frame(sample_id=k,C=C,n=sample_n,N=pop_N,expected_variance=expected,actual_variance=got,
    absolute_error=abs(got-expected),pass=abs(got-expected)<=absolute_tolerance+relative_tolerance*abs(expected))
})
design_results <- do.call(rbind,design)
write.csv(design_results,file.path(outdir,'scalar-fpc-design.csv'),row.names=FALSE)
design_identity_pass <- abs(enumerated_variance-closed_form_variance)<1e-14 &&
  abs(mean(design_results$actual_variance)-enumerated_variance)<1e-14

# Invalid requests are assessed separately; they are not numerical accuracy cases.
error_specs <- list(
 list(id='E01',expected='sitemix_error_estimate_var_method',args=list(C=0,n=10,scale='logit')),
 list(id='E02',expected='sitemix_error_estimate_var_method',args=list(C=10,n=10,scale='logit',boundary='agresti_coull')),
 list(id='E03',expected='sitemix_error_estimate_var_method',args=list(C=0,n=1,N=1,scale='logit')),
 list(id='E04',expected='sitemix_error_anscombe_incompatible_correction',args=list(C=3,n=8,scale='anscombe',correction='bc')),
 list(id='E05',expected='sitemix_error_anscombe_incompatible_correction',args=list(C=0,n=10,scale='anscombe',boundary='agresti_coull')),
 list(id='E06',expected='sitemix_error_invalid_fpc',args=list(C=3,n=8,N=7)),
 list(id='E07',expected='sitemix_error_invalid_fpc',args=list(C=3,n=8,N=20.5))
)
error_results <- lapply(error_specs,function(x) {
  e <- tryCatch({do.call(public_estimate,x$args);NULL},error=base::identity)
  data.frame(case_id=x$id,expected_class=x$expected,actual_class=if(is.null(e)) 'NO_ERROR' else paste(class(e),collapse=';'),
    pass=!is.null(e)&&inherits(e,x$expected),message=if(is.null(e)) '' else conditionMessage(e))
})
multinomial_error <- function(id,C,n,expected,...) {
  e <- tryCatch({sm_estimate_from_counts(data.frame(site_id='S',year=2026L,n_jt=as.integer(n),c_jt_a=as.integer(C),c_jt_b=as.integer(n-C)),
    family='multinomial',indicators=c('a','b'),vst='none',vjt=TRUE,min_n=1L,...);NULL},error=base::identity)
  data.frame(case_id=id,expected_class=expected,actual_class=if(is.null(e)) 'NO_ERROR' else paste(class(e),collapse=';'),
    pass=!is.null(e)&&inherits(e,expected),message=if(is.null(e)) '' else conditionMessage(e))
}
error_results[[length(error_results)+1L]] <- multinomial_error('E08',0,10,'sitemix_error_estimate_vcov_invariant',boundary_method='agresti_coull')
error_results[[length(error_results)+1L]] <- multinomial_error('E09',0,1,'sitemix_error_estimate_var_method',bias_correction='binomial_bc')
error_results <- do.call(rbind,error_results)
write.csv(error_results,file.path(outdir,'scalar-fpc-errors.csv'),row.names=FALSE,na='NA')
source_after <- verify_source()
census_se <- numeric_results[!is.na(numeric_results$N) & numeric_results$N==numeric_results$n & numeric_results$field %in% c('se_raw','se'),,drop=FALSE]
census_nonzero <- sum(census_se$actual!=0)
failed <- census_nonzero+ sum(!numeric_results$pass)+sum(!metadata_results$pass)+sum(!design_results$pass)+sum(!error_results$pass)+
  length(unexpected)+as.integer(!design_identity_pass)
summary <- list(step='Step 3.1',status=if(failed==0) 'passed' else 'failed',
  started_utc=format(start,tz='UTC',usetz=TRUE),ended_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
  package_path=find.package('sitemix'),package_version=as.character(packageVersion('sitemix')),
  source_manifest_sha256=identity$source_manifest_sha256,public_commit=identity$public_source$commit,
  source_before_unchanged=source_before,source_after_unchanged=source_after,census_SE_comparisons=nrow(census_se),census_exact_zero_failures=census_nonzero,
  scalar_configurations=length(configurations),scalar_successful_configurations=length(unique(numeric_results$case_id)),
  numeric_comparisons=nrow(numeric_results),numeric_failed=sum(!numeric_results$pass),
  max_absolute_error=max(numeric_results$absolute_error),absolute_tolerance=absolute_tolerance,relative_tolerance=relative_tolerance,
  metadata_comparisons=nrow(metadata_results),metadata_failed=sum(!metadata_results$pass),
  design_samples=nrow(design_results),design_failures=sum(!design_results$pass),
  design_enumerated_variance=enumerated_variance,design_formula_variance=closed_form_variance,
  mean_API_design_corrected_variance=mean(design_results$actual_variance),design_identity_pass=design_identity_pass,
  expected_error_fixtures=nrow(error_results),expected_error_matches=sum(error_results$pass),
  unexpected_errors=unexpected,warning_count=length(warnings_seen),warning_messages=unique(warnings_seen),
  total_failed_checks=failed,exit_code=as.integer(failed>0),
  oracle_independence='Reference helpers use only base R/statistics primitives; no sitemix internal helper or package test oracle is called. Wilson reference is solved from score-quadratic roots; corrected variance from observed binary sample variance.',
  scope='Selected public-API formula and domain checks, not full package tests, a coverage study, or a guarantee of statistical coverage/performance.',
  session_info=capture.output(sessionInfo()))
jsonlite::write_json(summary,file.path(outdir,'scalar-fpc-summary.json'),pretty=TRUE,auto_unbox=TRUE,null='null',digits=17)
cat(sprintf('Scalar configurations: %d; numeric comparisons: %d; max absolute error: %.4g\n',length(configurations),nrow(numeric_results),max(numeric_results$absolute_error)))
cat(sprintf('Metadata comparisons: %d; design samples: %d; expected error matches: %d/%d\n',nrow(metadata_results),nrow(design_results),sum(error_results$pass),nrow(error_results)))
cat(sprintf('Exact design variance: %.16g; mean API corrected variance: %.16g\n',enumerated_variance,mean(design_results$actual_variance)))
cat('Warnings:',length(warnings_seen),' Unexpected errors:',length(unexpected),' Failed checks:',failed,'\n')
cat(if(failed==0) 'SCALAR-FPC-OK\n' else 'SCALAR-FPC-FAILED\n')
quit(status=as.integer(failed>0))
