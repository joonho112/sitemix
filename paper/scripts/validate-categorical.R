#!/usr/bin/env Rscript
# Step 3.2: public API fixtures against independent algebra and count tables.
# Run from the manuscript root: Rscript scripts/validate-categorical.R
started <- Sys.time()
root <- normalizePath('.')
.libPaths(c(file.path(root, 'qa/r-library'), .libPaths()))
library(sitemix)
stopifnot(normalizePath(find.package('sitemix')) == normalizePath('qa/r-library/sitemix'))
output <- 'qa/validation'
dir.create(output, showWarnings = FALSE, recursive = TRUE)
abs_tol <- 1e-12
checks <- list(); matrices <- list(); scalars <- list(); errors <- list(); warnings_seen <- list(); tables <- list(); contrasts <- list()
flat <- function(x) paste(format(x, digits = 16, trim = TRUE), collapse = ';')
check <- function(id, actual, expected = TRUE, note = '', numeric = FALSE) {
  delta <- if (numeric && length(actual) == length(expected)) max(abs(as.numeric(actual) - as.numeric(expected))) else NA_real_
  ok <- if (numeric) is.finite(delta) && delta <= abs_tol else identical(actual, expected)
  checks[[length(checks) + 1L]] <<- data.frame(id, status = if (ok) 'pass' else 'FAIL', actual = flat(actual), expected = flat(expected), max_abs_error = delta, tolerance = if (numeric) abs_tol else NA_real_, note, stringsAsFactors = FALSE)
  invisible(ok)
}
run <- function(id, expr) withCallingHandlers(expr, warning = function(w) {
  warnings_seen[[length(warnings_seen) + 1L]] <<- data.frame(fixture = id, class = class(w)[1], message = conditionMessage(w))
  invokeRestart('muffleWarning')
})
expect_error <- function(id, expr, expected_class, interpretation) {
  z <- tryCatch(run(id, expr), error = identity)
  got <- if (inherits(z, 'error')) class(z)[1] else 'NO_ERROR'
  check(paste0(id, '_error_class'), inherits(z, expected_class), TRUE, interpretation)
  errors[[length(errors)+1L]] <<- data.frame(fixture=id,expected_class,actual_class=got,message=if(inherits(z,'error'))conditionMessage(z) else '',interpretation)
}
record_matrix <- function(id, actual, expected, labels = rownames(actual)) {
  check(paste0(id,'_matrix_oracle'),as.vector(actual),as.vector(expected),numeric=TRUE)
  g <- expand.grid(row=seq_len(nrow(actual)),column=seq_len(ncol(actual)))
  matrices[[length(matrices)+1L]] <<- data.frame(fixture=id,row=labels[g$row],column=labels[g$column],actual=as.vector(actual),expected=as.vector(expected),abs_error=abs(as.vector(actual)-as.vector(expected)))
}
# Independent one-hot population-count identity; no package-private helpers.
oracle_C <- function(counts, corrected=FALSE, N=NULL) {
  n <- sum(counts); p <- counts/n
  if (!is.null(N) && N == n) return(matrix(0,length(p),length(p)))
  M <- diag(p)-tcrossprod(p)
  V <- M / if (corrected) n-1 else n
  if (!is.null(N)) V <- V * if(corrected) (N-n)/N else (N-n)/(N-1)
  V
}
C_data <- function(counts,id='c') {
  dat <- data.frame(site_id=id,year=2026L,n_jt=sum(counts))
  for(k in seq_along(counts))dat[[paste0('c_jt_',letters[k])]] <- counts[k]
  dat
}
C_fixture <- function(id,counts,corrected=FALSE,N=NULL,boundary='wilson_floor',vst='none') {
  labels <- letters[seq_along(counts)]; n <- sum(counts)
  out <- run(id,sm_estimate_from_counts(C_data(counts,id),family='multinomial',indicators=labels,vjt=TRUE,vst=vst,boundary_method=boundary,bias_correction=if(corrected)'binomial_bc' else NULL,fpc=N,min_n=1L))
  mat <- as.matrix(out$V[[1]]); expected <- oracle_C(counts,corrected,N)
  record_matrix(id,mat,expected,labels)
  check(paste0(id,'_simplex'),as.vector(mat %*% rep(1,length(counts))),rep(0,length(counts)),numeric=TRUE)
  check(paste0(id,'_psd'),min(eigen(mat,symmetric=TRUE,only.values=TRUE)$values)>=-abs_tol)
  check(paste0(id,'_raw_scale'),out$V[[1]]$vcov_scale,'raw')
  check(paste0(id,'_support_rank'),out$V[[1]]$matrix_rank,as.integer(sum(counts>0)-1))
  num_rank <- sum(abs(eigen(mat,symmetric=TRUE,only.values=TRUE)$values)>abs_tol)
  scalars[[length(scalars)+1L]] <<- data.frame(fixture=id,category=labels,n,counts,p=out$theta_raw,se_raw=out$se_raw,row_se=out$se,estimate_scale=out$estimate_scale,matrix_diagonal=diag(mat),surrogate_gap=out$se_raw^2-diag(mat),scalar_correction_rule=out$V[[1]]$scalar_correction_rule,analytic_support_rank=out$V[[1]]$matrix_rank,numeric_rank=num_rank,population_size=if(is.null(N))NA_real_ else N)
  interior <- counts>0 & counts<n
  if(any(interior))check(paste0(id,'_interior_scalar_diag'),out$se_raw[interior]^2,diag(mat)[interior],numeric=TRUE)
  out
}
c1 <- C_fixture('C_interior',c(2,3,5))
c2 <- C_fixture('C_sparse',c(0,1,9))
c3 <- C_fixture('C_degenerate',c(0,0,10))
c4 <- C_fixture('C_n1',c(1,0))
c5 <- C_fixture('C_corrected',c(2,3,5),corrected=TRUE)
c6 <- C_fixture('C_FPC_plugin',c(2,3,5),N=20)
c7 <- C_fixture('C_FPC_corrected',c(2,3,5),corrected=TRUE,N=20)
c8 <- C_fixture('C_census_full_support',c(2,2,1),N=5)
c9 <- C_fixture('C_census_n1_corrected',c(1,0),corrected=TRUE,N=1)
c10 <- C_fixture('C_arcsine',c(2,3,5),vst='arcsine')
c11 <- C_fixture('C_boundary_none',c(0,1,9),boundary='none')
c12 <- C_fixture('C_AC_interior_allowed',c(2,3,5),boundary='agresti_coull')
check('C_sparse_positive_surrogate_zero_matrix',c2$se_raw[1]>0 && as.matrix(c2$V[[1]])[1,1]==0)
check('C_sparse_diag_contract',c2$V[[1]]$diag_contract,'row_se_raw_squared_except_boundary_surrogates')
check('C_census_numeric_rank_zero_metadata_two',all(as.matrix(c8$V[[1]])==0) && c8$V[[1]]$matrix_rank==2L)
check('C_census_scalar_zero',c8$se_raw,rep(0,3),numeric=TRUE)
check('C_arcsine_scale_difference',c10$estimate_scale[1]=='arcsine' && c10$V[[1]]$vcov_scale=='raw')
check('C_arcsine_V_raw_match',as.vector(as.matrix(c10$V[[1]])),as.vector(as.matrix(c1$V[[1]])),numeric=TRUE)
check('C_none_boundary_diag_equality',c11$se_raw^2,diag(as.matrix(c11$V[[1]])),numeric=TRUE)
expect_error('C_n0',sm_estimate_from_counts(C_data(c(0,0)),family='multinomial',indicators=c('a','b')),'sitemix_error_input_indicator_count','Zero denominator is not an observed zero category.')
missing_n <- C_data(c(2,3));missing_n$n_jt <- NA_real_
expect_error('C_missing_n',sm_estimate_from_counts(missing_n,family='multinomial',indicators=c('a','b')),'sitemix_error_input_type','Missing denominator cannot define a proportion.')
wrong_sum <- C_data(c(2,3));wrong_sum$n_jt <- 10
expect_error('C_incomplete_partition',sm_estimate_from_counts(wrong_sum,family='multinomial',indicators=c('a','b')),'sitemix_error_input_indicator_count','Complete category counts must sum to the denominator.')
expect_error('C_aggregate_wrapper',sm_estimate_from_aggregates(C_data(c(2,3,5)),family='multinomial',indicators=c('a','b','c')),'sitemix_error_ambiguous_dispatch','Published origin does not prohibit C counts, but aggregate wrapper does not dispatch C.')
expect_error('C_AC_boundary_matrix',sm_estimate_from_counts(C_data(c(0,1,9)),family='multinomial',indicators=c('a','b','c'),boundary_method='agresti_coull',vjt=TRUE),'sitemix_error_estimate_vcov_invariant','C Agresti-Coull matrix incompatibility is activated by boundary cells; interior fixture passes.')
# R2: same one-variable margins, different feasible joint tables.
Qlist <- list(lower=matrix(c(0,40,30,30),2,byrow=TRUE),upper=matrix(c(30,10,0,60),2,byrow=TRUE))
for(nm in names(Qlist)) {
  Q <- Qlist[[nm]];n <- sum(Q);pA <- rowSums(Q)/n;pB <- colSums(Q)/n
  VA <- (diag(pA)-tcrossprod(pA))/n;VB <- (diag(pB)-tcrossprod(pB))/n;cross <- (Q/n-outer(pA,pB))/n
  full <- rbind(cbind(VA,cross),cbind(t(cross),VB));labs=c('A1','A2','B1','B2');dimnames(full)=list(labs,labs)
  jointgrid <- expand.grid(A=1:2,B=1:2)
  inds <- rep(seq_len(4),times=as.vector(Q));units <- jointgrid[inds,,drop=FALSE]
  dat <- data.frame(site_id=paste0('R2_',nm),year=2026L,A1=as.integer(units$A==1),A2=as.integer(units$A==2),B1=as.integer(units$B==1),B2=as.integer(units$B==2))
  out <- sm_estimate(dat,family='multivariate',indicators=labs,vjt=TRUE,vst='none',boundary_method='none')
  record_matrix(paste0('R2_',nm,'_B_joint_rows'),as.matrix(out$V[[1]]),full,labs)
  centered <- scale(as.matrix(dat[labs]),center=TRUE,scale=FALSE)
  check(paste0('R2_',nm,'_independent_unit_oracle'),as.vector(crossprod(centered)/n^2),as.vector(full),numeric=TRUE)
  check(paste0('R2_',nm,'_margins'),c(rowSums(Q),colSums(Q)),c(40,60,30,70),numeric=TRUE)
  check(paste0('R2_',nm,'_cross_block_rows'),rowSums(cross),c(0,0),numeric=TRUE)
  check(paste0('R2_',nm,'_cross_block_columns'),colSums(cross),c(0,0),numeric=TRUE)
  check(paste0('R2_',nm,'_psd'),min(eigen(full,symmetric=TRUE,only.values=TRUE)$values)>=-abs_tol)
  target <- if(nm=='lower') c(-.0012,.0069,.0021) else c(.0018,.0009,.0081)
  d <- c(1,0,-1,0);s <- c(1,0,1,0)
  vals <- c(cross[1,1],drop(t(d)%*%full%*%d),drop(t(s)%*%full%*%s))
  check(paste0('R2_',nm,'_covariance_and_contrasts'),vals,target,numeric=TRUE)
  contrasts[[length(contrasts)+1L]] <- data.frame(joint_table=nm,cov_A1_B1=vals[1],var_A1_minus_B1=vals[2],var_A1_plus_B1=vals[3],within_A=.0024,within_B=.0021)
  tables[[length(tables)+1L]] <- data.frame(joint_table=nm,A=jointgrid$A,B=jointgrid$B,count=as.vector(Q),joint_probability=as.vector(Q)/n)
}
# C separately preserves each identified within-variable block.
a <- sm_estimate_from_counts(C_data(c(40,60)),family='multinomial',indicators=c('a','b'),vjt=TRUE,vst='none')
b <- sm_estimate_from_counts(C_data(c(30,70)),family='multinomial',indicators=c('a','b'),vjt=TRUE,vst='none')
record_matrix('R2_C_A_block',as.matrix(a$V[[1]]),.0024*matrix(c(1,-1,-1,1),2),c('A1','A2'))
record_matrix('R2_C_B_block',as.matrix(b$V[[1]]),.0021*matrix(c(1,-1,-1,1),2),c('B1','B2'))
combined <- C_data(c(40,60,30,70));combined$n_jt <- 100
expect_error('R2_one_C_for_two_variables',sm_estimate_from_counts(combined,family='multinomial',indicators=letters[1:4]),'sitemix_error_input_indicator_count','Concatenated counts sum to 2n and are not one categorical partition.')
D_data <- function(counts,n=100,id='d') {d <- C_data(counts,id);d$n_jt <- n;d}
D_est <- function(id,counts,n=100,relation='same_units',vjt=TRUE) run(id,sm_estimate_from_aggregates(D_data(counts,n,id),family='multivariate',indicators=letters[seq_along(counts)],vst='none',vjt=vjt,sampling_relation=relation,bias_correction=NULL,anscombe=FALSE))
D4 <- D_est('D1_four_marginals',c(40,60,30,70))
check('D1_does_not_preserve_known_C_blocks',as.matrix(D4$V[[1]])[1,2]==0 && as.matrix(a$V[[1]])[1,2]<0)
D2 <- D_est('D1_two_marginals',c(40,30))
F <- sm_frechet_envelope(D2,population_regime='d1a')
check('Frechet_R2_endpoints',c(F$raw_pairwise_intervals$pairwise_covariance_lower,F$raw_pairwise_intervals$pairwise_covariance_upper),c(-.0012,.0018),numeric=TRUE)
check('Frechet_K2_projection_identity',all(F$projection_diagnostics$projection_status=='identity_k_le_2'))
Db <- D_est('D1_boundary_marginal',c(0,30))
Fb <- sm_frechet_envelope(Db,population_regime='d1a')
check('Frechet_boundary_positive_surrogate',Db$se_raw[1]>0)
check('Frechet_boundary_rebuilt_diagonal',Fb$V_independence[[1]][1,1],0,numeric=TRUE)
check('Frechet_boundary_zero_covariance',c(Fb$raw_pairwise_intervals$pairwise_covariance_lower,Fb$raw_pairwise_intervals$pairwise_covariance_upper),c(0,0),numeric=TRUE)
check('Frechet_boundary_undefined_correlation',all(is.na(c(Fb$raw_pairwise_intervals$pairwise_correlation_lower,Fb$raw_pairwise_intervals$pairwise_correlation_upper))))
Du <- D_est('D1_unknown_units',c(40,30),relation='unknown')
expect_error('Frechet_unknown_units',sm_frechet_envelope(Du,population_regime='d1a'),'sitemix_error_invalid_population_regime','Equal denominators do not establish same observational units.')
supp <- data.frame(site_id='sup',year=2026L,indicator=c('a','b'),c_jt=c(NA,30),n_jt=c(100,100),suppression_flag=c(TRUE,FALSE))
Ds <- run('D1_suppressed_missing',sm_estimate_from_aggregates(supp,family='multivariate',vst='none',sampling_relation='same_units'))
check('Suppressed_not_zero',is.na(Ds$theta_raw[1]) && is.na(Ds$se_raw[1]) && Ds$estimate_status[1]=='suppressed_missing')
expect_error('Frechet_suppressed_missing',sm_frechet_envelope(Ds,population_regime='d1a'),'sitemix_error_invalid_indicators','Suppressed marginal is not an observed zero and blocks formal complete-marginal inference.')
expect_error('Suppression_sensitivity_raw_scale_rejected',sm_estimate_from_aggregates(supp,family='multivariate',vst='none',sampling_relation='same_units',suppression='upper_bound',suppression_sensitivity_acknowledge=TRUE),'sitemix_error_estimate_var_method','Current suppression upper-bound sensitivity requires unadjusted arcsine output.')
Dss <- run('D1_suppression_sensitivity',sm_estimate_from_aggregates(supp,family='multivariate',vst='arcsine',sampling_relation='same_units',suppression='upper_bound',suppression_sensitivity_acknowledge=TRUE))
check('Suppression_sensitivity_stays_missing',is.na(Dss$theta_raw[1]) && is.na(Dss$se_raw[1]) && Dss$estimate_status[1]=='suppression_sensitivity')
expect_error('Frechet_suppression_sensitivity',sm_frechet_envelope(Dss,population_regime='d1a'),'sitemix_error_suppression_sensitivity_excluded','Sensitivity fields are not identified marginals.')
# Regression for a margin of one; oracle calculated on the integer-count scale.
D1one <- D_est('D1_one_boundary',c(100,60))
Fone <- run('Frechet_one_boundary',sm_frechet_envelope(D1one,population_regime='d1a'))
o <- Fone$raw_pairwise_intervals
check('Frechet_one_lower_count_oracle',o$joint_probability_lower,max(0,100+60-100)/100,numeric=TRUE)
check('Frechet_one_upper_count_oracle',o$joint_probability_upper,min(100,60)/100,numeric=TRUE)
check('Frechet_one_covariance_zero',c(o$pairwise_covariance_lower,o$pairwise_covariance_upper),c(0,0),numeric=TRUE)
check('Frechet_one_print_success',length(capture.output(print(Fone)))>0)
check('Frechet_one_summary_success',inherits(summary(Fone),'summary.sm_frechet_envelope'))
# Independent counterexample: PSD plus pairwise Bernoulli feasibility is insufficient.
R <- matrix(-.5,3,3);diag(R)<-1
check('PSD_not_joint_feasibility_eigenvalues',sort(eigen(R,symmetric=TRUE,only.values=TRUE)$values),c(0,1.5,1.5),numeric=TRUE)
check('PSD_not_joint_feasibility_sum_variance',sum(.25*R),0,numeric=TRUE)
# S is integer with E(S)=1.5; Var(S)=0 is impossible. No solver/library used.
check_df <- do.call(rbind,checks)
write.csv(check_df,file.path(output,'categorical-checks.csv'),row.names=FALSE,na='')
write.csv(do.call(rbind,matrices),file.path(output,'categorical-matrices.csv'),row.names=FALSE,na='')
write.csv(do.call(rbind,scalars),file.path(output,'categorical-scalar-matrix.csv'),row.names=FALSE,na='')
write.csv(do.call(rbind,errors),file.path(output,'categorical-error-fixtures.csv'),row.names=FALSE,na='')
write.csv(do.call(rbind,tables),file.path(output,'categorical-joint-tables.csv'),row.names=FALSE,na='')
write.csv(do.call(rbind,contrasts),file.path(output,'categorical-contrasts.csv'),row.names=FALSE,na='')
write.csv(F$raw_pairwise_intervals,file.path(output,'categorical-frechet-endpoints.csv'),row.names=FALSE,na='')
write.csv(Fb$raw_pairwise_intervals,file.path(output,'categorical-frechet-boundary.csv'),row.names=FALSE,na='')
if(length(warnings_seen))write.csv(do.call(rbind,warnings_seen),file.path(output,'categorical-warnings.csv'),row.names=FALSE,na='')
capture.output(sessionInfo(),file=file.path(output,'categorical-session-info.log'))
identity <- jsonlite::fromJSON('qa/software-identity.json')
summary <- list(step='Step 3.2',status=if(all(check_df$status=='pass'))'pass' else 'FAIL',assertions=nrow(check_df),passed=sum(check_df$status=='pass'),failed=sum(check_df$status!='pass'),tolerance=list(absolute=abs_tol,relative=0,rank_eigen_threshold=abs_tol),max_numeric_absolute_error=max(check_df$max_abs_error,na.rm=TRUE),package_version=as.character(packageVersion('sitemix')),package_path=find.package('sitemix'),source_manifest_sha256=identity$source_manifest_sha256,public_commit=identity$public_source$commit,started_utc=format(started,tz='UTC',usetz=TRUE),ended_utc=format(Sys.time(),tz='UTC',usetz=TRUE),elapsed_seconds=as.numeric(difftime(Sys.time(),started,units='secs')),expected_error_fixtures=length(errors),warnings_recorded=length(warnings_seen),warning_note='One session-level D1 working-independence warning is captured and recorded; warnings are not treated as errors.',scope='Bounded independent categorical fixtures, not full test suite or performance/coverage claims.',r2=list(lower_cov=-.0012,upper_cov=.0018,contrast_difference_variance=c(.0069,.0009),contrast_sum_variance=c(.0021,.0081)),census_rank=list(counts=c(2,2,1),N=5,analytic_metadata=2,numeric_rank=0),AC_matrix_behavior='Scenario C interior allowed; boundary rejected')
jsonlite::write_json(summary,file.path(output,'categorical-summary.json'),pretty=TRUE,auto_unbox=TRUE,na='null')
cat('Categorical assertions:',nrow(check_df),'passed',sum(check_df$status=='pass'),'failed',sum(check_df$status!='pass'),'\n')
cat('Maximum numerical absolute error:',format(summary$max_numeric_absolute_error,digits=16),'\n')
if(any(check_df$status!='pass')) {print(check_df[check_df$status!='pass',]);quit(status=1)}
