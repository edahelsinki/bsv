#' Bootstrap validation (BSV) algorithm
#' 
#' Find the best model in L using a bootstrap p-value.
#'
#' @param L loss matrix (n,k), where L_ij = loss of model j on data point i
#' @param T0 number of data points to use for making the initial guess for the best model
#' @param alpha 
#' @param delta 
#'
#' @return
#' @export
#'
#' @examples
BSV = function(L, alpha, delta, T0=get_T0(a=alpha, d=delta, k=ncol(L))) {
  Nmax = nrow(L)
  if (T0>floor(Nmax/2)) {
    warning(sprintf("T0>nrow(L)/2. Setting T0=nrow(L)/2. (%d->%d)", T0, floor(Nmax/2)))
    T0 = floor(Nmax/2)
  }
  t = 0L
  while (TRUE) {
    t = t+1
    alpha_t = alpha*2^(-t)
    candidate = which.pmin(colSums(L[1:T0,,drop = FALSE]))
    T1 = min(Nmax-T0, T0)
    L1 = L[(T0+1):(T0+T1),,drop=FALSE]
    pvalue = boot_pvalue(L1, best=candidate, n_boot=10/alpha_t, upper=delta)
    T0 = T0+T1
    if (pvalue <= alpha_t | T0>=Nmax) 
      break
  }
  if (pvalue > alpha_t) {
    warning(sprintf("pvalue>alpha_t (%.2f>%.2f)", pvalue, alpha_t))
  }
  list(candidate=candidate, 
       pvalue=pvalue,
       n=T0, 
       t=t)
}

## P-values

#' Bootstrap p-value for the statistic `S[j] = l[j] - min(l[-j])`
#' 
#' A p-value p is computed by inverting a basic bootstrap confidence interval 
#' [lower, upper], which is adapted from Eq. 6.9 in Shalizi2020. 
#' The standard error of p is computed using jackknife-after-bootstrap.
#'
#' Advanced Data Analysis from an Elementary Point of View. Shalizi, C. R. (2021).
#' 
#' @param L matrix
#' @param best column index in L for which to compute S
#' @param n_boot number of bootstrap samples to estimate p-value
#' @param size number of bootstrap samples to estimate statistic
#' @param lower,upper boundaries of bootstrap confidence interval
#' @param N_min minimum allowable size of L
#'
#' @return pvalue and its standard error (p, sd(p))
#' @export
#'
#' @examples
boot_pvalue = function(L, best, size=nrow(L), lower=-Inf, upper=Inf, n_boot=1000, N_min=16L) {
  if (nrow(L)<N_min) {
    warning(sprintf("L contains fewer than %d data points. Returning pvalue=1.", N_min))
    return(1)
  }
  pval = function(r) (0.5+sum(r))/(1+length(r)) # mid-p pvalue
  x_counts_boot = replicate(n_boot, boot_counts(nrow(L), size=size))
  s_boot = apply(x_counts_boot, 2, function(i) get_stat_boot(L=L, best=best, i_count=i))
  s_obs = get_stat(l=colMeans(L), j=best)
  r = 1*(s_boot>2*s_obs-lower | s_boot<2*s_obs-upper) + .5*(s_boot==2*s_obs-lower | s_boot==2*s_obs-upper) 
  pval(r)
}

#' Compute statistic
#'
#' @param l numeric vector. colMeans(L), i.e. avg losses.
#' @param j index in l, l[j] and l[-j] should work.
get_stat = function(l, j) {
  l[j] - min(l[-j])
}

#' Compute statistic on bootstrap sample
#' 
#' The same as `get_stat(colMeans(L[sample.int(nrow(L), size, replace=TRUE),]), best)` but faster.
#'
#' @param L matrix
#' @param best column index of L
#' @param i_count integer vector where i_count[j] = number of times L[i,] appears in bootstrap sample. Default is equivalent to get_stat(colMeans(L),best).
#'
#' @return compute get_stat on bootstrap sample of L
#' @export
#'
#' @examples
get_stat_boot = function(L, best, i_count = rep(1, nrow(L))) {
  L_colmeans = c(i_count %*% L) / sum(i_count)
  get_stat(L_colmeans, best)
}

#' Sample column means on bootstraps of `size` points
#' The same as `colMeans(L[sample.int(nrow(L), size, replace=TRUE),])` but faster.
#' NOTE: To ignore NAs in L, change them to zeroes, e.g. by doing L[is.na(L)]=0 
#' in the parent function. The `idx %*% L` matrix multiplication can't ignore NAs.
#' @param L matrix
#' @param size integer, number of bootstrap samples
sample_boot_colmeans = function(L, size=nrow(L)) {
  idx = boot_counts(n=nrow(L), size=size)
  c(idx %*% L) / sum(idx)
}

#' Compute bootstrap counts for data items
#'
#' @param n number of items to sample from
#' @param size number of bootstrap samples
#' @return vector x of length n, where x[i] is the number of times item i was chosen.
boot_counts <- function(n, size = n) {
  tabulate(sample.int(n, size = size, replace = TRUE), nbins = n)
}

#' Probabilistic version of which.min which resolves ties in random.
which.pmin <- function(x) {
  a <- unname(which(x==min(x)))
  if(length(a)==1) a else sample(a,size=1)
}

#' Get conservative bound for T0
#' 
#' Eq. (8) in the manuscript.
#' 
#' @param a alpha
#' @param d delta
#' @param k number of models
#' @param Lmax maximum value of loss. Default is 1 for 0-1 loss.
get_T0 = function(a, d, k, Lmax=1) {
  round( (-log(a) + log(k-1)) / (d/Lmax) )
}
