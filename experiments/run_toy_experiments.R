# Run toy experiments
# Rscript --vanilla run_toy_experiments.R [all|1|2|process] [n_run=1000]

suppressPackageStartupMessages({
  library(tidyr)
  library(dplyr)
})
source("../src/utils.R")
source("../src/algo.R")

args = commandArgs(trailingOnly = TRUE)
sel = if (length(args)>0) args[1] else "all"
n_run = if (length(args)>1) as.integer(args[2]) else 1000
DIR_RESULTS = "../results/toy"
FILE_OUT = file.path(DIR_RESULTS, "results_all_processed.rds")
overwrite = FALSE
process = TRUE
algo = BSV

# Functions ####

sample_binary = function(n, p0) sample(c(0,1), size=n, replace=TRUE, prob=c(p0, 1-p0))

#' Make clear winner case
#' 
#' @param n,k number of rows and columns
#' @param acc_max,acc_min max and min accuracies
#' @return (n,k) loss matrix with 1 best and k-1 non-best models
make_clear_winner = function(N, k, k_best=1, acc_max=0.9, acc_min=0.5) {
  if (k_best>=k)
    return(matrix(sample_binary(N*k, acc_max), nrow=N))
  matrix(c(sample_binary(N*k_best, acc_max), sample_binary(N*(k-k_best), acc_min)), nrow=N, ncol=k)
}

#' Make correlated binary matrix
#' 
#' Repeat binary vector k times, and randomly flip bits in each of the k vectors to reduce correlation.
#'
#' @param n 
#' @param k 
#' @param acc numeric vector of accuracies (Pr(x=0))
#' @param r "correlation"
#'
#' @return
#' @export
#'
#' @examples
make_correlated = function(N, k, acc, r) {
  if (length(acc) == 1)
    acc = rep(acc, k)
  if (missing("k"))
    k = length(acc)
  x1 = sample_binary(N, acc[1])
  L = matrix(rep(x1, k), nrow=N, ncol=k)
  m = floor(N * (1-r)) # Needs to take acc into account.
  for (j in 2:k) {
    idx = sample(N, size=m)
    L[idx,j] = sample_binary(m, acc[j])
  }
  L
}

#' Generate 0-1 losses for k models, where k_delta of them are within the best.
#'
#' Accuracies for the best and non-best are acc_max and acc_min respectively.
#'
#' @param N 
#' @param k 
#' @param k_delta 
#' @param acc_max 
#' @param acc_min 
#'
#' @return
#' @export
#'
#' @examples
make_k_delta = function(N, k, k_delta, acc_max, acc_min) {
  best = replicate(k_delta, sample_binary(N, acc_max))
  nonbest = replicate(k-k_delta, sample_binary(N, acc_min))
  if (length(nonbest)==0) best else cbind(best, nonbest)
}

#' Generate 0-1 losses for k models, where k_delta of them are good. 
#' Accuracies rise linearly from acc_min to acc_max.
#'
#' @param N 
#' @param k 
#' @param acc_max 
#' @param acc_min 
#'
#' @return
#' @export
#'
#' @examples
make_linear_acc = function(N, k, acc_max, acc_min) {
  acc = seq(acc_max, acc_min, length.out=k)
  sapply(acc, function(a) sample_binary(N, a))
}

#' Run toy experiment
#'
#' @param grid_data list
#' @param grid_algo list
#' @param make_data function
#' @param n_run 
#'
#' @return
#' @export
#'
#' @examples
run_toy = function(grid_data, grid_algo, make_data, n_run=1000, verbose=TRUE) {
  L_df = run_f_on_grid(make_data, grid_data) # Column res is the dataset
  L_df$dataset = summarize_param_df(L_df)
  L_list = L_df$res
  names(L_list) = L_df$dataset
  L_df$res = NULL
  
  grid_algo$dataset = L_df$dataset
  f = function(dataset, alpha, delta, T0) replicate(n_run, algo(L_list[[dataset]], T0=T0, alpha=alpha, delta=delta), simplify = FALSE)
  res = run_f_on_grid(f, grid_algo, verbose = verbose)
  df = merge(L_df, res, by="dataset")
  list(data = L_list,
       results = df
  )
}


#' Toy experiment 1
#' 
#' Generate 0-1 losses for k models, where there is one clear winner and k-1 "bad" models.
#' Run algo for various k. 
#' 
#' @param k vector of number of models
#' @param acc_max,acc_min 
#' @param n_run number of times to run algo on each dataset
#' @param N size of dataset
#' @return list(data,results) where data=list of data frames, results=list of algorithm results (list of best,pvalue,N) (not sure didn't test)
run_exp1 = function(k, k_best=1, N=1e4, acc_max, acc_min, T0=128, alpha=.1, delta=.1, n_run=100) {
  g=expand.grid(list(k=k, k_best=k_best, N=N, acc_max=acc_max, acc_min=acc_min), KEEP.OUT.ATTRS = FALSE)
  g=g[g$k_best <= g$k,]
  run_toy(grid_data=g, 
          grid_algo=list(T0=T0, alpha=alpha, delta=delta), 
          make_data=make_clear_winner, 
          n_run=n_run, verbose=TRUE)
}

#' Toy experiment 2
#' 
#' Generate 0-1 losses for k models, that have "correlation" r. 
#' Run algo for various r.
run_exp2 = function(r, k=100, N = 1000, acc=c(0.9, rep(0.7, k-1)), T0=128, alpha=.1, delta=.1, n_run=100) {
  if (!is.list(acc)) acc = list(acc)
  make_correlated_ = function(N, k, acc, r) if (is.list(acc)) make_correlated(N=N, k=k, acc=acc[[1]], r=r)
  run_toy(grid_data=list(r=r, k=k, N=N, acc=acc), 
          grid_algo=list(T0=T0, alpha=alpha, delta=delta), 
          make_data=make_correlated_, 
          n_run=n_run, verbose=TRUE)
}

#' Toy experiment 4 (incomplete)
#' 
#' Generate 0-1 losses for k models, where k_delta of them are good. 
#' Accuracies rise linearly from acc_min to acc_max.
run_exp4 = function(k_delta, k, N, acc_max=0.9, acc_min=0.5, alpha, T0, n_run=1000) {
  L = make_linear_acc(N=N, k=k, acc_max=acc_max, acc_min=acc_min) 
  k_delta = c(2,5,15)
  #delta = acc[k_delta] - min(acc)
  grid = list(k=k, acc_max=acc_max, acc_min=acc_min, alpha=alpha, delta=delta, T0=T0)
  run_sim = function(d) replicate(n_run, algo(L=L, T0=T0, alpha=alpha, delta=d), simplify = FALSE) 
  res = run_f_on_grid(run_sim, grid)
  list(data=L, 
       res=res)
}

# Run ####

if (!dir.exists(DIR_RESULTS))
  dir.create(DIR_RESULTS)
r = function(x) file.path(DIR_RESULTS, x)

cat(sprintf("Running toy experiments (%s)\n", sel))
t_start = Sys.time()
cat(sprintf("Start time: %s.\n", t_start))
cat(sprintf("Using n_run=%d.\n", n_run))
if (sel %in% c("all", "1")) {
  cat("Toy experiment 1.\n")
  FILE_EXP1 = r("toy_exp1.rds")
  set.seed(2021)
  exp1 = run_if_missing_file({
    run_exp1(k = 2^(1:7), 
             k_best = 2^(0:7),
             N=5000, 
             acc_max=c(0.9), 
             acc_min=0.2, 
             T0=128, 
             alpha=.05, 
             delta=.05, 
             n_run=n_run)}, 
    file=FILE_EXP1, overwrite=overwrite)
}

if (sel %in% c("all", "2")) {
  cat("Toy experiment 2.\n")
  FILE_EXP2 = r("toy_exp2.rds")
  
  set.seed(2021)
  exp2 = run_if_missing_file({
    run_exp2(r=seq(0.1, 1, 0.1), 
             k=c(10,100), 
             N=5000, 
             acc=c(0.9, rep(0.7, 100-1)),
             T0=c(64, 128, 256), 
             alpha=c(.05, .1), 
             delta=c(.01, .05), 
             n_run=n_run)}, 
    file=FILE_EXP2, overwrite=overwrite)
}


if (process) {
  cat("Processing results...\n")
  results_files = list.files(DIR_RESULTS, pattern = "toy.+\\.rds", full.names = TRUE)
  results_list = lapply(results_files, readRDS) 
  names(results_list) = gsub("\\.rds", "", basename(results_files))
  df_results = tibble(exp = names(results_list), 
                      sims = lapply(results_list, function(x) unnest_sim_df(x$results))) %>% 
    unnest_longer(sims) %>% 
    do.call(cbind, .)
  colnames(df_results) = gsub("^sims\\.", "", colnames(df_results))
  
  df_datasets = summarize_data(unlist(lapply(results_list, function(x) x$data), recursive = FALSE), 
                               deltas = unique(df_results$delta))
  df_datasets$dataset = gsub("^toy_exp[0-9]\\.", "", df_datasets$dataset)
  
  df = left_join(as_tibble(df_datasets), as_tibble(df_results), by=c("dataset", "delta", "N","k"))
  
  saveRDS(df, FILE_OUT)
  cat("Saved to", FILE_OUT, "\n")
}

t_end = Sys.time()
t_diff = t_end - t_start
cat(sprintf("Done. (%s, %.2f %s)\n", t_end, t_diff, attr(t_diff, "units")))
