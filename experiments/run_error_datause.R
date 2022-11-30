# Rscript --vanilla run_error_datause.R [sel] [n_run=1000]
# sel can be classification/1 or synthetic/3.
source("../src/utils_figures.R")
source("../src/utils.R")
source("../src/algo.R")

args = commandArgs(trailingOnly = TRUE)
sel = if (length(args)>0) args[1] else 3
n_rep = if (length(args)>1) args[2] else 5000

n = c(16, 32, 64, 128, 192, 256, 384, 512)
T0 = c(16, 32, 64, 128, 256)
overwrite = FALSE
n_run = 100 # Number of times error rate should be estimated with n_rep simulations for the optimal case.
grid = list(T0=T0, alpha=.05, delta=c(.05, .1))

#' Column means on a subsample of size n
sample_colmeans = function(x, n) colMeans(x[sample(nrow(x), size=n),,drop=FALSE])

#' Approximate error rate when using n data points to find the best model
#' @param n 
#' @param data 
#' @param n_rep 
#' @param true_best 
sim_error_n = function(n, data, true_best, n_rep) {
  losses = replicate(n_rep, sample_colmeans(data, n))
  selected = apply(losses, 2, which.min)
  compute_error_rate(selected, true_best)
}
sim_ours = function(T0, alpha, delta, data, n_sim) replicate(n_sim, BSV(data[sample(nrow(data)),], alpha=alpha, delta=delta, T0=T0), simplify = FALSE)

if (sel %in% c("spambase", "1")) {
  FILE_PREPARED = "../data/prepared/cls_zeroone_prepared.rds" 
  dataset = "spambase"
} else if (sel %in% c("electricity", "2")) {
  FILE_PREPARED = "../data/prepared/cls_zeroone_prepared.rds" 
  dataset = "electricity"
} else if (sel  %in% c("synthetic", "3")) {
  FILE_PREPARED = "../data/prepared/synthetic.rds"
  dataset = "synth_5"
} else {
  stop(sprintf("Unknown sel (%s).", sel))
}

DIR_RESULTS = "../results/error_datause"
if (!dir.exists(DIR_RESULTS)) dir.create(DIR_RESULTS, recursive = TRUE)

# Run ####
data_all = readRDS(FILE_PREPARED)
data = data_all[[dataset]]

t_start = Sys.time()
# Optimal curve
cat(sprintf("Running error simulation %d times for n=%s.\n", n_run, paste0(n, collapse=",")))
FILE_OPT = file.path(DIR_RESULTS, sprintf("res_%s_opt.rds", dataset))
set.seed(2021)
n_rep_size = function(n) ifelse(n<200, 1000, 10000)
grid_opt = expand.grid(n=n, delta=grid$delta)
grid_opt$n_rep = n_rep_size(grid_opt$n)
# Use n_rep simulations to estimate error. Use n_run replications of the n_rep simulations to estimate deviation of the error.
run_opt = function(n, n_rep, delta) {
  true_best = get_best(data, delta)
  replicate(n_run, sim_error_n(n, data=data, true_best=true_best, n_rep=n_rep))
}
df_opt = run_if_missing_file({
  df_opt = run_f_on_grid(run_opt, grid_opt, verbose=TRUE)
  df_opt = unnest_sim_df(df_opt)
  df_opt$error = df_opt[,'do.call(cbind, res_new)']
  df_opt[,'do.call(cbind, res_new)'] = NULL
  df_opt
}, file=FILE_OPT, overwrite=overwrite)

# Ours on a grid
cat(sprintf("Running ours on a grid.\n"))
FILE_OURS = file.path(DIR_RESULTS, sprintf("res_%s_ours.rds", dataset))
set.seed(2021)
df_ours = run_if_missing_file({
  df_ours_raw=run_f_on_grid(function(...) sim_ours(..., data=data, n_sim=n_rep), grid, verbose=TRUE)
  df_ours = unnest_sim_df(df_ours_raw)
  df_ours = mutate(df_ours, true_best = map(delta, function(d) get_best(data, delta=d)))
  df_ours
}, file=FILE_OURS, overwrite=overwrite)


# Save ####
cat(sprintf("Combining results.\n"))
FILE_OUT = file.path(DIR_RESULTS, sprintf("res_%s.rds", dataset))
res = list(opt=df_opt, ours=df_ours)
saveRDS(res, FILE_OUT)
cat(sprintf("Saved to %s.\n", FILE_OUT))
t_end = Sys.time()
t_diff = t_end-t_start
cat(sprintf("Done. (%.2f %s)\n", t_diff, attr(t_diff, "units")))
