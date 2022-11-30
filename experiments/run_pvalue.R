source("../src/synthetic_data.R")
source("../src/algo.R")
source("../src/utils.R")

DIR_RESULTS = "../results/pvalue"
FILE_DATA = "../data/prepared/synthetic.rds"
FILE_OUT = file.path(DIR_RESULTS, "pvalues.rds")
grid = list(alpha=.05, delta=c(.1, .05, .01), T0=c(128, 256))
overwrite = FALSE

if (!dir.exists(DIR_RESULTS))
  dir.create(DIR_RESULTS, recursive = TRUE)

L_list = readRDS(FILE_DATA)
L = L_list[["synth_5"]]
n_run = 1000
n_boot = 1000
get_pvalues = function(L, delta) sapply(1:ncol(L), function(i) boot_pvalue(L, best=i, n_boot=n_boot, upper=delta))
run_sim = function(T0, delta, alpha) replicate(n_run, get_pvalues(L[sample(nrow(L), T0),], delta), simplify = FALSE)

cat("Running p-values example. \n")
if (file.exists(FILE_OUT) & !overwrite) {
  cat(sprintf("File exists: %s\n", FILE_OUT))
} else {
  t_start = Sys.time()
  cat(sprintf("Start time: %s.\n", t_start))
  set.seed(2021)
  res = run_f_on_grid(run_sim, grid, verbose = TRUE)
  saveRDS(res, FILE_OUT)
  cat(sprintf("Saved to %s.\n", FILE_OUT))
  t_end = Sys.time()
  t_diff = t_end - t_start
  cat(sprintf("Done. (%s, %.2f %s)\n", t_end, t_diff, attr(t_diff, "units")))
}
