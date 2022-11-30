# Run the algorithm n_run times for every parameter combination in `params` and every dataset in FILE_PREPARED.
# Creates a result_{dataset}.rds file in DIR_RESULTS for every dataset in FILE_PREPARED (result of prepare_data.R).
# Rscript --vanilla run_param_tables.R [sel] [n_run=1000]
# sel can be classification,regression,logloss,classification_cv or 1,2,3,4,5 or an id 101-116, 201-204, 301-308, 501-520.

suppressPackageStartupMessages({
  library(tidyr)
  library(dplyr)
})
source("../src/utils.R")
source("../src/algo.R") 
args=commandArgs(trailingOnly = TRUE)

sel = if (length(args)>0) args[1] else "classification"
n_run = if (length(args)>1) as.integer(args[2]) else 1000
overwrite = FALSE
debug = FALSE
process = TRUE
algo = BSV

id = as.integer(sel)
if (!is.na(id) && id>=100) {
  id_task = id %/% 100
  id_dataset = id %% 100
} else {
  id_task = sel
}

if (sel %in% c("classification", "1") || id_task==1) {
  DIR_RESULTS = "../results/param_tables/classification"
  FILE_PREPARED = "../data/prepared/cls_zeroone_prepared.rds"
  params = list(alpha = c(.1, .05), delta = c(.1, .01), T0 = c(128, 512))
} else if (sel %in% c("regression", "2") || id_task==2) {
  DIR_RESULTS = "../results/param_tables/regression"
  FILE_PREPARED = "../data/prepared/reg_mse_prepared.rds" 
  params = list(alpha = c(.1, .05), T0 = c(64, 128))
  deltas = list(cpu_small = c(.1, 5),
                delta_elevators = c(2e-7, 1e-6),
                kin8nm = c(.01, .001),
                topo_2_1 = c(8e-5, 2e-4))
} else if (sel  %in% c("logloss", "3") || id_task==3) {
  DIR_RESULTS = "../results/param_tables/classification_logloss"
  FILE_PREPARED = "../data/prepared/cls_logloss_prepared.rds"
  params = list(alpha = c(.1, .05), delta = c(.1, .01), T0 = c(128, 256))
} else if (sel  %in% c("classification_cv", "5") || id_task==5) {
  DIR_RESULTS = "../results/param_tables/classification_cv"
  FILE_PREPARED = "../data/prepared/cls_cv_zeroone_prepared.rds"
  params = list(alpha = c(.05), delta = c(.01, .05), T0 = c(128, 256))
} else {
  stop(sprintf("Unknown sel (%s).", sel))
}

if (debug) {
  n_run = 2
  DIR_RESULTS = "../results/tmp"
}

if (!dir.exists(DIR_RESULTS)) 
  dir.create(DIR_RESULTS, recursive=TRUE)
L_list = readRDS(FILE_PREPARED) # List of loss matrices.
datasets = names(L_list)
if (exists("id_dataset"))
  datasets = datasets[id_dataset]

# Customize parameters per dataset.
customize_params = function(d) {
  p = params
  p$T0 = p$T0[p$T0 <= nrow(L_list[[d]])]
  # if (d=="adult") p$T0 = p$T0[p$T0>64]
  d_base = gsub("\\.fold\\d$", "", d)
  if (exists("deltas") && !is.null(deltas[[d_base]]))
    p$delta = deltas[[d_base]]
  p
}
param_list = lapply(datasets, customize_params)
names(param_list) = datasets

algo_timed = make_timeable(algo)
#' Simulate runs of the algorithm
#'
#' @param L matrix, where L[i,j] is the loss of model j on data point i.
#' @return list of n_run runs of the algorithm
run_sim = function(L, alpha, delta, T0, n_run) {
  replicate(n_run, 
            algo_timed(L[sample(nrow(L)),], T0=T0, alpha=alpha, delta=delta), 
            simplify = FALSE)
}

t_start = Sys.time()
cat(sprintf("Start time: %s.\n", t_start))
cat(sprintf("Experiment: '%s'.\n", sel))
cat(sprintf("Using n_run=%d.\n", n_run))
for (i in seq_along(datasets)) {
  name = datasets[i]
  cat(sprintf("Dataset: %s (%d/%d)\n", name, i, length(datasets)))
  L = L_list[[name]]
  grid_L = param_list[[name]]
  run_sim_L = function(...) run_sim(L=L, n_run=n_run, ...)
  FILE_RESULT = file.path(DIR_RESULTS, sprintf("result_%s.rds", name))
  set.seed(2021)
  df_sim = run_if_missing_file({
    run_f_on_grid(f = run_sim_L, grid = grid_L, verbose = TRUE)
  }, file=FILE_RESULT, overwrite=overwrite)
}

if (process) {
  cat("Processing results...\n")
  FILE_OUT = file.path(DIR_RESULTS, "results_all_processed.rds")
  results_files = list.files(DIR_RESULTS, pattern = "result_.+\\.rds", full.names = TRUE)
  results_list = lapply(results_files, readRDS) 
  names(results_list) = gsub("\\.rds|result_", "", basename(results_files))
  df_results = tibble(dataset = names(results_list), 
                      sims = lapply(results_list, unnest_sim_df)) %>% 
    unnest_longer(sims) %>% 
    do.call(cbind, .)
  colnames(df_results) = gsub("^sims\\.", "", colnames(df_results))
  
  df_datasets = summarize_data(L_list, deltas = unique(df_results$delta))
  df = left_join(as_tibble(df_datasets), as_tibble(df_results), by=c("dataset", "delta"))
  
  saveRDS(df, FILE_OUT)
  cat("Saved to", FILE_OUT, "\n")
}

cat("\nPrinting warnings (if any).\n")
options(nwarnings = 10000)  
warnings()

t_end = Sys.time()
t_diff = t_end - t_start
cat(sprintf("Done. (%s, %.2f %s)\n", t_end, t_diff, attr(t_diff, "units")))
