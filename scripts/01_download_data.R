# Rscript --vanilla download_data.R
# Download and process data from openML: 
# - download model predictions for binary classification tasks, and save as 0-1 losses and log-losses.
# - download model predictions for regression tasks, and save as mean squared error (MSE) losses.
# - download the original data for the binary classification tasks.
#
# Terms:
# - openML task = ML task + dataset + evaluation method (e.g. classify iris and evaluate with cv)
# - openML run  = model instances applied to tasks (e.g. classify iris with WEKA's RandomForest using parameters x)

suppressPackageStartupMessages({
  library(OpenML)
  library(dplyr)
  library(tidyr)
  library(purrr)
})
source("../src/utils_data_download.R")
setOMLConfig(apikey = "c1994bdb7ecb3c6f3c8f3b35f4b47f1f") # Public read-only API key from the openML tutorial (https://docs.openml.org/R-guide/).

DIR_DATA = "../data"
fn = function(x) file.path(DIR_DATA, x)
if (!dir.exists(DIR_DATA)) {
  dir.create(DIR_DATA) 
  dir.create(file.path(DIR_DATA, "raw")) 
  dir.create(file.path(DIR_DATA, "clean"))
}
# Input files: openML tasks selected in `dataset_selection.R`
task_ids_cls_holdout = as.integer(readLines(fn("task_ids_cls_holdout.txt")))
task_ids_cls_10cv = as.integer(readLines(fn("task_ids_cls_10cv.txt")))
task_ids_reg_10cv = as.integer(readLines(fn("task_ids_reg_10cv.txt")))

# Output files (created if they don't exist): openML runs (raw and processed), and original datasets.
RAW_CLS_RUNS = fn("raw/raw_cls_runs.rds")   
RAW_CLS_TASKS = fn("raw/raw_cls_tasks.rds") 
RAW_REG_CV_RUNS = fn("raw/raw_reg_cv_runs.rds")
RAW_REG_CV_TASKS = fn("raw/raw_reg_cv_tasks.rds")
RAW_CLS_CV_TASKS = fn("raw/raw_cls_cv_tasks.rds") 
RAW_CLS_CV_RUNS = fn("raw/raw_cls_cv_runs.rds")   

CLEAN_CLS_ZEROONE = fn("clean/cls_zeroone.rds")
CLEAN_CLS_LOGLOSS = fn("clean/cls_logloss.rds")
CLEAN_CLS_DATASETS = fn("clean/cls_original_datasets.rds") # Original datasets (not runs or tasks).
CLEAN_REG_MSE = fn("clean/reg_mse.rds")
CLEAN_CLS_CV_ZEROONE = fn("clean/cls_cv_zeroone.rds")   


# Download model predictions ####

cat("Retrieving data from openML API (ETA ~2h)...\n")
cat("\n")
cat("Classification tasks\n")
cat("Downloading tasks... ")
tasks_cls_holdout = eval_and_save({get_tasks_list(task_ids_cls_holdout)}, RAW_CLS_TASKS, load = TRUE)
cat("Saving original datasets... ")
eval_and_save({extract_datasets(tasks_cls_holdout)}, CLEAN_CLS_DATASETS)
cat("Downloading runs (model predictions)... ") # About 790mb in memory. Saves ~144mb rds file.
runs_cls_holdout = eval_and_save({load_all_runs_safe(task_ids_cls_holdout, tmp_dir=fn("tmp/raw_tasks_cls_holdout"))}, RAW_CLS_RUNS, load=TRUE)
cat("Processing (computing 0-1 losses)... ") #<1MB on disk
eval_and_save({process_raw_runs_holdout_zeroone(runs_cls_holdout)}, CLEAN_CLS_ZEROONE)
cat("Processing (computing log-losses)... ")
eval_and_save({process_raw_runs_holdout_logloss(runs_cls_holdout)}, CLEAN_CLS_LOGLOSS)
cat("Downloading *cross-validated* tasks... ")
eval_and_save({get_tasks_list(task_ids_cls_10cv)}, RAW_CLS_CV_TASKS)
cat("Downloading *cross-validated* runs (ETA ~1h)... ") # 7.7Gb in memory, 1.1Gb on disk.
runs_cls_10cv = eval_and_save({load_all_runs_safe(task_ids_cls_10cv, tmp_dir=fn("tmp/raw_tasks_cls_10cv"))}, RAW_CLS_CV_RUNS, load=TRUE) 
cat("Processing (computing 0-1 losses)... ")
eval_and_save({process_raw_runs_10cv_zeroone(runs_cls_10cv)}, CLEAN_CLS_CV_ZEROONE)
cat("\n")
cat("Regression tasks\n")
cat("Downloading tasks... ")
eval_and_save({get_tasks_list(task_ids_reg_10cv)}, RAW_REG_CV_TASKS)
cat("Downloading model predictions... ")
runs_list_reg_10cv = eval_and_save({load_all_runs_safe(task_ids_reg_10cv, tmp_dir=fn("tmp/raw_tasks_reg_10cv"))}, RAW_REG_CV_RUNS, load=TRUE)
cat("Processing (computing squared error losses)...")
eval_and_save({process_raw_runs_10cv_mse(runs_list_reg_10cv)}, CLEAN_REG_MSE)

cat("Done.\n")
