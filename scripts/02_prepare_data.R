# Prepare clean openML data for experiments: 
# 1. select datasets
# 2. create modified versions with clear and unclear winners
# Rscript --vanilla 02_prepare_data.R [sel=all]
# sel = all, or classification,regression,nll,synthetic,classification cv,original or 1,2,3,4,5,6

args = commandArgs(trailingOnly = TRUE)
sel = if (length(args)>0) args[1] else "all"
source("../src/utils.R")
source("../src/synthetic_data.R")
DIR_DATA = "../data"
r = function(x) file.path(DIR_DATA, x)
DIR_DATA_PREPARED = r("prepared")
if (!dir.exists(DIR_DATA_PREPARED)) dir.create(DIR_DATA_PREPARED)

cat("Preparing openML data for experiments...\n")
# In each block, FILE_CLEAN is the input and FILE_PREPARED is the output.

if (sel %in% c("all", "classification", "1")) {
  FILE_CLEAN = r("clean/cls_zeroone.rds")
  FILE_PREPARED = r("prepared/cls_zeroone_prepared.rds") 
  
  clean = readRDS(FILE_CLEAN)
  clean = lapply(clean, function(x) {x[is.na(x)]=1; x}) # Assume NAs are mistakes (replace NAs with loss=1)

  # Create modified versions (clear_win and unclear_win) of some datasets 
  datasets = c("adult", "electricity", "kr-vs-kp", "spambase")
  del = 0.1 # difference between best and second best model is at least del.
  clear_win = lapply(clean[datasets], function(x) trim_winners(x, del))
  names(clear_win) = paste0(names(clear_win), "_clear_d0.1")
  unclear_win = lapply(datasets, function(d) keep_top_k(clean[[d]], ncol(clear_win[[paste0(d, "_clear_d0.1")]])))
  names(unclear_win) = paste0(datasets, "_unclear")
  
  data_list = c(clean, clear_win, unclear_win)
  saveRDS(data_list, FILE_PREPARED)
  cat("Saved binary classification data to", FILE_PREPARED, "\n")
}


if (sel %in% c("all", "regression", "2")) {
  FILE_CLEAN = r("clean/reg_mse.rds")
  FILE_PREPARED = r("prepared/reg_mse_prepared.rds") 
  datasets = c("cpu_small.fold0", "delta_elevators.fold0", "kin8nm.fold0", "topo_2_1.fold0")
  
  clean = readRDS(FILE_CLEAN)
  prepared = unlist(lapply(clean, function(x) split(x, x$fold)), recursive=FALSE)
  prepared = lapply(prepared, function(x) as.matrix(x[,-which(colnames(x) %in% c("fold", "row_id"))]))
  prepared = lapply(prepared, function(x) x[,order(colSums(x, na.rm=TRUE))])
  names(prepared) = gsub("\\.(\\d)$", ".fold\\1", names(prepared))
  saveRDS(prepared[datasets], FILE_PREPARED)
  cat("Saved regression data to", FILE_PREPARED, "\n")
}


if (sel %in% c("all", "logloss", "3")) {
  FILE_CLEAN = r("clean/cls_logloss.rds")
  FILE_PREPARED = r("prepared/cls_logloss_prepared.rds") 
  
  clean = readRDS(FILE_CLEAN)
  prepared = lapply(clean, clip_loss, M=1)
  prepared = lapply(prepared, function(X) X[,order(colSums(X, na.rm=TRUE))])
  names(prepared) = paste0(names(prepared), "_ll")
  saveRDS(prepared, FILE_PREPARED)
  cat("Saved logloss data to", FILE_PREPARED, "\n")
}

if (sel %in% c("all", "synthetic", "4")) {
  FILE_PREPARED = r("prepared/synthetic.rds")
  N = 1e5
  create_data = function(a1, a2, a3, r12s, r22s) createdata(n=N, a1=a1, a2=a2, a3=a3, r12=r12s*ranger12(a1,a2)[2], r22=r22s*ranger12(a1,a2)[2])
  grid = list(
    synth_5 = list(a1=.8, a2=.7, a3=.6, r12s=.5, r22s=.5)
  )
  grid_str = sapply(grid, function(x) paste(paste(names(x), x,sep="="), collapse=","))
  set.seed(2021)
  L_list = lapply(grid, function(...) do.call(create_data, ...))
  L_list = lapply(names(L_list), function(i) {attr(L_list[[i]], "params") = grid_str[i]; L_list[[i]]})
  names(L_list) = names(grid)
  saveRDS(L_list, FILE_PREPARED)
  cat("Saved synthetic data to", FILE_PREPARED, "\n")
}

if (sel %in% c("all", "classification_cv", "5")) {
  # Additional datasets for appendix.
  FILE_CLEAN = r("clean/cls_cv_zeroone.rds")
  FILE_PREPARED = r("prepared/cls_cv_zeroone_prepared.rds") 
  clean = readRDS(FILE_CLEAN)
  # Assume NAs are mistakes (replace NAs with loss=1)
  clean = lapply(clean, function(x) {x[is.na(x)]=1; x})
  prepared = unlist(lapply(clean, function(x) split(x, x$fold)), recursive=FALSE)
  prepared = lapply(prepared, function(x) as.matrix(x[,-which(colnames(x) %in% c("fold", "row_id"))]))
  prepared = lapply(prepared, function(x) x[,order(colSums(x, na.rm=TRUE))])
  names(prepared) = gsub("\\.(\\d)$", ".fold\\1", names(prepared))
  datasets = names(prepared)
  datasets = datasets[grep("fold0", names(prepared))]
  saveRDS(prepared[datasets], FILE_PREPARED)
  cat("Saved to", FILE_PREPARED, "\n")
}


if (sel %in% c("all", "original", "6")) {
  FILE_DATASETS = r("clean/cls_original_datasets.rds")
  FILE_PREPARED = r("prepared/cls_original_datasets_prepared.rds")
  datasets = c("adult", "electricity", "kr-vs-kp", "spambase")
  raw = readRDS(FILE_DATASETS)
  prepared = lapply(raw[datasets], function(d) prepare_openml_dataset(d))
  saveRDS(prepared, FILE_PREPARED)
  cat("Saved original datasets to", FILE_PREPARED, "\n")
}
