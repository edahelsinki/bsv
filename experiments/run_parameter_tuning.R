# Parameter tuning experiment
# Rscript --vanilla run_parameter_tuning.R [dataset]
# - Create a split (trva-test=20-80).
# - Run cv.glmnet on trva.
# - For every lambda checked by cv.glmnet, train a glmnet model on tr.
# - For every trained model, compute the 0-1 loss on the whole data.
# - For all parameters in `grid`, run our algo on va `n_rep` times.
suppressPackageStartupMessages({
  library(glmnet)
})
source("../src/algo.R")
source("../src/utils.R")

args = commandArgs(trailingOnly = TRUE)
datasets = if (length(args)>0) datasets = strsplit(args[1], ",")[[1]] else c("adult", "electricity", "kr-vs-kp", "spambase")
if (suppressWarnings(!any(is.na(as.integer(datasets))))) {
  # If datasets is a list of integers, then select from these datasets:
  datasets = c("adult", "electricity", "kr-vs-kp", "spambase")[as.integer(datasets)]
}
algo = BSV

DIR_RESULTS = "../results/parameter_tuning"
n_rep = 1000
overwrite = FALSE
verbose = TRUE
debug = FALSE
data_list = readRDS("../data/prepared/cls_original_datasets_prepared.rds")
grid = list(T0=c(64, 128, 256), alpha=c(.1, .05), delta=c(.1, .01))

# Functions ####

#' Balanced split
#'
#' @param x vector with two unique values
#' @param p_tr 
#'
#' @return vector of length(x)*p indices of x, randomly sampled to have the same balance as x.
#' @export
#'
#' @examples
split_balanced = function(x, p=.5) {
  labels = unique(x)
  x1 = which(x==labels[1])
  x2 = which(x==labels[2])
  c(sample(x1, floor(length(x1)*p)),
    sample(x2, floor(length(x2)*p)))
}

#' Train cv.glmnet
#'
#' @param X data as matrix or data frame
#' @param y target variable as a vector or as a column of X
#'
#' @return list with model losses L (where model = glmnet with some lambda), lambdas, 
#' @export
#'
#' @examples
train_cvglmnet = function(X, y, ...) {
  if (length(y)==1) {
    j = if (is.character(y)) which(colnames(X)==y) else y
    y = X[,j]
    X = X[,-j]
  }
  X = as.matrix(X)
  cv.glmnet(X, y, family = "binomial", ...)
}

#' Train many glmnets using lambda in lambdas
#' Note: not cv.glmnet but glmnet.
#'
#' @param X matrix or data frame
#' @param y target variable as a vector or as a column of X
#' @param lambdas numeric vector of lambdas for glmnet
train_glmnet_lambdas = function(X, y, lambdas) {
  if (length(y)==1) {
    j = if (is.character(y)) which(colnames(X)==y) else y
    y = X[,j]
    X = X[,-j]
  }
  X = as.matrix(X)
  models = lapply(lambdas, function(l) glmnet(X, y, family="binomial", lambda=l))
  names(models) = paste0("l=", formatC(lambdas, 3))
  attr(models, "lambdas") = lambdas
  models
}

#' Compute loss matrix for glmnet
#' Note: if a model returns a NA prediction, it is replaced by 1 (i.e. NAs are assumed to be wrong classifications).
#'
#' @param X matrix or data frame
#' @param y target variable as a vector or as a column of X
#' @param models list of glmnet models
compute_loss_matrix = function(X, y, models) {
  pred_glmnet = function(model, newdata) as.integer(predict(model, newx=newdata, type="class")[,1])
  if (length(y)==1) {
    j = if (is.character(y)) which(colnames(X)==y) else y
    y = X[,j]
    X = X[,-j]
  }
  X = as.matrix(X)
  lambdas = sapply(models, function(m) m$lambda)
  L_list = lapply(models, function(m) as.integer(pred_glmnet(m, X) != y))
  names(L_list) = names(models)
  L = do.call(function(x) matrix(x, ncol=length(L_list)), list(unlist(L_list)))
  L[is.na(L)] = 1
  colnames(L) = names(L_list)
  attr(L, "lambdas") = lambdas
  L
}


# Run ####
if (debug) {
  n_rep=2
  grid=list(T0=c(128), alpha=c(.1), delta=c(.1))
}
if (!dir.exists(DIR_RESULTS)) dir.create(DIR_RESULTS, recursive=TRUE)
r = function(x) file.path(DIR_RESULTS, x)
t_start = Sys.time()
cat("Running parameter tuning example.\n")
cat(sprintf("Start time: %s.\n", t_start))
cat(sprintf("Datasets: %s.\n", paste0(datasets, collapse=",")))
for (i in seq_along(datasets)) {
  name = datasets[i]
  data = data_list[[name]]$data
  y_name = data_list[[name]]$target_feature
  cat(sprintf("%d/%d Dataset '%s'\n", i, length(datasets), name))
  FILE_OUT = r(paste0(name, "_results_processed.rds"))
  if (file.exists(FILE_OUT) & !overwrite) {
    cat(sprintf("File exists: %s\n", FILE_OUT))
  } else {
    # Make split (i_tr, i_te, i_va)
    set.seed(2021)
    i_te = split_balanced(data[,y_name], p=.8)
    i_trva = setdiff(1:nrow(data), i_te)
    i_tr = i_trva[split_balanced(data[i_trva, y_name], p=.5)] 
    i_va = setdiff(1:nrow(data), c(i_te, i_tr))
    
    # Run cv.glmnet on training+validation set.
    cat(sprintf("Training cv.glmnet...\n"))
    model_cv = train_cvglmnet(data[-i_te,], y_name)
    lambdas = model_cv$lambda
    
    # Train glmnet for lambdas from v.glmnet and compute losses on va and te.
    cat(sprintf("Training %d glmnet models...\n", length(lambdas)))
    models_lambdas = train_glmnet_lambdas(data[i_tr,,drop=FALSE], y_name, lambdas)
    losses = compute_loss_matrix(data, y_name, models_lambdas)
    
    # Run our algo with various params n_rep times
    cat(sprintf("Running ours %d times...\n", n_rep))
    run_ours = function(T0, alpha, delta, n=n_rep) replicate(n=n, algo(L=losses[sample(i_va), ], T0=T0, alpha=alpha, delta=delta), simplify = FALSE)
    set.seed(2021) # Splits are fixed, but bootstrap is random.
    res_ours = run_f_on_grid(f = run_ours, grid = grid, verbose = TRUE)
    
    cat("Processing results...\n")
    out = list(split=list(i_tr=i_tr, i_va=i_va, i_te=i_te), 
               model_cv=model_cv,
               lambdas=lambdas,
               models_lambdas=models_lambdas,
               losses=losses,
               ours=res_ours)
    saveRDS(out, FILE_OUT)
    cat(sprintf("Saved to %s.", FILE_OUT))
  }
}
cat("\nPrinting warnings (if any).\n")
options(nwarnings = 10000)  
warnings()

t_end = Sys.time()
t_diff = t_end - t_start
cat(sprintf("End time: %s.\n", t_end))
cat(sprintf("Done. (%.2f %s)\n", t_diff, attr(t_diff, "units")))
