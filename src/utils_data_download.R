suppressPackageStartupMessages({
  library(OpenML)
  library(dplyr)
  library(tidyr)
  library(purrr)
})

#' Evaluate expression and save to file.
#'
#' @param expr expression that can be fed to eval()
#' @param file file to save to
#' @param load if TRUE and file exists, then loads the file and returns it.
#' @return invisibly returns file or NULL. Saves the result to expr to file, if it doesn't exist
eval_and_save <- function(expr, file, load=FALSE) {
  if (file.exists(file)) {
    cat("File exists:", file)
    out = NULL
    if (load) {
      out = readRDS(file)
      cat(" (loaded).")
    }
    cat("\n")
  } else {
    out = eval(expr)
    saveRDS(out, file)
    cat("Saved to", file, "\n")
  }
  return(invisible(out))
}

#' Get list of tasks from task_ids
#'
#' @param task_ids integer vector
#'
#' @return
#' @export
#'
#' @examples
get_tasks_list = function(task_ids) {
  tasks_list = lapply(task_ids, getOMLTask)
  names(tasks_list) = task_ids
  tasks_list
}

#' Extract datasets from list of openML tasks
#'
#' @param tasks_list list of openML tasks, see get_tasks_list()
extract_datasets = function(tasks_list) {
  data_list = lapply(tasks_list, function(x) x$input$data.set)
  names(data_list) = sapply(data_list, function(x) x$desc$name)
  data_list
}

#' Download all runs for an openML task
#'
#' @param task_id openML task id (integer). can be vector.
#' @param limit max number of runs to download per task_id
#'
#' @return list of runs for openML tasks.
#' @export
#'
#' @examples
load_all_runs = function(task_id, limit=5000) {
  bad_runs = c(501827, 501834) # Runs that fail to load from openML API.
  if (length(task_id)==1) {
    runs = listOMLRuns(task_id)
  } else {
    runs_raw = lapply(task_id, listOMLRuns, limit=limit)
    runs = do.call(rbind, runs_raw)
  }
  runs = runs[!runs$run.id %in% bad_runs,,drop=FALSE]
  runs_list = lapply(runs$run.id, getOMLRun) 
  names(runs_list) = sapply(runs_list, function(x) x$run.id)
  runs_list
}


#' Safer version of load_all_runs() that saves intermediate results for each task_id.
#'
#' @param task_ids integer vector of openML task IDs
#' @param tmp_dir 
#'
#' @return list of runs for every task in task_ids
#' @export
#'
#' @examples
load_all_runs_safe = function(task_ids, tmp_dir="tmp") {
  if (!dir.exists(tmp_dir)) dir.create(tmp_dir, recursive=TRUE)
  runs_list = vector("list", length(task_ids))
  filenames = file.path(tmp_dir, paste0("raw_runs_task_", task_ids, ".rds"))
  for (i in seq_along(task_ids)) {
    f_name = filenames[i]
    if (!file.exists(f_name)) {
      runs = load_all_runs(task_ids[i])
      saveRDS(runs, f_name)
      runs_list[[i]] = runs
    } else {
      runs_list[[i]] = readRDS(f_name)
    }
  }
  unlist(runs_list, recursive=FALSE) # Unnest from task_id_list(run_id_list()) to run_id_list(). "Ungroup".
}

#' Make loss matrix from data frame of losses of m models on n data points
#'
#' @param df data.frame of m rows, where dataset=char, model=char, loss=list column of int(n), row_id=list column of int(n).
#'
#' @return matrix (n,m)
#' @export
#'
#' @examples
make_loss_matrix = function(df) {
  # L is a list of m data frames, with columns model, row_id, loss.
  L = mapply(data.frame, model = df$model, row_id = df$row_id, loss = df$loss, SIMPLIFY = FALSE)
  # df_long: dataframe of n*m rows, with columns model, row_id, loss
  df_long = do.call(rbind, L)
  # df_wide: dataframe of n rows, with columns row_id, model1_losses, ..., modelm_losses.
  df_wide = pivot_wider(df_long, c("row_id"), names_from = "model", values_from = "loss")
  mat = as.matrix(df_wide[,-which(colnames(df_wide)=="row_id")])
  rownames(mat) = df_wide$row_id
  mat[,order(colMeans(mat, na.rm=TRUE))]
}

#' Make loss matrix from data frame of cross-validated losses.
make_loss_matrix_cv = function(df) {
  sort_by_colsum = function(df) df[,order(colSums(df, na.rm = TRUE))]
  df = mutate(df, 
              P = pmap(
                .f = data.frame, 
                list(model = model, fold = fold,row_id = row_id,loss = loss)))
  df_long = do.call(rbind, df$P)
  df_wide = pivot_wider(df_long, c("fold", "row_id"), names_from = "model", values_from = "loss")
  id_cols = which(colnames(df_wide) %in% c("fold", "row_id"))
  cbind(df_wide[,id_cols], sort_by_colsum(df_wide[,-id_cols]))
}

#' Process raw runs of binary classification into list of loss matrices
#' 
#' @param runs_list list of openML runs of Supervised Classification tasks with number.of.classes=2 and estimation.procedure="33% Holdout set"
#' 
#' @return list of loss matrices. A loss matrix L is L[i,j] = loss of model j on data point i. 
# Here all models are binary classifiers, so the loss is 0-1 loss, which means 
# L[i,j] = 0 if model j correctly classifies point i, else L[i,j]=1.
process_raw_runs_holdout_zeroone = function(runs_list) {
  zeroone_error = function(y, yhat) as.integer(y!=yhat)
  runs_df = tibble(runs = runs_list) %>% 
    hoist(runs, "run.id", "task.id", "input.data", "flow.id", "flow.name", "setup.id", "setup.string", "predictions") %>% 
    hoist(input.data, "datasets") %>% 
    unnest_wider(datasets) %>% 
    # Note: flow.id and setup.id are dropped here.
    select(run.id, name, flow.name, setup.string, predictions) %>% 
    # Dropping identical runs (i.e. same model, setup, dataset, predictions).
    distinct(name, flow.name, setup.string, predictions) %>% 
    # Combine model name and setup into one column.
    mutate(dataset=name, model=paste0(flow.name, "||", setup.string)) %>% 
    select(dataset, model, predictions) %>% 
    unnest_wider(predictions) %>% 
    # Filtering out runs with no ground truth labels (`correct`). 
    filter(!sapply(correct, is.null)) 
  
  # Compute losses
  runs_df = runs_df %>% 
    mutate(loss = map2(prediction, correct, zeroone_error)) %>% 
    mutate(loss = map2(loss, row_id, setNames)) %>% 
    # Dropping prediction confidence, because it is named `confidence_{CLASSNAME}` and CLASSNAME is different for each dataset.
    # Keeping only loss and setting its names to row_ids.
    select(dataset, model, loss, row_id) %>% 
    # If more than one set of predictions of a model on a dataset, keep the first one. 
    distinct(dataset, dataset, model, .keep_all = TRUE) 
  
  # Make loss matrices
  runs_split = split(runs_df, factor(runs_df$dataset))
  runs_split = runs_split[sapply(runs_split, nrow)>1] # Keep only datasets with more than one model.
  runs_clean = lapply(runs_split, make_loss_matrix) #~20MB in memory
  runs_clean
}

#' Process raw runs of binary classification into list of loss matrices
#'
#' @param runs_list 
#'
#' @return list of matrices
process_raw_runs_holdout_logloss = function(runs_list) {
  ground_truth_columns = c("correct", "truth")
  #' Add negative log-likelihood as a column
  #' 
  #' @param pred data frame. Has columns correct (or truth), confidence.[label0], confidence.[label1].
  #' @param ground_truth potential column names for column with ground truth labels.
  #' @param rm_orig_cols remove confidence columns
  add_nll = function(pred, ground_truth=ground_truth_columns, rm_orig_cols=TRUE) {
    ground_truth = ground_truth[ground_truth %in% colnames(pred)]
    labels = unique(pred[,ground_truth])
    nll = ifelse(pred[,ground_truth]==labels[1], 
                 -log10(pred[,paste0("confidence.", labels[1])]), 
                 -log10(pred[,paste0("confidence.", labels[2])]))
    pred$nll = nll
    if (rm_orig_cols) 
      pred[,paste0("confidence.", labels)]=NULL
    pred
  }
  
  runs_df = tibble(runs = runs_list) %>% 
    hoist(runs, "run.id", "task.id", "input.data", "flow.id", "flow.name", "setup.id", "setup.string", "predictions") %>% 
    hoist(input.data, "datasets") %>% 
    unnest_wider(datasets) %>% 
    # Note: flow.id and setup.id are dropped here.
    select(run.id, name, flow.name, setup.string, predictions) %>% 
    # Dropping identical runs (i.e. same model, setup, dataset, predictions).
    distinct(name, flow.name, setup.string, predictions) %>% 
    # Combine model name and setup into one column.
    mutate(dataset=name, model=paste0(flow.name, "||", setup.string)) %>% 
    select(dataset, model, predictions) %>% 
    # Remove model predictions for which there is no ground truth.
    filter(map_lgl(predictions, function(P) any(ground_truth_columns %in% names(P)))) %>% 
    mutate(predictions = map(predictions, add_nll)) %>% 
    unnest_wider(predictions) %>% 
    # Filtering out runs with no ground truth labels (`correct`). 
    filter(!sapply(correct, is.null)) 
  
  runs_df = runs_df %>% 
    mutate(loss = nll) %>% 
    mutate(loss = map2(loss, row_id, setNames)) %>% 
    select(dataset, model, loss, row_id) %>% 
    # If more than one set of predictions of a model on a dataset, keep the first one. 
    distinct(dataset, dataset, model, .keep_all = TRUE) %>% 
    # Only keep models that dont result in Inf negative log likelihood.
    filter(!sapply(loss, function(x) any(is.infinite(x))))
  
  runs_split = split(runs_df, factor(runs_df$dataset))
  runs_split = runs_split[sapply(runs_split, nrow)>1] # Keep only datasets with more than one model.
  
  runs_clean = lapply(runs_split, make_loss_matrix)
  runs_clean
}

#' Process raw runs of cross-validated regression into list of loss matrices
#'
#' @param raw list of openML runs of cross-validated regression
#'
#' @return
#' @export
#'
#' @examples
process_raw_runs_10cv_mse = function(raw) {
  sqerror = function(y,yhat) (y-yhat)^2
  cv_df =
    tibble(runs = raw) %>% 
    hoist(runs, "run.id", "task.id", "input.data", "flow.id", "flow.name", 
          "setup.id", "setup.string", "predictions") %>% 
    select(-runs) %>% 
    hoist(input.data, "datasets") %>% 
    unnest_wider(datasets) %>% 
    # Note: flow.id and setup.id are dropped here.
    select(run.id, name, flow.name, setup.string, predictions) %>% 
    # Dropping identical runs (i.e. same model, setup, dataset, predictions).
    distinct(name, flow.name, setup.string, predictions) %>% 
    # Combine model name and setup into one column.
    mutate(dataset=name, model=paste0(flow.name, "||", setup.string)) %>% 
    select(dataset, model, predictions) %>% 
    unnest_wider(predictions) %>% 
    # Combine columns with ground truth (`correct` and `truth`).
    mutate(correct = map2(correct, truth, function(x,y) if(!is.null(x)) x else y)) %>% 
    select(-truth) %>% 
    # Filtering out runs with no ground truth labels. 
    filter(!sapply(correct, is.null)) %>% 
    arrange(dataset, model)
  
  cv_df = cv_df %>%
    # Not using prediction confidence, because it is named `confidence_{CLASSNAME}` and CLASSNAME is different for each dataset.
    mutate(loss = map2(correct, prediction, sqerror)) %>% 
    mutate(loss = map2(loss, row_id, setNames)) %>% 
    select(dataset, model, loss, fold, row_id) %>% 
    # If more than one set of predictions of a model on a dataset, keep the first one. 
    distinct(dataset, model, .keep_all = TRUE) 
  
  cv_split = split(cv_df, factor(cv_df$dataset))
  
  clean = lapply(cv_split, make_loss_matrix_cv)
  clean 
}

#' Process raw runs of 10-fold cross-validated classification tasks
#'
#' @param raw list of openML runs with 10-fold CV
#'
#' @return list of loss matrices
#' @export
#'
#' @examples
process_raw_runs_10cv_zeroone = function(raw) {
  zeroone_error = function(y, yhat) as.integer(y!=yhat)
  cv_df =
    tibble(runs = raw) %>% 
    hoist(runs, "run.id", "task.id", "input.data", "flow.id", "flow.name", 
          "setup.id", "setup.string", "predictions") %>% 
    select(-runs) %>% 
    hoist(input.data, "datasets") %>% 
    unnest_wider(datasets) %>% 
    # Note: flow.id and setup.id are dropped here.
    select(run.id, name, flow.name, setup.string, predictions) %>% 
    # Dropping identical runs (i.e. same model, setup, dataset, predictions).
    distinct(name, flow.name, setup.string, predictions) %>% 
    # Combine model name and setup into one column.
    mutate(dataset=name, model=paste0(flow.name, "||", setup.string)) %>% 
    select(dataset, model, predictions) %>% 
    unnest_wider(predictions) %>% 
    # Combine columns with ground truth (`correct` and `truth`).
    mutate(correct = map2(correct, truth, function(x,y) if(!is.null(x)) x else y)) %>% 
    select(-truth) %>% 
    # Filtering out runs with no ground truth labels. 
    filter(!sapply(correct, is.null)) %>% 
    arrange(dataset, model)
  
  cv_df = cv_df %>%
    mutate(loss = map2(correct, prediction, zeroone_error)) %>% 
    mutate(loss = map2(loss, row_id, setNames)) %>% 
    select(dataset, model, loss, fold, row_id) %>% 
    # If more than one set of predictions of a model on a dataset, keep the first one. 
    distinct(dataset, model, .keep_all = TRUE) 
  
  cv_split = split(cv_df, factor(cv_df$dataset))
  
  clean = lapply(cv_split, make_loss_matrix_cv)
  clean 
}

