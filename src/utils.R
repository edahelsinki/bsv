#' Run a function on a grid of input values
#'
#' @param grid named list or a data frame of arguments to f
#' If a list, then it is passed to expand.grid(), else it is used as is (column 
#' names are argument names to f, rows are different combinations of values).
#' @param f function
#' @param verbose 
#'
#' @return dataframe with list column res
#' @export
#'
#' @examples
#' run_f_on_grid(function(a,b) a+b, list(a=1:3, b=1:10)) # As list
#' run_f_on_grid(function(a,b) a+b, expand.grid(list(a=1:3, b=1:10))) # As df
run_f_on_grid = function(f, grid, verbose=FALSE) {
  grid_df = if (is.data.frame(grid)) grid else expand.grid(grid, stringsAsFactors = FALSE) 
  n = nrow(grid_df)
  res = vector("list", n)
  on.exit(return(cbind(grid_df, res=I(res))), add=TRUE) # Return results so far, even if an error happens.
  for (i in 1:n) {
    if (verbose)
      cat(sprintf("%d/%d\t%s", i, n, paste0(paste0(names(grid_df[i,,drop=FALSE]), "=", grid_df[i,]), collapse=", ")), "\n")
    res[[i]] = do.call(f, grid_df[i,,drop=FALSE])
  }
}

#' Trim winners
#'
#' Effectively makes the difference in losses between the best model and the 
#' second best at least d.
#' 
#' @param L matrix where L[i,j] is the loss of model j on data point i
#' @param d if loss_j-loss_min<d then model j is dropped
#'
#' @return L with fewer columns
#' @export
#'
#' @examples
trim_winners = function(L, d) {
  if (d == 0)
    return(L)
  trim = function(loss, d) c(which.min(loss), which(loss - min(loss, na.rm=TRUE) > d))
  losses = colMeans(L)
  j = trim(losses, d)
  L[,j,drop=FALSE]
}

#' Keep the top k most accurate models in a prediction matrix
#'
#' @param L matrix where L[i,j] is the loss of model j on data point i
#' @param k 
#'
#' @return L with k columns with the lowest means
#' @export
#'
#' @examples
keep_top_k = function(L, k) {
  if (k == ncol(L))
    return(L)
  losses = colMeans(L, na.rm=TRUE)
  L[,order(losses)[1:k],drop=FALSE]
}

#' Get best models (lowest loss)
#'
#' @param L loss matrix
#' @param delta 
#'
#' @return indices in L of best models
#' @export
#'
#' @examples
get_best = function(L, delta=1) {
  losses=colMeans(L, na.rm = TRUE)
  which(losses <= min(losses) + delta)
}

#' Summarize datasets into a dataframe
#'
#' @param data_list list of matrices or data frames
#' @param deltas numeric vector of deltas. See get_best().
summarize_data = function(data_list, deltas = 0.1) {
  df = expand.grid(
    dataset = names(data_list), 
    delta = deltas, 
    stringsAsFactors = FALSE, 
    KEEP.OUT.ATTRS = FALSE
  )
  df$N = sapply(df$dataset, function(x) nrow(data_list[[x]]))
  df$k = sapply(df$dataset, function(x) ncol(data_list[[x]]))
  df$true_best = I(mapply(function(name, d) get_best(data_list[[name]], d), df$dataset, df$delta))
  df
}

#' Make function timeable
#'
#' @param f function
#'
#' @return f, whose returned result has attr "time"
#' @export
#'
#' @examples
make_timeable = function(f) {
  f_t = function(...) {
    t0 = Sys.time()
    res = f(...)
    t1 = Sys.time()
    attr(res, "time") = t1-t0
    res
  }
  f_t
}

#' Unnest a list of simulations
#'
#' e.g. n parameter combinations, for which we have run n_rep simulations, 
#' where each simulation produces 3 numbers
#'
#' @param sim_list list of n lists of n_rep lists of n_num numbers
#' @return list of n_num lists of n vectors of n_rep numbers
unnest_sim_list = function(sim_list) {
  is_null = sapply(sim_list, is.null) 
  if (all(is_null))
    return(NULL)
  n = length(sim_list) 
  sim = sim_list[!is_null][[1]]
  n_rep = length(sim) 
  n_num = length(sim[[1]]) 
  out = vector("list", n_num)
  names(out) = names(sim[[1]])
  for (i in 1:n_num) {
    out2 = vector("list", n)
    for (j in 1:n) {
      out2[[j]] = sapply(sim_list[[j]], function(x) x[[i]])
    }
    out[[i]] = out2
  }
  out
}

#' Unnest a column list of simulations
#' 
#' @param df data frame with a list column called res
#' @param col name of list column to unnest
#' @return data frame with column col unnested (and removed).
unnest_sim_df = function(df, col="res") {
  res_new = unnest_sim_list(df[[col]])
  if (is.null(res_new))
    return(NULL)
  df[[col]] = NULL
  cbind(df, do.call(cbind, res_new))
}

#' Remove rows that have at least one NA in any column
#'
#' @param X data frame or matrix
#'
#' @return X with possibly some rows removed
#' @export
#'
#' @examples
remove_na_rows = function(X) {
  X[!apply(is.na(X), 1, any),,drop=FALSE]
}

#' Remove columns that have only one unique value
#'
#' @param df dataframe
#'
#' @return df with possibly some columns removed
#' @export
#'
#' @examples
remove_constant_vars = function(df) {
  j = sapply(df, function(x) length(unique(x)) > 1)
  df[,j]
}

#' Make dummy variables
#'
#' Note: Does not work with NAs because model.matrix silently removes NAs in x.
#'
#' @param data 
#' @param factor_cols factor columns to make into dummy variables (binary variables for each factor level)
#'
#' @return
#' @export
#'
#' @examples
make_dummy_vars = function(data, factor_cols) {
  make_dummy = function(d=data, colname) {
    x = d[,colname]
    mat = model.matrix(~x-1)
    colnames(mat) = gsub("^x", "", colnames(mat))
    colnames(mat) = paste0(colname, "_", colnames(mat))
    attributes(mat)$assign = NULL
    attributes(mat)$contrasts = NULL
    mat
  }
  cols = colnames(data)
  non_factor_cols = setdiff(cols, factor_cols)
  factors_dummy = do.call(cbind, lapply(factor_cols, function(col) make_dummy(colname=col)))
  if (length(non_factor_cols)==0)
    return(as.data.frame(factors_dummy))
  cbind(data[,non_factor_cols,drop=FALSE], factors_dummy)
}

#' Normalize data
#' 
#' Apply scale() only to numeric variables.
#'
#' @param data 
#' @param center 
#' @param scale 
#'
#' @return
#' @export
#'
#' @examples
normalize = function(data, center=TRUE, scale=TRUE) {
  i_num = sapply(data, is.numeric)
  data[,i_num] = scale(data[,i_num], center=center, scale=scale)
  data
}

#' Prepare openML dataset
#'
#' @param x list L where L$data is a dataframe and L$target.features has the target feature
#'
#' @return list L with L$data with NAs removed and L$target_feature
#' @export
#'
#' @examples
prepare_openml_dataset = function(x) {
  d = list()
  d$target_feature = x$target.features
  data = remove_na_rows(x$data)
  factor_cols = setdiff(colnames(data)[sapply(data, is.factor)], d$target_feature)
  if (length(factor_cols>0))
    data = make_dummy_vars(data, factor_cols)
  data = remove_constant_vars(data) 
  data = droplevels(data) # Remove unused factor levels.
  data = normalize(data)
  d$data = data
  d
}

#' Summarize parameter data frame
#'
#' @param df data frame whose columns are parameter values on a grid
#'
#' @return character vector x with length(x)=nrow(df), of the form c("col1=val1,col2=val2","col1=val1,col2=val2")
#' @export
#'
#' @examples
summarize_param_df = function(df) {
  # Use only values from columns that are non-constant.
  j_non_constant_non_list = which(sapply(df, function(x) !is.list(x) && length(unique(x))>1))
  sapply(1:nrow(df), function(i) paste0(paste0(names(df)[j_non_constant_non_list],"="), 
                                        df[i,j_non_constant_non_list], collapse=","))
}

#' Run expression if file is missing
#' 
#' @param expr expression to run
#' @param file file to save to, if it is missing
#' @param overwrite if TRUE, overwrites file
run_if_missing_file = function(expr, file, overwrite=FALSE) {
  if (!file.exists(file) | overwrite) {
    out = eval(expr)
    saveRDS(out, file)
    cat(sprintf("Saved to %s.\n", file))
  } else {
    cat(sprintf("Loading from existing file %s. ", file))
    out = readRDS(file)
    cat(sprintf("Loaded.\n"))
  }
  return(out)
}

#' Sample columns from a loss matrix L 
#' L has K columns, where K_best of them are within delta from the best.
#' This function returns L with k columns and k_best within delta from the best.
#' Is deterministic if k_best <= K_best and k_nonbest <= K_nonbest, where k_nonbest = k-k_best.
#' Else is random, because columns are resampled to reach k or k_best.
#'
#' @param L loss matrix
#' @param k number of columns to sample
#' @param k_best number of columns with mean within delta from the minimum
#' @param delta 
sample_k = function(L, k, k_best, delta) {
  K = ncol(L)
  l = unname(colMeans(L))
  approx_best = which(l - min(l) <= delta)
  approx_nonbest = setdiff(1:K, approx_best)
  K_best = length(approx_best)
  
  if (k_best > k) {
    warning("Can't have k_best>k. Setting k_best=k.\n")
    k_best = k
  }
  
  if (k_best <= K_best) {
    j_best = approx_best[1:k_best]
  } else {
    m = k_best - K_best
    extra = sample(1:K_best, m, replace=TRUE)
    j_best = c(approx_best, extra)
  }
  
  if (k_best==k) {
    j_nonbest = c()
  } else if (k-k_best <= K-K_best) {
    j_nonbest = (K_best+1):(K_best+k-k_best)
  } else {
    m = k-k_best-(K-K_best)
    extra = sample(K_best:K, m, replace=TRUE)
    j_nonbest = c(K_best:K, extra)
  }
  
  L[,sort(c(j_best, j_nonbest)), drop=FALSE]
}

#' Clip a loss matrix
#'
#' @param L 
#' @param M maximum allowed value
#'
#' @return
#' @export
#'
#' @examples
clip_loss = function(L, M) {
  apply(L, 2, function(x) pmin(x, M))
}
