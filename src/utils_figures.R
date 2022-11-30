suppressPackageStartupMessages({
  library(scales)
  library(ggplot2)
  library(purrr)
  library(dplyr)
  library(tidyr)
})

compute_error_rate = function(selected, true) {
  n_rep = length(selected)
  if (is.list(true)) {
    stopifnot(length(true) == n_rep)
    n_wrong = sum(sapply(1:n_rep, function(i) !selected[i] %in% true[[i]]))
  } else {
    n_wrong = sum(!selected %in% true)
  }
  (1+n_wrong)/(1+n_rep)
}

compute_alpha_emp = function(selected, true, p, a) {
  n_rep = length(selected)
  if (is.list(true)) {
    stopifnot(length(true) == n_rep)
    if (length(a)==1) a = rep(a, n_rep)
    n_wrong = sum(sapply(1:n_rep, function(i) !(selected[i] %in% true[[i]]) & (p[i]<=a[i])))
  } else {
    n_wrong = sum(!selected %in% true & p <= a)
  }
  (1+n_wrong)/(1+n_rep)
}

compute_power_emp = function(selected, true, p, a) {
  n_rep = length(selected)
  if (is.list(true)) {
    stopifnot(length(true) == n_rep)
    if (length(a)==1) a = rep(a, n_rep)
    n_wrong = sum(sapply(1:n_rep, function(i) (selected[i] %in% true[[i]]) & (p[i]<=a[i])))
  } else {
    n_wrong = sum(selected %in% true & p <= a)
  }
  (1+n_wrong)/(1+n_rep)
}

#' Add measures to results table
#'
#' @param df data frame with list-columns n, candidate, true_best, pvalue, N.
#'
#' @return df with extra columns
#' @export
#'
#' @examples
add_stats = function(df) {
  mutate(df, 
         k_delta = map_dbl(true_best, length), 
         data_use_avg = map_dbl(n, mean, na.rm=TRUE), 
         data_use_sd = map_dbl(n, sd, na.rm=TRUE),
         data_use_max = map_dbl(n, max, na.rm=TRUE), 
         data_use_pct_avg = data_use_avg / N,
         data_use_pct_sd = data_use_sd / N,
         pvalue_avg = map_dbl(pvalue, mean, na.rm=TRUE),
         pvalue_sd_rep = map_dbl(pvalue, sd, na.rm=TRUE),
         t_avg = map_dbl(t, mean, na.rm=TRUE),
         t_sd = map_dbl(t, sd, na.rm=TRUE),
         p_N = map2_dbl(N, n, function(t_obs, t) (1+sum(t >= t_obs)) / (1+length(t))),
         error_rate = map2_dbl(candidate, true_best, compute_error_rate),
         power_emp = pmap_dbl(list(p=pvalue, a=alpha, selected=candidate, true=true_best), 
                              compute_power_emp), 
         alpha_emp = pmap_dbl(list(p=pvalue, a=alpha, selected=candidate, true=true_best), 
                              compute_alpha_emp))
}

add_dummy_error = function(df, col="k_delta") {
  mutate(df, 
         error_dummy = map2_dbl(get(col), k, function(n_best, n_all) 1-n_best/n_all))
}

add_n_ci = function(df) {
  # df should have columns data_use_avg, data_use_sd, T0, N
  mutate(df, 
         n_lower = pmax(2*T0, data_use_avg-data_use_sd), 
         n_upper = pmin(N, data_use_avg+data_use_sd), 
         n_lower2 = pmax(2*T0, data_use_avg-2*data_use_sd), 
         n_upper2 = pmin(N, data_use_avg+2*data_use_sd)
  )
}

add_model_evals = function(df) mutate(df,
                                      model_evals_avg = map2_dbl(k, n, function(x,y) mean(x*y, na.rm=TRUE)),
                                      model_evals_10foldCV = k*N*(10-1),
                                      model_train_ours = k,
                                      model_train_10foldCV = k*10,
                                      model_train_LOOCV = k*N)

binom_ci = function(p=0.05, n=1000, alpha=0.95) qnorm(alpha + (1-alpha)/2)*sqrt(p*(1-p) / n) # for 0.95 CI we take the qnorm(0.975) to get the familiar z=1.96
add_binom_prop_ci = function(df, col, n_rep=length(df$candidate[[1]])) {
  mutate(df, 
         sd = map_dbl(get(col), ~binom_ci(., n=n_rep, alpha=.95)),
         lower=pmax(0, get(col)-sd), 
         upper=pmin(1, get(col)+sd))
}



print_latex_table = function(df, caption="", label="", ...) {
  print(xtable::xtable(df, 
                       type = "latex", 
                       caption = caption, 
                       label = label), 
        sanitize.colnames.function = identity, 
        sanitize.text.function = identity, 
        include.rownames = FALSE, 
        ...)
}

format_tbl = function(df) {
  if ("dataset" %in% names(df)) {
    df = df %>% 
      mutate(dataset = gsub("_", "\\_",dataset, fixed=TRUE), 
             dataset = sprintf("\\texttt{%s}", dataset))
  }
  if ("k_delta" %in% names(df)) {
    df$k_delta = as.integer(df$k_delta)
    if ("k" %in% names(df)) {
      df$kk_delta = sprintf("%d(%d)", df$k, df$k_delta)
    }
  }
  df %>% 
    mutate(T0 = as.integer(T0), 
           data_use_avg=as.integer(round(data_use_avg, 0)), 
           data_use_sd=as.integer(round(data_use_sd, 0)),
           data_use_max=as.integer(data_use_max), 
           data_use = paste0(data_use_avg, "+-", data_use_sd),
           delta = formatC(delta), 
           error_rate = scales::percent(error_rate, accuracy=.1), 
           power_emp = scales::percent(power_emp, accuracy=.1), 
           alpha_emp = scales::percent(alpha_emp, accuracy=.1),
           data_use_pct_avg = scales::percent(data_use_pct_avg, accuracy=1), 
           data_use_avgsd=sprintf("%d(%d)", data_use_avg, data_use_sd)) 
}

rename2latex = function(df) {
  if ("error_rate" %in% names(df)) df$error_rate = gsub("%", "\\\\%", df$error_rate)
  if ("power_emp" %in% names(df)) df$power_emp =  gsub("%", "\\\\%", df$power_emp) 
  if ("alpha_emp" %in% names(df)) df$alpha_emp =  gsub("%", "\\\\%", df$alpha_emp)
  if ("data_use_pct_avg" %in% names(df)) df$data_use_pct_avg =  gsub("%", "\\\\%", df$data_use_pct_avg)
  
  df %>% 
    rename_with(
      recode, 
      N="$N$", 
      N_va="$N_{va}$", 
      N_te="$N_{te}$", 
      N_trva="$N_{trva}$", 
      k="$k$", 
      k_delta="$k_{\\delta}$", 
      kk_delta="$k(k_{\\delta})$",
      delta="$\\delta$",
      alpha="$\\alpha$",
      n_rep = "$n_{rep}$",
      T0="$T$",
      data_use="$\\mathrm{E}[n]$",
      data_use_avg="$\\mathrm{E}[n]$",
      data_use_sd="$\\mathrm{sd}(n)$",
      data_use_max="$\\max(n)$",
      data_use_pct_avg="$\\mathrm{E}[n]/N$",
      data_use_avgsd="$\\mathrm{E}[n](\\mathrm{sd}(n))$",
      error_rate="$\\mathrm{error}$",
      alpha_emp="$\\alpha_{emp}$",
      power_emp="$\\mathrm{power}_{emp}$", 
      error_rate_cv = "$\\mathrm{error}_{\\mathrm{cv}}$", 
      error_rate_ours = "$\\mathrm{error}_{\\mathrm{ours}}$", 
      error_rate_cv_delta = "$\\mathrm{error}_{\\mathrm{cv},\\delta}$", 
      error_rate_ours_delta = "$\\mathrm{error}_{\\mathrm{ours},\\delta}$", 
      error_rate_lambda.min = "$\\mathrm{error}_{\\mathrm{cv, lambda.min}}$", 
      error_rate1_ours = "$\\mathrm{error}_{\\mathrm{ours,best}}$",
      error_rate1_cv = "$\\mathrm{error}_{\\mathrm{cv}}$",
      k_med = "$\\mathrm{median}(k)$"
    ) 
}




#' Summarize datasets
#'
#' @param L_list list of loss matrices 
#' @return data frame with columns dataset, model, avg_loss, rank, 
#' where dataset=names(L_list), model=colnames(L), avg_loss=colMeans(L), rank=1:ncol(L) (and where L=L_list[[dataset]]).
summarize_losses = function(L_list) {
  make_loss_profile = function(d) {
    names = colnames(d)
    if (is.null(names)) names = 1:ncol(d)
    data.frame(model=names, avg_loss=unname(colMeans(d)), rank=1:ncol(d))
  }
  loss_profile_list = lapply(L_list, make_loss_profile)
  tibble(dataset=factor(names(loss_profile_list), levels=names(loss_profile_list), ordered=TRUE), 
         lp=loss_profile_list) %>% 
    unnest(lp)
}

#' Plot losses
#'
#' @param L_list list of loss matrices
#' @param facet_scales 
#' @param facet_ncol 
#'
#' @return
#' @export
#'
#' @examples
plot_loss_profiles = function(L_list, facet_scales="free_x", facet_ncol=3) {
  df_long = summarize_losses(L_list)
  ggplot(df_long, aes(rank, avg_loss)) + 
    geom_col(size=0.1)+
    labs(x="Model", y="Average loss") + 
    facet_wrap(~dataset, scales=facet_scales, ncol=facet_ncol)
}


