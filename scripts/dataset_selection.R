# Dataset selection (openML tasks)
library(OpenML)
library(dplyr)
library(tidyr)

DIR_DATA = "../data"
r = function(x) file.path(DIR_DATA, x)
if (!dir.exists(DIR_DATA)) dir.create(DIR_DATA)

#' Add number of runs available for tasks
#' Note: n_runs is the number of total runs available for that task, but it includes duplicates.
#' @param tasks_df data frame with a column task.id
#'
#' @return tasks_df with n_runs column
#' @export
#'
#' @examples
add_runs_count = function(tasks_df) {
  runs_list = sapply(tasks_df$task.id, listOMLRuns)
  runs = do.call(rbind, runs_list)
  if (nrow(runs)==0) 
    stop("No runs available.")
  runs %>% 
    count(task.id, sort=TRUE, name="n_runs") %>% 
    full_join(tasks_df, by="task.id") %>% 
    select(n_runs, everything()) %>% 
    mutate(n_runs = ifelse(is.na(n_runs), 0, n_runs)) %>% 
    as_tibble()
}

# Load all binary classification tasks with 33% holdout and 10-fold CV (with predictive_accuracy as evaluation), and all regression tasks with 10-fold CV.
tasks_cls_holdout = listOMLTasks(task.type = "Supervised Classification", 
                                estimation.procedure = "33% Holdout set", 
                                number.of.classes = 2) %>% 
  select(task.id, name, number.of.instances, number.of.instances.with.missing.values, everything(), -status) %>% 
  add_runs_count()

tasks_reg_10cv = listOMLTasks(task.type = "Supervised Regression", 
                              estimation.procedure = "10-fold Crossvalidation") %>% 
  select(task.id, name, number.of.instances, number.of.instances.with.missing.values, everything(), -status) %>% 
  # Remove problematic task.id=52948. Throws lexical error: invalid character inside string. "Error in training the model:   Error in chol.default(R) 
  filter(task.id!=52948) %>% 
  add_runs_count()

tasks_cls_10cv = listOMLTasks(task.type = "Supervised Classification", 
                             estimation.procedure = "10-fold Crossvalidation", 
                             evaluation.measures = "predictive_accuracy",
                             number.of.classes = 2) %>% 
  select(task.id, name, number.of.instances, number.of.instances.with.missing.values, everything(), -status) %>% 
  add_runs_count()

# Filter tasks
selected_tasks_cls_holdout = tasks_cls_holdout %>% 
  filter(number.of.instances > 3000, n_runs > 10) %>% 
  # Remove second kr-vs-kp dataset.
  filter(task.id!=317602)

selected_tasks_reg_10cv = tasks_reg_10cv %>% 
  filter(number.of.instances > 1000, n_runs >= 5) %>% 
  # Remove second cpu_small dataset (the one with the fewer unique runs, not actual runs as shown here).
  filter(task.id!=4883)
  
selected_tasks_cls_10cv = tasks_cls_10cv %>% 
# openML's R API can't download tasks in Sparse_ARFF format because it is not supported by the farff package.
  filter(number.of.instances > 10000, number.of.instances < 1e6, n_runs > 10, n_runs < 500, format !="Sparse_ARFF")

# Save task ids
writeLines(as.character(selected_tasks_cls_holdout$task.id), r("task_ids_cls_holdout.txt"))
writeLines(as.character(selected_tasks_reg_10cv$task.id), r("task_ids_reg_10cv.txt"))
writeLines(as.character(selected_tasks_cls_10cv$task.id), r("task_ids_cls_10cv.txt"))
