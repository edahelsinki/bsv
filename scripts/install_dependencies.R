required_packages = c(
  "dplyr",
  "purrr",
  "tidyr", 
  "forcats",
  "ggplot2",
  "ggpubr",
  "scales", 
  "xtable",
  "latex2exp",
  "OpenML", 
  "farff",
  "glmnet")

packages_to_install = required_packages[!required_packages %in% installed.packages()]
if (length(packages_to_install) == 0) {
  cat("All required packages are already installed.\n")
} else {
  cat("The following packages will be installed: \n") 
  cat(paste0(packages_to_install, collapse="\n"), "\n")
  for (pkg in packages_to_install) 
    install.packages(pkg, repos = "http://cran.us.r-project.org")
}
  
