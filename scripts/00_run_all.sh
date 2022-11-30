#!/bin/bash
# Run this script to reproduce all results.
# bash 00_run_all.R

echo "Checking dependencies..."
Rscript --vanilla install_dependencies.R

echo "Checking data..."
Rscript --vanilla 01_download_data.R
Rscript --vanilla 02_prepare_data.R

echo "Running experiments..."
./03_run_experiments.sh

echo "Creating figures in figures.Rmd..."
R -e 'suppressPackageStartupMessages({rmarkdown::render("figures.Rmd")})'

echo "Done."
