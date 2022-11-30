# Model Selection with Bootstrap Validation

This repository contains the material for reproducing the experiments in [1].

- `scripts/00_run_all.sh` will install the required dependencies (using `install_dependencies.R`), download and prepare the data (using `01_download_data.R` and `02_prepare_data.R`), run all the experiments (using `03_run_experiments.sh`), and create all figures in the paper (using `figures.Rmd`). 
- `results` contains all the results from the experiments. Delete this or parts of it to rerun the experiment.
- `data` contains the processed data used in the experiments. The raw data are not included due to their size. The experiments can be run without them. 

All experiments were run in parallel on a high-performance computing cluster. To run individual experiments, run the corresponding lines in `03_run_experiments.sh`. 

[1]: R. Savvides, J. Mäkelä, and K. Puolamäki, Model selection with bootstrap validation, Stat. Anal. Data Min.: ASA Data Sci. J. (2022), 1-15. https://doi.org/10.1002/sam.11606
