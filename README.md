In this repository we release all code to replicate all results, tables and figures presented in the paper:
Automated Benchmark-Driven Design and Explanation of Hyperparameter Optimizers

The repository ist structured as follows:
  * `experiments/` contains all code used troughout our benchmark experiments and ablation studies

Within `experiments/` you can find:
  * `algorithms/`: FIXME
  * `analysis/`: directory containing all code used to analyse our benchmark experiments and ablation studies
  * `ofatime/`: directory containing all code used throughout our ablation study
  * `plots/`: directory containing all plots as presented in the paper
  * `results/`: directory containing all aggregated results; if you would like to obtain the raw unaggregated results,
    please open an issue as we currently do not host them due to large file sizes
  * `config.R`, `helper.R`, `publication_themes.R`, `reduce.R`, `setup.R`, `submit.R`, `test.R` FIXME

How to replicate our ablation study:
  1. Setup `R (4.0.2)` and install all required packages.
     You can find an `renv.lock` file at `experiments/ofatime/`.
     By installing renv and calling `renv::restore(lockfile = "experiments/ofatime/renv.lock")` you can install all R packages with the exact same version as used on the cluster.
  2. `experiments/ofatime/ofatime.R` is the main file to run the ablation study.
     It relies on batchtools to be run on a cluster.
     When setting this up on your own, adjust paths and directories (replace `"FIXME"` placeholders) of the batchtools registry and output files as needed or run an interactive session:
     ```r
     reg = makeExperimentRegistry(file.dir = NA, source = file.path(root, "experiments/ofatime/optim.R"))`
     ```
     After saving the results, you must aggregate them using the code provided in `experiments/analysis/aggregate.R`.
     This step is not necessary if you only want to inspect our results because we provide aggregated versions already in `experiments/results/`.
  3. `experiments/analysis/plots.R` is the main file to generate results and figures.

A note on YAHPO GYM:
  * Benchmark experiments and ablations rely on [YAHPO GYM](https://github.com/slds-lmu/yahpo_gym)'s [pre-alpha version](https://github.com/slds-lmu/paper_2021_multi_fidelity_surrogates).
  * Required data (e.g., ONNX files) is provided in the `experiments/ofatime/data/surrogates/` directory.
