#############################################
#											#
# PREPROCESSING OF FILES TO ANALYZE QUICKLY #
#											#
#############################################

source("experiments/helper.R")

library(data.table)


### LCBENCH 

prob = "lcbench"
algorithms = c("hpbster_bohb", "mlr3hyperband", "randomsearch_full_budget", "smac_full_budget", "smashy")
path = file.path("experiments/results_sequential/prepared_files", prob)
dirs = file.path(path, paste0(algorithms, ".rds"))

df = computeDatasetForAnalysis(dirs, type = "sequential")
saveRDS(df, file.path("experiments", "results_sequential", "prepared_files_for_analysis", "lcbench", "result_sequential.rds"))

df = computeDatasetForAnalysis(dirs, type = "parallel")
saveRDS(df, file.path("experiments", "results_sequential", "prepared_files_for_analysis", "lcbench", "result_parallel.rds"))