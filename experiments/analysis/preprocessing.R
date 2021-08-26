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

### NB301 

prob = "nb301"
algorithms = c("hpbster_bohb", "mlr3hyperband", "randomsearch_full_budget", "smac_full_budget")
path = file.path("experiments/results_sequential/prepared_files", prob)
dirs = file.path(path, paste0(algorithms, ".rds"))

dir.create(file.path("experiments", "results_sequential", "prepared_files_for_analysis", prob))

df = computeDatasetForAnalysis(dirs, type = "sequential", min_max = "max")
saveRDS(df, file.path("experiments", "results_sequential", "prepared_files_for_analysis", prob, "result_sequential.rds"))

# df = computeDatasetForAnalysis(dirs, type = "parallel")
# saveRDS(df, file.path("experiments", "results_sequential", "prepared_files_for_analysis", prob, "result_parallel.rds"))


### RBV2_SUPER 

prob = "rbv2_super"
algorithms = c("hpbster_bohb", "mlr3hyperband", "randomsearch_full_budget", "smac_full_budget")
path = file.path("experiments/results_sequential/prepared_files", prob)
dirs = file.path(path, paste0(algorithms, ".rds"))

dir.create(file.path("experiments", "results_sequential", "prepared_files_for_analysis", prob))

df = computeDatasetForAnalysis(dirs, type = "sequential", min_max = "max")
saveRDS(df, file.path("experiments", "results_sequential", "prepared_files_for_analysis", prob, "result_sequential.rds"))
