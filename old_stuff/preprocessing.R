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
dirs = file.path(path, paste0(algorithms, "_small_version.rds"))

dir.create(file.path("experiments", "results_sequential", "prepared_files_for_analysis", prob), recursive = TRUE)

df = computeDatasetForAnalysis(dirs, type = "sequential", min_max = "max")
saveRDS(df, file.path("experiments", "results_sequential", "prepared_files_for_analysis", prob, "result_sequential_small.rds"))

# For HB / BOHB, we take them logarithmically 
library(batchtools)

df = readRDS("experiments/results_sequential/prepared_files_for_analysis/rbv2_super/result_sequential.rds")

unlink("preprocessing_registry", recursive = TRUE)
reg = makeRegistry(file.dir = "preprocessing_registry", seed = 123L, packages = c("data.table"))
reg = loadRegistry("preprocessing_registry", writeable = TRUE)
reg$cluster.functions = makeClusterFunctionsMulticore(ncpus = 6)

budgets = exp(seq(log(3^(-3)), log(100), length.out = 2))
budgets = c(budgets, seq(3^(-3), 100, length.out = 2))
budgets = budgets[order(budgets)]


fun = function(tt) {
	budgets = exp(seq(log(3^(-3)), log(100), length.out = 2))
	budgets = c(budgets, seq(3^(-3), 100, length.out = 2))
	budgets = budgets[order(budgets)]
	out = lapply(budgets, function(x) {
		df[budget_cum <= x, ][, .SD[which.min(normalized_regret)], by = c("job.id")]
	})
	do.call(rbind, out)
}

batchMap(fun, tt = unique(df$task))

submitJobs()



df$budget_ceiling = ceiling(df$budget_cum / 10)
df_small = df[, .SD[which.min(df$normalized_regret)], by = c("budget_ceiling", "job.id")]

saveRDS(df, file.path("experiments", "results_sequential", "prepared_files_for_analysis", prob, "result_sequential_small.rds"))

# TODO: smac_only needs to be moved from di25pic2 to 

# For hpbster_bohb, mlr3hyperband, and randomsearch we compiled it on the cluster; 
# For smac we did it locally (was computed on di25pic2)
df_local = readRDS(file.path("experiments", "results_sequential", "prepared_files_for_analysis", prob, "result_sequential_local.rds")) # _smac_only.rds"))
df_smac = readRDS(file.path("experiments", "results_sequential", "prepared_files_for_analysis", prob, "result_sequential_smac_only.rds"))

df_smac$y_min = NULL
df_smac$y_max = NULL

df_minima = df_local[, c("task", "y_min", "y_max")]
df_minima = setDT(df_minima)[, .SD[1], by = c("task")]
df_smac = merge(df_smac, df_minima, by = c("task"), all.x = TRUE)
df_smac$normalized_regret = ((-1) * df_smac$perfmin - df_smac$y_min) / (df_smac$y_max - df_smac$y_min)

df = rbind(df_local, df_smac)

% TASKS TO REMOVE 
task_to_omit = unique(df_smac[perfmin > 1000, ]$task)
df = df[- which(task %in% task_to_omit), ]

saveRDS(df, file.path("experiments", "results_sequential", "prepared_files_for_analysis", prob, "result_sequential.rds"))
