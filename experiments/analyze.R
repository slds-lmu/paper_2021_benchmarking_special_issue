# SCRIPT TO CREATE FIGURES, TABLES, AND GRAPHICS

source("experiments/helper.R")

library(data.table)
library(ggplot2)

# Learning Curves for the different problems 
problem = "lcbench"
path = file.path("experiments/results", problem)
dirs = list.dirs(path)
dirs = dirs[2:length(dirs)]

instances = readRDS("../paper_2021_multi_fidelity_surrogates/inst/instances.rds")

algorithms = c("hpbster", "mlr3hyperband", "smac", "randomsearch")

d = dirs[1]
paths = paste0(d, "/", algorithms, ".rds")


results_baseline = readRDS("experiments/results/results_baseline.rds")



df = computeDatasetForAnalysis(dirs, algorithms, seq(0, 4, by = 0.25))

# TODO: Compute brackets to visualize them
# brackets = df[, .SD[new_bracket], by = c("algorithm")]
# brackets$budget_cum_log = log(brackets$budget_cum / 52, 10)
# brackets = brackets[, .SD[which.min(job.id)], by = c("algorithm")]


for (dir in dirs) {
  p = plotAggregatedLearningCurves(dir, init_des = TRUE)

  task.id = strsplit(dir, "/")[[1]][4]
  idx = which(instances[cfg == "lcbench",]$level == task.id)
  test = ifelse(instances[idx, ]$test, "test", "train")

  store_path = file.path("figures", "learning_curves", test)

  if (!dir.exists(store_path))
  	dir.create(store_path)

  ggsave(file.path(store_path, paste0(task.id, ".png")), p, width = 8, height = 4)
}



# 2) Same plot, but show ranks and aggregate over all

# For every dataset, we compute a rank, and than we average 

dataset = lapply(dirs, function(d) {

	dfq = readRDS(file.path(d, "learning_curves.rds"))

	return(dfq)
})

df = do.call(rbind, dataset)

# Compute mean per algorithm for every task
df = df[, .(mean_perf_per_task = mean(perf_mean)), by = c("task", "algorithm_nexps", "q")]

# Compute ranks per task 
df = df[, rank_per_task := rank(mean_perf_per_task), by = c("task", "q")]

# Average across tasks
df = df[, .(mean_rank = mean(rank_per_task)), by = c("q", "algorithm_nexps")]

p = ggplot(data = df, aes(x = q, y = mean_rank, colour = algorithm_nexps)) + geom_line()
p


# 3) Quantile Plots 


# 4) Rank table 


# 5) TODO: epsilon-Plots (budget epsilon to best performance)


# 6) Compared to random search 

# LATER:
# - Also run hyperband baseline 
# - Put plotting functions into helper 





# TODO: 
# 1) Learning Curve Plots
# - ADD 
# - DONE Mark quantiles for ancertainties
# - DONE Show task
# - DONE Mark end of the initial design 
# - DONE: Distinguish between train and test instances 

# - DISCUSS: Time amount; it is becoming a lot! Parallel analyzation
# - TODO Mark Brackets 
# - Analyze why mlr3hyperband works so well
# - TODO: Stages look strange in bohb! 

