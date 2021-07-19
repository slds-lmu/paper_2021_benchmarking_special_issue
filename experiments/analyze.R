# SCRIPT TO CREATE FIGURES, TABLES, AND GRAPHICS

source("experiments/helper.R")

library(data.table)
library(ggplot2)
library(na.tools)
library(batchtools)

# Learning Curves for the different problems 

#### branin

prob = "branin"
path = file.path("experiments/results_sequential/prepared_files", prob)
dirs = list.files(path, pattern = ".rds", full.names = TRUE)

df = computeDatasetForAnalysis(dirs, quantiles = seq(-2, 2, by = 0.1))

# Aggregate over experiments 
dfp = df[, .(mean_normalized_regret = mean(normalized_regret), lower = quantile(normalized_regret, 0.1), upper = quantile(normalized_regret, 0.9)), by = c("task", "algorithm_nexps", "q")]

p = plotAggregatedLearningCurves(dfp, var = "mean_normalized_regret")
p = p + ggtitle("Branin") # + theme(legend.direction = "horizontal", legend.position = "bottom")
ggsave(file.path("experiments/results_sequential/figures", "branin_mean_normalized_regret.png"), p, width = 9, height = 5)

# Aggregate over experiments 
dfp = df[, .(mean_performance = mean(performance), lower = quantile(performance, 0.1), upper = quantile(performance, 0.9)), by = c("task", "algorithm_nexps", "q")]

plotAggregatedLearningCurves(dfp, var = "mean_performance")

# Analyze histograms over budgets closer
df1 = readRDS("experiments/results_sequential/prepared_files/branin/hpbster_bohb.rds")
df2 = readRDS("experiments/results_sequential/prepared_files/branin/mlr3hyperband.rds")

df1 = df1[1, ]$result[[1]]
df1$algorithm = "hpbster_hb"
df1$config = 1:nrow(df1)
df1$budget_cum = cumsum(df1$budget)
df2 = df2[1, ]$result[[1]]
df2$algorithm = "mlr3hyperband"
df2$config = 1:nrow(df2)
df2$budget_cum = cumsum(df2$budget)

p1 = ggplot(data = rbind(df1, df2)[budget_cum <= 120, ], aes(x = budget, y = ..density.., fill = algorithm)) + geom_histogram(position = "dodge") + ggtitle("Budgets evaluated in a run")
p1 + facet_grid(. ~ algorithm)

p1 = ggplot(data = df1[budget_cum <= 120, ], aes(x = config, y = log(budget))) + geom_line() + ggtitle("hpbandster_hb")
p1

p2 = ggplot(data = df2[budget_cum <= 120, ], aes(x = config, y = log(budget))) + geom_line()  + ggtitle("mlr3hyperband")
p2

gridExtra::grid.arrange(p2, p1, nrow = 1)



### lcbench 

prob = "lcbench"
path = file.path("experiments/results_sequential/prepared_files", prob)
dirs = list.files(path, pattern = ".rds", full.names = TRUE)

# Train and test instances 
instances = readRDS("../paper_2021_multi_fidelity_surrogates/inst/instances.rds")

# df = computeDatasetForAnalysis(dirs, quantiles = seq(-2, 2, by = 0.25))
df = readRDS("experiments/results_sequential/prepared_files_for_analysis/lcbench/learning_curves.rds")

# Analysis on a per task level 
dfp = df[, .(mean_normalized_regret = mean(normalized_regret), lower = quantile(normalized_regret, 0.1), upper = quantile(normalized_regret, 0.9)), by = c("task", "algorithm", "q")]

dfp_train = dfp[task %in% setDT(instances)[cfg == "lcbench" & test == TRUE, ]$level, ]

p = plotAggregatedLearningCurves(dfp_train, var = "mean_normalized_regret") + facet_wrap(vars(task), nrow = 4)
p

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
  	dir.create(store_path, recursive = TRUE)

  ggsave(file.path(store_path, paste0(task.id, ".png")), p, width = 8, height = 4)
}

for (dir in dirs) {
  p = plotAggregatedLearningCurves(dir, init_des = TRUE, var = "perf_mean_rel_rs")

  task.id = strsplit(dir, "/")[[1]][4]
  idx = which(instances[cfg == "lcbench",]$level == task.id)
  test = ifelse(instances[idx, ]$test, "test", "train")

  store_path = file.path("figures", "learning_curves_relative_rs", test)

  if (!dir.exists(store_path))
    dir.create(store_path, recursive = TRUE)

  ggsave(file.path(store_path, paste0(task.id, ".png")), p, width = 8, height = 4)
}

for (dir in dirs) {
  p = plotAggregatedLearningCurves(dir, init_des = TRUE, var = "mean_rank")

  task.id = strsplit(dir, "/")[[1]][4]
  idx = which(instances[cfg == "lcbench",]$level == task.id)
  test = ifelse(instances[idx, ]$test, "test", "train")

  store_path = file.path("figures", "learning_curves_mean_rank", test)

  if (!dir.exists(store_path))
    dir.create(store_path, recursive = TRUE)

  ggsave(file.path(store_path, paste0(task.id, ".png")), p, width = 8, height = 4)
}


lcs = lapply(dirs, function(dir) {
  readRDS(file.path(dir, "learning_curves.rds"))
})

lcs = do.call(rbind, lcs)
lcs_sub = lcs[, .(mean_rank = mean(mean_rank), perf_mean_rel_rs = mean(perf_mean_rel_rs)), by = c("q", "algorithm_nexps")]
lcs_sub = lcs_sub[algorithm_nexps != "smac (1)", ]
lcs_sub = lcs_sub[algorithm_nexps != "smac (3)", ]

p = ggplot(data = lcs_sub, aes_string(x = "q", y = "mean_rank", colour = "algorithm_nexps", fill = "algorithm_nexps")) 
p = p + geom_line()
p = p + theme_bw()
p = p + scale_x_continuous(breaks = seq(0, 4, by = 1),
        labels= 10^seq(0, 4))
p + ylab("Mean rank (across replications and tasks)")
ggsave(file.path("figures", "mean_rank_across_tasks.png"), p, width = 8, height = 4)


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


# 240 Evaluationen kosten 240 sekunden 
# 6000 Evaluationen kosten 19497 Sekunden