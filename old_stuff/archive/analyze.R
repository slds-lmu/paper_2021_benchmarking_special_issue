# SCRIPT TO CREATE FIGURES, TABLES, AND GRAPHICS

source("experiments/helper.R")

library(data.table)
library(ggplot2)
library(na.tools)
library(batchtools)


### LEARNING CURVES ####

prob = "branin"
path = file.path("experiments/results_sequential/prepared_files", prob)
dirs = list.files(path, pattern = ".rds", full.names = TRUE)

df = computeDatasetForAnalysis(dirs)




















# Learning Curves for the different problems 

#### branin

# saveRDS(df, file.path("experiments/results_sequential/prepared_files_for_analysis", prob, "learning_curves.rds"))
# readRDS(file.path("experiments/results_sequential/prepared_files_for_analysis", prob, "learning_curves.rds"))

# Aggregate over experiments 
dfp = df[, .(mean_normalized_regret = mean(normalized_regret), lower = quantile(normalized_regret, 0.1), upper = quantile(normalized_regret, 0.9)), by = c("task", "algorithm", "q")]

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
# saveRDS(df, "experiments/results_sequential/prepared_files_for_analysis/lcbench/learning_curves.rds")
df = readRDS("experiments/results_sequential/prepared_files_for_analysis/lcbench/learning_curves.rds")
# df$algorithm_type = unlist(lapply(df$algorithm_type, function(x) strsplit(x, "_")[[1]][1]))
# df[algorithm == "mlr3hyperband", ]$algorithm_type = "hb"
# df[algorithm == "randomsearch_full_budget", ]$algorithm_type = "randomsearch"
# df[algorithm == "smac_full_budget", ]$algorithm_type = "smac"

# Analysis on a per task level 
dfp = df[, .(mean_normalized_regret = mean(normalized_regret), sd = sd(normalized_regret), n = .N), by = c("task", "algorithm", "budget_cum_log")]
dfp$upper = dfp$mean + 2 * dfp$sd / sqrt(dfp$n)
dfp$lower = dfp$mean - 2 * dfp$sd / sqrt(dfp$n)

# Compute budget distribution
dfs = lapply(c("smac_bohb", "smac_hb", "mlr3hyperband", "hpbster_hb", "hpbster_bohb", "smashy"), function(alg) {
  cbind(alg, readRDS(file.path("experiments/results_sequential/prepared_files", prob, paste0(alg, ".rds")))[1, ]$result[[1]])[, c("alg", "budget", "performance")]
})
dfs = do.call(rbind, dfs)
dfs = dfs[, iter := 1:.N, by = c("alg")]
dfs = dfs[, budget_cum_sum := cumsum(budget), by = c("alg")]
dfs = dfs[budget_cum_sum <= 12480, ]

p = ggplot(data = dfs, aes(x = budget, y = ..density..)) + geom_histogram() + facet_wrap(vars(alg), nrow = 2)
p = p + theme_bw()
ggsave(file.path("experiments", "results_sequential", "figures", "budget_distribution.png"), width = 12, height = 6)

p = ggplot(data = dfs[iter <= 200, ], aes(x = iter, y = budget)) + geom_line() + facet_wrap(vars(alg), nrow = 2)
p = p + theme_bw()
ggsave(file.path("experiments", "results_sequential", "figures", "budget_allocation.png"), width = 12, height = 6)


for (var in c("test", "train")) {
  
  store_path = file.path("experiments/results_sequential/figures", prob, "learning_curves", var)

  if (!dir.exists(store_path))
    dir.create(store_path, recursive = TRUE)

  tt = var == "test"

  dfp_train = dfp[task %in% setDT(instances)[cfg == prob & test == tt, ]$level, ]
  dfp_train_sub = dfp_train[, .(mean_normalized_regret_over_tasks = mean(mean_normalized_regret)), by = c("algorithm", "budget_cum_log")]

  # HB vs. SMAC / RANDOMSEARCH
  p = plotAggregatedLearningCurves(dfp_train_sub[algorithm %in% c("hpbster_bohb", "hpbster_hb", "randomsearch_full_budget", "smac_full_budget"), ], var = "mean_normalized_regret_over_tasks", x = "budget_cum_log", se = FALSE) 
  p = p + ggtitle(paste0("Mean Normalized Regret across Tasks (LCBench ", var,")"))
  ggsave(file.path(store_path, paste0(var, "_across_instances_bohb_hb_rs_randomsearch_variants.png")), p, width = 8, height = 4)

  # Comparison of HB implementations
  p = plotAggregatedLearningCurves(dfp_train_sub[algorithm %in% c("hpbster_hb", "mlr3hyperband"), ], var = "mean_normalized_regret_over_tasks", x = "budget_cum_log", se = FALSE) 
  p = p + ggtitle(paste0("Mean Normalized Regret across Tasks (LCBench ", var,")"))
  ggsave(file.path(store_path, paste0(var, "_across_instances_hb_variants.png")), p, width = 8, height = 4)

  # Comparison of BOHB implementations
  p = plotAggregatedLearningCurves(dfp_train_sub[algorithm %in% c("hpbster_bohb", "smac_bohb"), ], var = "mean_normalized_regret_over_tasks", x = "budget_cum_log", se = FALSE) 
  p = p + ggtitle(paste0("Mean Normalized Regret across Tasks (LCBench ", var,")"))
  ggsave(file.path(store_path, paste0(var, "_across_instances_bohb_variants.png")), p, width = 8, height = 4)

  # Comparison of BOHB, HB, SMASHY, RS & SMAC implementations
  p = plotAggregatedLearningCurves(dfp_train_sub[algorithm %in% c("hpbster_bohb", "hpbster_hb", "smashy_config_lcbench", "randomsearch_full_budget", "smac_full_budget"), ], var = "mean_normalized_regret_over_tasks", x = "budget_cum_log", se = FALSE) 
  p = p + ggtitle(paste0("Mean Normalized Regret across Tasks (LCBench ", var,")"))
  ggsave(file.path(store_path, paste0(var, "_across_instances_variants.png")), p, width = 8, height = 4)

  p = plotAggregatedLearningCurves(dfp_train[algorithm %in% c("hpbster_hb", "hpbster_bohb", "smashy_config_lcbench", "smac_full_budget", "randomsearch_full_budget"), ], var = "mean_normalized_regret", x = "budget_cum_log", se = FALSE) + facet_wrap(vars(task), ncol = 9)
  ggsave(file.path(store_path, paste0(var, "_instances.png")), p, width = 18, height = 6)

  # Per problem visualize the outcome 
  for (i in instances[instances$test == tt & cfg == "lcbench", ]$level) {

    p = plotAggregatedLearningCurves(dfp[task == i & algorithm %in% c("hpbster_bohb", "hpbster_hb", "smashy_config_lcbench", "randomsearch_full_budget", "smac_full_budget"), ], var = "mean_normalized_regret", x = "budget_cum_log", se = TRUE) + ggtitle(paste0("LCBench, instance ", i))
    
    store_path_sub = file.path(store_path, "individual")

    if (!dir.exists(store_path_sub))
      dir.create(store_path_sub, recursive = TRUE)

    ggsave(file.path(store_path_sub, paste0(i, ".png")), p, width = 8, height = 4)
  }
}










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