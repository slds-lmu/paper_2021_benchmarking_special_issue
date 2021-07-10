# SCRIPT TO CREATE FIGURES, TABLES, AND GRAPHICS

source("experiments/helper.R")

library(data.table)
library(ggplot2)

# Learning Curves for the different problems 
prob = "lcbench"
path = file.path("experiments/results", prob)
dirs = list.dirs(path)
dirs = dirs[2:length(dirs)]

instances = readRDS("../paper_2021_multi_fidelity_surrogates/inst/instances.rds")

algorithms = c("hpbster", "mlr3hyperband", "smac", "randomsearch", "smashy")

d = dirs[1]
paths = paste0(d, "/", algorithms, ".rds")

status_summary = readRDS(file.path("experiments/results/status_summary.rds"))
# Exclude the ones for which we do not have a smac run
ids = status_summary[algorithm == "smac" & done >= 3, ]$task

dirs = file.path("experiments", "results", "lcbench", ids)

results_baseline = readRDS("experiments/results/results_baseline.rds")

#### 
# Transform the smashy results into the correct format
df = results_baseline[cfg == prob, 1:23]
df = as.data.table(df)
df_unique = df[ , .(mean(epoch)), by = c("cfg", "level", "repl")]
df_unique$job.id = 10^6 + seq_len(nrow(df_unique))

outlist = lapply(seq_len(nrow(df_unique)), function(i) {
  
  config = df_unique[i, ]

  out = df[level == config$level & repl == config$repl, c("epoch", "val_cross_entropy")]
  out$epoch = exp(out$epoch)
  names(out) = c("budget", "performance")
  return(out)
})

df_unique$repl = NULL
df_unique$V1 = NULL
names(df_unique) = c("problem", "task", "job.id")
df_unique = df_unique[, c("job.id", "problem", "task")]
dff = cbind(df_unique, nobjectives = 1, objectives_scalar = "val_cross_entropy", algorithm = "smashy", algorithm_type = NA, eta = NA, full_budget = NA, multi.point = NA, .count = 1)
dff$result = outlist

for (tsk in unique(dff$task)) {
  saveRDS(dff[task == tsk, ], file.path("experiments/results", prob, tsk, "smashy.rds"))
}



#### 

computeDatasetForAnalysis(dirs, algorithms, seq(0, 4, by = 0.25))

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