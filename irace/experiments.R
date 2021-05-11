source("./irace/optimization.R")
lgr::get_logger("bbotk")$set_threshold("warn")
folder = "./irace/data"

workdir = "./irace/data/surrogates"
cfg = cfgs("lcbench", workdir = workdir)
cfg$setup()

# 07.05.2021
# 52*100*8 smashy budget 
# 3000 irace budget
# single-crit
subfolder = "data_07_05_single_b3000"
dir.create(file.path(folder,  subfolder))

future::plan("multicore", workers = 40)
task_levels = readRDS(system.file("instances.rds", package = "mfsurrogates"))[test == TRUE & cfg == "lcbench", level]

res = optimize_irace(
  objective_targets = "val_balanced_accuracy",
  test_targets = "test_balanced_accuracy", 
  instances = task_levels, 
  cfg = cfg, 
  evals = 3000, 
  instance_file = file.path(folder, subfolder, "irace_instance.rda"),
  log_file = file.path(folder, subfolder, "irace_log.Rdata"))

# 11.05.2021
# 52*100*8 smashy budget 
# 3000 irace budget
# multi-crit
subfolder = "data_11_05_multi_b3000"
dir.create(file.path(folder,  subfolder))

future::plan("multicore", workers = 40)
task_levels = readRDS(system.file("instances.rds", package = "mfsurrogates"))[test == TRUE & cfg == "lcbench", level]

res = optimize_irace(
  objective_targets = c("val_balanced_accuracy", "time"),
  test_targets = "test_balanced_accuracy", "time"),
  instances = task_levels, 
  cfg = cfg, 
  evals = 3000, 
  instance_file = file.path(folder, subfolder, "irace_instance.rda"),
  log_file = file.path(folder, subfolder, "irace_log.Rdata"))
