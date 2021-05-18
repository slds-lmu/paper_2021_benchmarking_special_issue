source("./irace/optimization.R")
lgr::get_logger("bbotk")$set_threshold("warn")
folder = "./irace/data"
set.seed(7345)

# 17.05.2021
subfolder = "test_data_17_05_single_b3000"
dir.create(file.path(folder,  subfolder))

instances_plan = readRDS(system.file("instances.rds", package = "mfsurrogates"))[test == FALSE & cfg %in% c("rbv2_super", "lcbench")]
instances_plan[,targets := ifelse(cfg == "lcbench", "val_cross_entropy", "logloss")]
instances_plan = instances_plan[sample(nrow(instances_plan)), ]

#future::plan("multicore", workers = 40)

res = optimize_irace(
  instances_plan = instances_plan,
  evals = 3000,
  highest_budget_only = TRUE,
  instance_file = file.path(folder, subfolder, "irace_instance.rda"),
  log_file = file.path(folder, subfolder, "irace_log.Rdata"),
  workdir = "./irace/data/surrogates")

# 17.05.2021
# small_test
subfolder = "test_data_17_05_single_b3000"
dir.create(file.path(folder,  subfolder))

instances_plan = readRDS(system.file("instances.rds", package = "mfsurrogates"))[test == FALSE & cfg %in% c("rbv2_super", "lcbench")]
instances_plan[,targets := ifelse(cfg == "lcbench", "val_cross_entropy", "logloss")]
instances_plan = instances_plan[sample(nrow(instances_plan)), ]

# future::plan("multicore", workers = 8)

res = optimize_irace(
  instances_plan = instances_plan,
  evals = 300,
  highest_budget_only = TRUE,
  instance_file = file.path(folder, subfolder, "irace_instance.rda"),
  log_file = file.path(folder, subfolder, "irace_log.Rdata"),
  workdir = "./irace/data/surrogates")

# 17.05.2021
# small_test_multi
subfolder = "test_data_17_05_multi_b3000"
dir.create(file.path(folder,  subfolder))

instances_plan = readRDS(system.file("instances.rds", package = "mfsurrogates"))[test == FALSE & cfg %in% c("rbv2_super", "lcbench")]
instances_plan[,targets := ifelse(cfg == "lcbench", list(c("val_cross_entropy", "time")), list(c("logloss", "timepredict")))]
instances_plan = instances_plan[c(1, 9), ]

future::plan("multicore", workers = 40)

res = optimize_irace(
  instances_plan = instances_plan,
  evals = 3000,
  highest_budget_only = TRUE,
  instance_file = file.path(folder, subfolder, "irace_instance.rda"),
  log_file = file.path(folder, subfolder, "irace_log.Rdata"),
  workdir = "./irace/data/surrogates")



