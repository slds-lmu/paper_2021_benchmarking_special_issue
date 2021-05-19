source("./irace/optimization_single.R")
lgr::get_logger("bbotk")$set_threshold("warn")
folder = "./irace/data"
set.seed(7345)

# 19.05.2021
# single-crit
subfolder = "data_19_05_single_b3000"
dir.create(file.path(folder,  subfolder))

# set instances
instances_plan = readRDS(system.file("instances.rds", package = "mfsurrogates"))[test == FALSE & cfg %in% c("rbv2_super", "lcbench")]
# set targets
instances_plan[,targets := ifelse(cfg == "lcbench", "val_cross_entropy", "logloss")]
# set lower and upper bound of fidelity parameter
instances_plan[,lower := ifelse(cfg == "lcbench", 1, 3^(-3))]
instances_plan[,upper := ifelse(cfg == "lcbench", 52, 1)]
instances_plan[,id_plan := 1:.N]
instances_plan = instances_plan[sample(nrow(instances_plan)),] 

future::plan("multicore", workers = 40)

res = optimize_irace(
  instances_plan = instances_plan,
  evals = 3000,
  highest_budget_only = TRUE,
  instance_file = file.path(folder, subfolder, "irace_instance.rda"),
  log_file = file.path(folder, subfolder, "irace_log.Rdata"),
  workdir = "./irace/data/surrogates")
