source("./irace/optimization.R")
#lgr::get_logger("bbotk")$set_threshold("warn")
folder = "./irace/data"
set.seed(1)

# 13.05.2021
# test with low budget
subfolder = "test"
dir.create(file.path(folder,  subfolder))

instances_plan = readRDS(system.file("instances.rds", package = "mfsurrogates"))[test == FALSE & cfg %in% c("rbv2_super")] # ,  "lcbench"
instances_plan[,targets := ifelse(cfg == "lcbench", "val_balanced_accuracy", "logloss")]
instances_plan = instances_plan[sample(nrow(instances_plan)), ]

#future::plan("multicore", workers = 40)

res = optimize_irace(
  instances_plan = instances_plan,
  evals = 300, 
  instance_file = file.path(folder, subfolder, "irace_instance.rda"),
  log_file = file.path(folder, subfolder, "irace_log.Rdata"))

