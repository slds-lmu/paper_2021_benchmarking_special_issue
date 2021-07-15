# Log:
# * remove kknn with n = 1

set.seed(7345)
source("./irace/optimization.R")
lgr::get_logger("bbotk")$set_threshold("warn")
folder = "./irace/data"
workdir = "./irace/data/surrogates"
subfolder = "data_12_07_single_lcbench"
dir.create(file.path(folder,  subfolder))

# set instances
instances_plan = readRDS(system.file("instances.rds", package = "mfsurrogates"))[test == FALSE & cfg == "lcbench"]
# set targets
instances_plan[,targets := ifelse(cfg == "lcbench", "val_cross_entropy", "logloss")]
# set lower and upper bound of fidelity parameter
instances_plan[,lower := ifelse(cfg == "lcbench", 1, 3^(-3))]
instances_plan[,upper := ifelse(cfg == "lcbench", 52, 1)]
# set nadir
instances_plan[, nadir := 1]

# set instance id and shuffle
instances_plan[,id_plan := 1:.N]
instances_plan = instances_plan[sample(nrow(instances_plan)),] 

# download latest files
lapply(unique(instances_plan$cfg), function(id) {
  cfg = cfgs(id, workdir = workdir)
  cfg$setup(force = TRUE)
})

future::plan("multicore", workers = 40)

res = optimize_irace(
  instances_plan = instances_plan,
  evals = 3000,
  highest_budget_only = TRUE,
  instance_file = file.path(folder, subfolder, "irace_instance.rda"),
  log_file = file.path(folder, subfolder, "irace_log.Rdata"),
  codomain = ps(y = p_dbl(-Inf, 0, tags = "maximize")),
  workdir = workdir)