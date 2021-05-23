source("./irace/optimization.R")
lgr::get_logger("bbotk")$set_threshold("warn")
folder = "./irace/data"
workdir = "./irace/data/surrogates"

# 18.05.2021
# single-crit
set.seed(7345)
subfolder = "test_data_21_05_single_b3000"
dir.create(file.path(folder,  subfolder))

# set instances
instances_plan = readRDS(system.file("instances.rds", package = "mfsurrogates"))[test == FALSE & cfg %in% c("lcbench")] # "rbv2_super", 
# set targets
instances_plan[,targets := ifelse(cfg == "lcbench", "val_cross_entropy", "logloss")]
# set lower and upper bound of fidelity parameter
instances_plan[,lower := ifelse(cfg == "lcbench", 1, 3^(-3))]
instances_plan[,upper := ifelse(cfg == "lcbench", 52, 1)]
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
  evals = 300,
  highest_budget_only = TRUE,
  instance_file = file.path(folder, subfolder, "irace_instance.rda"),
  log_file = file.path(folder, subfolder, "irace_log.Rdata"),
  codomain = ps(y = p_dbl(0, 1, tags = "maximize")),
  workdir = workdir)

# 18.05.2021
# multi-crit
set.seed(7345)
subfolder = "data_20_05_multi_b3000"
dir.create(file.path(folder,  subfolder))
max_time = readRDS( "./irace/data/surrogates/rbv2_super/max_time.rds")

# set instances
instances_plan = readRDS(system.file("instances.rds", package = "mfsurrogates"))[test == FALSE & cfg == "rbv2_super"]
# nadir
instances_plan = merge(instances_plan, max_time, by.x = "level", by.y = "task_id")
instances_plan[, nadir := lapply(max_timepredict, function(time) c(mmce = 1 + 1, timepredict = time + 1))]
instances_plan$max_timepredict = NULL
# target trafo
timepredict_trafo = function(data) {
  data$timepredict = log(data$timepredict)
  data
}
instances_plan[, target_trafo := rep(list(timepredict_trafo), .N)]
# set targets
instances_plan[,targets := rep(list(c("mmce", "timepredict")), .N)]
# set lower and upper bound of fidelity parameter
instances_plan[,lower := 3^(-3)]
instances_plan[,upper := 1]
# set instance id and shuffle
instances_plan[,id_plan := 1:.N]
instances_plan = instances_plan[sample(nrow(instances_plan)), ]

#future::plan("multicore", workers = 40)

res = optimize_irace(
  instances_plan = instances_plan,
  evals = 300,
  highest_budget_only = TRUE,
  instance_file = file.path(folder, subfolder, "irace_instance.rda"),
  log_file = file.path(folder, subfolder, "irace_log.Rdata"),
  workdir = "./irace/data/surrogates")

