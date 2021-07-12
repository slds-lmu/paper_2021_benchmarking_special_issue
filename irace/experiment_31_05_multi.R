# Log:
# * latest mfsurrogates package version (31.05)
# * new mfsurrogates models
# * set upper bound of `filter_factor_*` parameters to 1000 in irace search space
# * convert ranger and kknn surrogate learner to graph learners that impute missing values
# * log transformation of `timepredict` target
# * fix missing dependencies in smashy search space

set.seed(7345)
source("./irace/optimization.R")
lgr::get_logger("bbotk")$set_threshold("warn")
folder = "./irace/data"
workdir = "./irace/data/surrogates"
subfolder = "data_31_05_multi"
dir.create(file.path(folder,  subfolder))
max_time = readRDS( "./irace/data/surrogates/rbv2_super/max_time.rds")

# set instances
instances_plan = readRDS(system.file("instances.rds", package = "mfsurrogates"))[test == FALSE & cfg == "rbv2_super"]
# nadir
instances_plan = merge(instances_plan, max_time, by.x = "level", by.y = "task_id")
instances_plan[, nadir := lapply(max_timepredict, function(time) c(mmce = 1 + 1, timepredict = log(time + 1)))]
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
  codomain = ps(y = p_dbl(0, Inf, tags = "maximize")),
  workdir = workdir)