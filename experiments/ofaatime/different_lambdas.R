library(data.table)
library(paradox)
library(mlr3misc)
root = here::here()
workdir = file.path(root, "irace/data/surrogates")

# stuff copied from marc
source(file.path(root, "irace", "optimization.R"))

eval_ = function(job, data, instance, budget_factor = 30, ...) {
  root = here::here()
  workdir = file.path(root, "irace/data/surrogates")
  irace_instance = instance
  xs = list(...)
  xs$surrogate_learner = xs$surrogate_learner[[1L]]

  # get surrogate model
  cfg = cfgs(irace_instance$cfg, workdir = workdir)
  objective = if (instance$cfg == "branin") {
    cfg$get_objective()
  } else {
    cfg$get_objective(task = irace_instance$level, target_variables = irace_instance$targets)
  }

  # create search space
  domain = objective$domain
  param_ids = domain$ids()
  budget_idx = which(domain$tags %in% c("budget", "fidelity"))
  budget_id = param_ids[budget_idx]
  budget_lower = irace_instance$lower
  budget_upper = irace_instance$upper
  params_to_keep = param_ids[- budget_idx]

  search_space = ParamSet$new(domain$params[params_to_keep])
  search_space$add(ParamDbl$new(id = budget_id, lower = log(budget_lower), upper = log(budget_upper), tags = "budget"))
  domain_tafo = domain$trafo
  search_space$trafo = function(x, param_set) {
    if (!is.null(domain_tafo)) x = domain_tafo(x, param_set)
    x[budget_id] = if (domain$params[[budget_id]]$class == "ParamInt") as.integer(exp(x[[budget_id]])) else exp(x[[budget_id]])
    x
  }
  search_space$deps = domain$deps

  # calculate smashy budget
  budget_limit = 1L * search_space$length * budget_factor * budget_upper

  # call smashy with configuration parameter in xs
  instance_ = mlr3misc::invoke(opt_objective, objective = objective, budget_limit = budget_limit, search_space = search_space, .args = xs)

  cols = c(instance$targets, instance$budget_par)

  res = instance_$archive$data[, cols, with = FALSE]
  res$cfg = instance$cfg
  res$task = instance$level
  res[, eval_nr := seq_len(.N)]
  res[, best := map_dbl(seq_len(NROW(res)), function(i) min(res[[instance$targets]][1:i]))]  # FIXME: should introduce max_min multiplicator if we do not minimize
}

library(batchtools)
reg = makeExperimentRegistry(file.dir = "/dss/dssfs02/lwp-dss-0001/pr74ze/pr74ze-dss-0000/ru84tad2/registry_different_lambdas", source = file.path(root, "irace", "optimization.R"))
#reg = makeExperimentRegistry(file.dir = NA, source = file.path(root, "irace", "optimization.R"))
saveRegistry(reg)

# table of all problems
instances_plan = readRDS(system.file("instances.rds", package = "mfsurrogates"))[cfg %in% c("lcbench", "rbv2_super")]
instances_plan[, targets := ifelse(cfg == "lcbench", "val_cross_entropy", "logloss")]
instances_plan[, budget_par := ifelse(cfg == "lcbench", "epoch", "trainsize")]
instances_plan[, lower := ifelse(cfg == "lcbench", 1, 3 ^ (-3))]
instances_plan[, upper := ifelse(cfg == "lcbench", 52, 1)]
instances_plan[, id_plan := 1:.N]
instances_plan
instances_plan = rbind(instances_plan, data.table(cfg = "branin", test = FALSE, level = NA_character_, targets = "y", budget_par = "fidelity", lower = 0.001, upper = 0.001, id_plan = 125))

# add problems
prob_designs = imap(split(instances_plan, instances_plan$id_plan), function(instancex, name) {
  prob_id = sprintf("%s_%s", instancex$cfg[1], name)
  addProblem(prob_id, fun = function(...) list(...), seed = 123)
  set_names(list(instancex), prob_id)
})
nn = sapply(prob_designs, names)
prob_designs = unlist(prob_designs, recursive = FALSE, use.names = FALSE)
names(prob_designs) = nn

# add eval_ algorithm (never use `eval` as a function name or have a function named `eval` in .GlobalEnv)
addAlgorithm("eval_", fun = eval_)

irace_result = readRDS(file.path(root, "irace", "data", "data_31_05_single", "irace_instance.rda"))
irace_result_lcbench = readRDS(file.path(root, "irace", "data", "data_07_07_single_lcbench", "irace_instance.rda"))
# irace_result_rbv2_super = readRDS(file.path(root, "irace", "data", "data_07_07_single_lcbench", "irace_instance.rda"))

lambda = irace_result$result_x_domain
lambda$surrogate_learner = list(list(lambda$surrogate_learner)) # batchtools complains otherwise

lambda_martin = copy(lambda)
lambda_martin$budget_log_step = log(7)
lambda_martin$mu = 32
lambda_martin$survival_fraction = 0.45
lambda_martin$filter_factor_first = 50
lambda_martin$filter_factor_first.end = 1000
lambda_martin$filter_factor_last = 10
lambda_martin$filter_factor_last.end = 25
lambda_martin$random_interleave_fraction = 0.5
lambda_martin$random_interleave_fraction.end = 0.8

lambda_lcbench = irace_result_lcbench$result_x_domain
lambda_lcbench$surrogate_learner = list(list(lambda_lcbench$surrogate_learner)) # batchtools complains otherwise

#lambda_rbv2_super = irace_result_rbv2_super$result_x_domain
#lambda_rbv2_super$surrogate_learner = list(list(lambda_rbv2_super$surrogate_learner)) # batchtools complains otherwise

ids_lambda = addExperiments(
  prob.designs = prob_designs,
  algo.designs = list(eval_ = as.data.table(lambda)),
  repls = 30L
)
addJobTags(ids_lambda, "lambda")

ids_lambda_martin = addExperiments(
  prob.designs = prob_designs,
  algo.designs = list(eval_ = as.data.table(lambda_martin)),
  repls = 30L
)
addJobTags(ids_lambda_martin, "lambda_martin")

ids_lambda_lcbench = addExperiments(
  prob.designs = prob_designs[grepl("lcbench", names(prob_designs))],
  algo.designs = list(eval_ = as.data.table(lambda_lcbench)),
  repls = 30L
)
addJobTags(ids_lambda_lcbench, "lambda_lcbench")

#ids_lambda_rbv2_super = addExperiments(
#  prob.designs = prob_designs[grepl("rbv2_super", names(prob_designs))],
#  algo.designs = list(eval_ = as.data.table(lambda_rbv2_super)),
#  repls = 30L
#)
#addJobTags(ids_lambda_rbv2_super, "lambda_rbv2_super")


# Standard resources used to submit jobs to cluster
resources.serial.default = list(
  walltime = 3600L * 24L * 2L, memory = 1024L * 2L, clusters = "serial", max.concurrent.jobs = 100L
)

all_jobs = findJobs()
all_jobs[, chunk := batchtools::chunk(job.id, chunk.size = 100L)]
submitJobs(all_jobs, resources = resources.serial.default)

