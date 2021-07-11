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

  # stop time for irace
  t0 = Sys.time()

  # get surrogate model
  cfg = cfgs(irace_instance$cfg, workdir = workdir)
  objective = cfg$get_objective(task = irace_instance$level, target_variables = irace_instance$targets)

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
  budget_limit = search_space$length * budget_factor * budget_upper

  # call smashy with configuration parameter in xs
  instance_ = mlr3misc::invoke(opt_objective, objective = objective, budget_limit = budget_limit, search_space = search_space, .args = xs)

  list(archive = instance_$archive, time = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}

library(batchtools)
reg = makeExperimentRegistry(file.dir = "/dss/dssfs02/lwp-dss-0001/pr74ze/pr74ze-dss-0000/ru84tad2/registry_different_lambdas", source = file.path(root, "irace", "optimization.R"))
reg = makeExperimentRegistry(file.dir = NA, source = file.path(root, "irace", "optimization.R"))
saveRegistry(reg)

# table of all problems
instances_plan = readRDS(system.file("instances.rds", package = "mfsurrogates"))[cfg %in% c("lcbench", "rbv2_super")]
instances_plan[, targets := ifelse(cfg == "lcbench", "val_cross_entropy", "logloss")]
instances_plan[, lower := ifelse(cfg == "lcbench", 1, 3 ^ (-3))]
instances_plan[, upper := ifelse(cfg == "lcbench", 52, 1)]
instances_plan[, id_plan := 1:.N]
instances_plan

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
searchspace = irace_result$search_space
on_log_scale = c("budget_log_step", "mu", "filter_factor_first", "filter_factor_last", "filter_select_per_tournament",
  "filter_factor_first.end", "filter_factor_last.end", "filter_select_per_tournament.end")  # FIXME: any way to get this automatic?

lambda = irace_result$result_x_domain
kknn = lambda$surrogate_learner
ranger = irace_result$archive$data$x_domain[[1]]$surrogate_learner
lambda$surrogate_learner = list(list(kknn)) # batchtools complains otherwise

lambda_lcbench = irace_result_lcbench$result_x_domain
kknn = lambda_lcbench$surrogate_learner
ranger = irace_result_lcbench$archive$data$x_domain[[1]]$surrogate_learner
lambda_lcbench$surrogate_learner = list(list(kknn)) # batchtools complains otherwise

# baseline / comparison experiments
ids = addExperiments(
  prob.designs = prob_designs, 
  algo.designs = list(eval_ = as.data.table(lambda)),
  repls = 3L
)
addJobTags(ids, "baseline")

for (i in seq_along(lambda)) {
  id = names(lambda)[i]

  if (id == "surrogate_learner") {
    lambdas = lambda
    lambdas$surrogate_learner[[1L]] = list(ranger)
    lambdas = as.data.table(lambdas)

    ids = addExperiments(
      prob.designs = prob_designs,
      algo.designs = list(eval_ = lambdas),
      repls = 3L
    )

    addJobTags(ids, id)
  } else {
    value = lambda[[i]]
    param = searchspace$params[[id]]
    algo.design = as.data.table(lambda)

    lambdas = switch(param$class,
      ParamLgl = ,
      ParamFct = {
        lapply(param$levels, function(val) {
          insert_named(lambda, set_names(list(val), id))
        })
      },

      ParamInt = {
        lapply(unique(round(seq(from = param$lower, to = param$upper, length.out = ngrid))), function(val) {
          insert_named(lambda, set_names(list(as.integer(val)), id))
        })
      },

      ParamDbl = {
        lapply(seq(from = param$lower, to = param$upper, length.out = ngrid), function(val) {
          insert_named(lambda, set_names(list(val), id))
        })
      }
    )

    lambdas = rbindlist(lambdas, use.names = TRUE)

    if (id %in% on_log_scale) {
      lambdas[, (id) := exp(get(id))]
    }

    if (id %in% c("mu", "filter_select_per_tournament", "filter_select_per_tournament.end")) {  # FIXME: these are double on log and integer after retrafo?
      lambdas[, (id) := as.integer(round(get(id)))]
    }

    ids = addExperiments(
      prob.designs = prob_designs,
      algo.designs = list(eval_ = lambdas),
      repls = 3L
    )

    addJobTags(ids, id)
  }
}

# turn surrogate off
lambdas = lambda
lambdas$random_interleave_fraction = 0
lambdas$random_interleave_fraction.end = 0
lambdas = as.data.table(lambdas)

ids = addExperiments(
  prob.designs = prob_designs,
  algo.designs = list(eval_ = lambdas),
  repls = 3L
)

addJobTags(ids, "surrogate_turned_off")

#testJob(32, external = TRUE)
#testJob(2236)
#testJob(6325)

# Standard resources used to submit jobs to cluster
resources.serial.default = list(
  walltime = 3600L * 24L * 2L, memory = 1024L * 2L, clusters = "serial", max.concurrent.jobs = 100L
)

all_jobs = findJobs()
all_jobs[, chunk := batchtools::chunk(job.id, chunk.size = 100L)]
submitJobs(all_jobs, resources = resources.serial.default)

# FIXME: budget factor?

# get all jobs to investigate param "id"
findJobsHP = function(id, problem) {
  ijoin(findExperiments(prob.name = problem), findTagged(c(id, "baseline")))
}

