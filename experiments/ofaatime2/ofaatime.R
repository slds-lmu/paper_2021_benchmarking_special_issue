library(data.table)
setDTthreads(1L)
library(mfsurrogates)
library(mlr3misc)
library(lgr)

root = here::here()
workdir = file.path(root, "irace/data/surrogates")
source(file.path(root, "experiments/ofaatime2/optim2.R"))

eval_ = function(job, data, instance, budget_factor = 30, ...) {
  data.table::setDTthreads(1L)
  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  root = here::here()
  workdir = file.path(root, "irace/data/surrogates")

  xs = list(...)

  # get surrogate model
  objective = mfsurrogates::cfgs(instance$cfg, workdir = workdir)$get_objective(task = instance$level, target_variables = instance$target)
  if (instance$cfg == "rbv2_super") {
    objective$domain$params$trainsize$lower = 1 / 27
  }

  meta_objective = get_meta_objective_from_surrogate(objective, budgetfactor = budget_factor)
  res = mlr3misc::invoke(meta_objective, .args = xs)
  res$cfg = instance$cfg
  res$task = instance$level
  list(archive = res)
}


library(batchtools)
ngrid = 5L
reg = makeExperimentRegistry(file.dir = "/gscratch/lschnei8/registry_ofaatime_12_09", source = file.path(root, "experiments/ofaatime2/optim2.R"))
#reg = makeExperimentRegistry(file.dir = NA, source = file.path(root, "experiments/ofaatime2/optim2.R"))
saveRegistry(reg)

instances = readRDS(system.file("instances.rds", package = "mfsurrogates"))
instances = instances[cfg %in% c("lcbench", "rbv2_super")]
instances[cfg == "lcbench", target := "val_cross_entropy"]
instances[cfg == "lcbench", multiplier := 1]
instances[cfg == "rbv2_super", target := "logloss"]
instances[cfg == "rbv2_super", multiplier := 1]
#instances = instances[test == TRUE]  # ablation only on test
instances[, id_plan := 1:.N]

# add problems
prob_designs = imap(split(instances, instances$id_plan), function(instancex, name) {
  prob_id = sprintf("%s_%s", instancex$cfg[1], name)
  addProblem(prob_id, fun = function(...) list(...), seed = 123)
  set_names(list(instancex), prob_id)
})
nn = sapply(prob_designs, names)
prob_designs = unlist(prob_designs, recursive = FALSE, use.names = FALSE)
names(prob_designs) = nn

# add eval_ algorithm (never use `eval` as a function name or have a function named `eval` in .GlobalEnv)
addAlgorithm("eval_", fun = eval_)

searchspace = get_searchspace(include.mu = TRUE, include.batchmethod = TRUE, infill = "all", include.siman = TRUE, include.mo = FALSE, numeric.only = FALSE)
on_log_scale = c("budget_log_step", "mu", "filter_factor_first", "filter_factor_last", "filter_select_per_tournament", "filter_factor_first.end", "filter_factor_last.end", "filter_select_per_tournament.end")

#lambda_lcbench = 
#lambda_rbv2_super =
# iterate over the for loop below once with lambda = lambda_lcbench and lambda = lambda_rbv2_super
#lambdas_cfg = list(lcbench = lambda_lcbench, rbv2_super = lambda_rbv2_super)

lambda = list(
  budget_log_step = 2,
  survival_fraction = 0.5,
  surrogate_learner = "ranger",
  filter_with_max_budget = TRUE,
  filter_factor_first = 1,
  random_interleave_fraction = 0.5,
  random_interleave_random = FALSE,
  sample = "random",
  mu = 2,
  batch_method = "smashy",
  filter_factor_last = 1,
  filter_algorithm = "tournament",
  filter_select_per_tournament = 1,
  filter_factor_first.end = 1,
  random_interleave_fraction.end = 0.3,
  filter_factor_last.end = 1,
  filter_select_per_tournament.end = 1
)

repls = 1L

for (cfg in c("lcbench", "rbv2_super")) {
  #lambda = lambdas_cfg[[cfg]]
  prob_designs_selected = prob_designs[grepl(cfg, names(prob_designs))]

  # baseline / comparison experiments
  ids = addExperiments(
    prob.designs = prob_designs_selected,
    algo.designs = list(eval_ = as.data.table(lambda)),
    repls = repls
  )
  addJobTags(ids, c(cfg, "baseline"))

  for (i in seq_along(lambda)) {
    id = names(lambda)[i]
  
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

    if (id %in% c("mu", "filter_select_per_tournament", "filter_select_per_tournament.end")) { # FIXME: these are double on log and integer after retrafo?
      lambdas[, (id) := as.integer(round(get(id)))]
    }
  
    ids = addExperiments(
      prob.designs = prob_designs_selected,
      algo.designs = list(eval_ = lambdas),
      repls = repls
    )
    addJobTags(ids, c(cfg, id))
    
  }

  # turn surrogate off
  lambdas = lambda
  lambdas$random_interleave_fraction = 1
  lambdas$random_interleave_fraction.end = 1
  lambdas = as.data.table(lambdas)
  
  ids = addExperiments(
    prob.designs = prob_designs_selected,
    algo.designs = list(eval_ = lambdas),
    repls = repls
  )
  addJobTags(ids, c(cfg, "surrogate_turned_off"))
}

# standard resources used to submit jobs to cluster
resources.serial.default = list(
  walltime = 3600L * 24L * 2L, memory = 1024L * 2L, clusters = "serial", max.concurrent.jobs = 100L
)

all_jobs = findJobs()
all_jobs[, chunk := batchtools::chunk(job.id, chunk.size = 10L)] # ceiling(NROW(all_jobs) / 100L)
submitJobs(all_jobs, resources = resources.serial.default)


