library(data.table)
library(paradox)
library(mlr3misc)
root = here::here()


# stuff copied from marc
source(file.path(root, "irace", "optimization.R"))
workdir = file.path(root, "irace/data/surrogates")
highest_budget_only = TRUE

eval = function(job, data, instance, budget_factor = 30, ...) {
  # ML: wrap and unpack
  irace_instance = instance
  irace_instance$surrogate_learner = irace_instance$surrogate_learner[[1L]]
  xs = list(...)
    
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
  instance = mlr3misc::invoke(opt_objective, objective = objective, budget_limit = budget_limit, 
    search_space = search_space, .args = xs)

  cols_y = instance$archive$cols_y

  # the objective is maximized
  objective_multiplicator = unname(instance$objective_multiplicator) * -1

  # filter for experiments with highest budget only
  if (highest_budget_only) instance$archive$data = instance$archive$data[get(budget_id) == max(get(budget_id)), ]

  # apply target_trafo
  if (!is.null(irace_instance$target_trafo)) instance$archive$data = irace_instance$target_trafo(instance$archive$data)

  y = if (instance$objective$codomain$length > 1) {
    # hypervolume
    mat = as.matrix(instance$archive$data[, cols_y, with = FALSE])
    mat = sweep(mat, 2, objective_multiplicator, `*`)
    miesmuschel:::domhv(mat, nadir = irace_instance$nadir * objective_multiplicator) 
  } else {
    # best performance
    as.numeric(instance$archive$best()[, cols_y, with = FALSE]) * objective_multiplicator
  }

  list(y = y, time = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}

library(batchtools)
ngrid = 5L # How many points on a grid for real values?
reg = makeExperimentRegistry(file.dir = NA)
reg$source = "../../irace/optimization.R"
saveRegistry(reg)

# table of all problems
instances_plan = readRDS(system.file("instances.rds", package = "mfsurrogates"))[cfg %in% c("lcbench", "rbv2_super")]
instances_plan[,targets := ifelse(cfg == "lcbench", "val_cross_entropy", "logloss")]
instances_plan[,lower := ifelse(cfg == "lcbench", 1, 3^(-3))]
instances_plan[,upper := ifelse(cfg == "lcbench", 52, 1)]
instances_plan[,id_plan := 1:.N]
instances_plan

# add problems
prob_designs = imap(split(instances_plan, instances_plan$id_plan), function(instance, name) {
  prob_id = sprintf("%s_%s", instance$cfg[1], name)
  addProblem(prob_id, fun = function(...) list(...), seed = 123)
  set_names(list(instance), prob_id)
})
nn = sapply(prob_designs, names)
prob_designs = unlist(prob_designs, recursive = FALSE, use.names = FALSE)
names(prob_designs) = nn

# add eval algorithm
addAlgorithm("eval", fun = eval)

irace_result = readRDS(file.path(root, "irace", "data", "data_31_05_single", "irace_instance.rda"))
searchspace = irace_result$search_space
lambda = irace_result$result_x_domain
lambda$surrogate_learner = list(lambda$surrogate_learner) # batchtools complains otherwise

# baseline / comparison experiments
ids = addExperiments(
  prob.designs = prob_designs, 
  algo.designs = list(eval = as.data.table(lambda)),
  repls = 3
)
addJobTags(ids, "baseline")


for (i in seq_along(lambda)) {
  id = names(lambda)[i]
  if (id == "surrogate_learner") {
    next
  }
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

  ids = addExperiments(
    prob.designs = prob_designs, 
    algo.designs = list(eval = rbindlist(lambdas, use.names = TRUE)),
    repls = 3L
  )

  addJobTags(ids, id)
}


testJob(32)

# FIXME: budget factor?


# # get all jobs to investigate param 'id'
# findJobsHP = function(id, problem) {
#   ijoin(findExperiments(prob.name = problem), findTagged(c(id, "baseline")))
# }

# findJobsHP("filter_factor_first", "lcbench_1")
