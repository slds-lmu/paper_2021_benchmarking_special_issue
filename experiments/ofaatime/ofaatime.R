library(data.table)
library(paradox)
library(mlr3misc)
library(mlr3extralearners)
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
	start_t = Sys.time()
  instance_ = mlr3misc::invoke(opt_objective, objective = objective, budget_limit = budget_limit, search_space = search_space, .args = xs)
	end_t = Sys.time()

  cols = c(instance$targets, instance$budget_par)

  res = instance_$archive$data[, cols, with = FALSE]
  res$cfg = instance$cfg
  res$task = instance$level
  res[, eval_nr := seq_len(.N)]
  res[, best := map_dbl(seq_len(NROW(res)), function(i) min(res[[instance$targets]][1:i]))]  # FIXME: should introduce max_min multiplicator if we do not minimize

  list(archive = res, runtime = end_t - start_t)
}

library(batchtools)
# FIXME: delete old registry
ngrid = 5L # How many points on a grid for real values?
reg = makeExperimentRegistry(file.dir = "/dss/dssfs02/lwp-dss-0001/pr74ze/pr74ze-dss-0000/ru84tad2/registry_ofaatime", source = file.path(root, "irace", "optimization.R"))
#reg = makeExperimentRegistry(file.dir = NA, source = file.path(root, "irace", "optimization.R"))
saveRegistry(reg)

# table of all problems
instances_plan = readRDS(system.file("instances.rds", package = "mfsurrogates"))[cfg %in% c("lcbench", "rbv2_super")]
instances_plan[, targets := ifelse(cfg == "lcbench", "val_cross_entropy", "logloss")]
instances_plan[, budget_par := ifelse(cfg == "lcbench", "epoch", "trainsize")]
instances_plan[, lower := ifelse(cfg == "lcbench", 1, 3 ^ (-3))]
instances_plan[, upper := ifelse(cfg == "lcbench", 52, 1)]
instances_plan = rbind(instances_plan, data.table(cfg = "branin", test = FALSE, level = NA_character_, targets = "y", budget_par = "fidelity", lower = 0.001, upper = 1))
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

# FIXME: use separate results once available
irace_result_lcbench = readRDS(file.path(root, "irace", "data", "data_07_07_single_lcbench", "irace_instance.rda"))
searchspace = irace_result_lcbench$search_space
on_log_scale = c("budget_log_step", "mu", "filter_factor_first", "filter_factor_last", "filter_select_per_tournament",
  "filter_factor_first.end", "filter_factor_last.end", "filter_select_per_tournament.end")  # FIXME: any way to get this automatic?
lambda_lcbench = irace_result_lcbench$result_x_domain
lambda_lcbench$surrogate_learner = list(list(lambda_lcbench$surrogate_learner)) # batchtools complains otherwise

imputepl = po("imputeoor", offset = 1, multiplier = 10) %>>% po("fixfactors") %>>% po("imputesample")
imputepl_cubist =  po("colapply", applicator = as.integer, affect_columns = selector_type("logical")) %>>% imputepl
imputepl_mars = po("colapply", applicator = as.integer, affect_columns = selector_type("logical")) %>>% po("encode") %>>% imputepl
kknn = GraphLearner$new(imputepl %>>% mlr3::lrn("regr.kknn", fallback = mlr3::lrn("regr.featureless"), encapsulate = c(train = "evaluate", predict = "evaluate")))
kknn$id = paste0(kknn$id, ".7")
kknn_local = kknn$clone(deep = TRUE)
kknn_local$param_set$values$regr.kknn.k = 1
kknn_local$id = paste0(kknn_local$id, ".1")

learners = list(
  ranger = GraphLearner$new(imputepl %>>% mlr3::lrn("regr.ranger", fallback = mlr3::lrn("regr.featureless"), encapsulate = c(train = "evaluate", predict = "evaluate"))),
  knn = kknn,
  kknn_local = kknn_local,
  cubist = GraphLearner$new(imputepl_cubist %>>% mlr3::lrn("regr.cubist", fallback = mlr3::lrn("regr.featureless"), encapsulate = c(train = "evaluate", predict = "evaluate"))),
  mars = GraphLearner$new(imputepl_mars %>>% mlr3::lrn("regr.mars", fallback = mlr3::lrn("regr.featureless"), encapsulate = c(train = "evaluate", predict = "evaluate"))),
  gp = mlr3::lrn("regr.km", optim.method = "gen", covtype = "matern3_2", nugget.stability = 10^-8, fallback = mlr3::lrn("regr.featureless"), encapsulate = c(train = "evaluate", predict = "evaluate"))
)
learners = lapply(learners, function(x) { class(x) <- c("LearnerRegr", class(x)) ; x })

# iterate over the for loop below once with lambda = lambda_lcbench and lambda = lambda_rbv2_super
lambdas_cfg = list(lcbench = lambda_lcbench, rbv2_super = lambda_rbv2_super)

for (cfg in c("lcbench", "rbv2_super")) {
  lambda = lambdas_cfg[[cfg]]

  # baseline / comparison experiments
  ids = addExperiments(
    prob.designs = prob_designs,
    algo.designs = list(eval_ = as.data.table(lambda)),
    repls = 30L
  )
  addJobTags(ids, c(cfg, "baseline"))

  for (i in seq_along(lambda)) {
    id = names(lambda)[i]
  
    if (id == "surrogate_learner") {
      learners_ = if (cfg == "lcbench") learners else learners[-5L]  # drop gp for rbv2_super
      lambdas = lapply(learners_, function(learner) {
        lambda$surrogate_learner = list(list(learner))
        lambda
      })
      lambdas = rbindlist(lambdas, use.names = TRUE)
  
      ids = addExperiments(
        prob.designs = prob_designs,
        algo.designs = list(eval_ = lambdas),
        repls = 10L
      )
  
      addJobTags(ids, c(cfg, id))
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
        repls = 10L
      )
  
      addJobTags(ids, c(cfg, id))
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
    repls = 10L
  )
  
  addJobTags(ids, c(cfg, "surrogate_turned_off"))
}

# Standard resources used to submit jobs to cluster
resources.serial.default = list(
  walltime = 3600L * 24L * 1L, memory = 1024L * 2L, clusters = "serial", max.concurrent.jobs = 100L
)

all_jobs = findJobs()
all_jobs[, chunk := batchtools::chunk(job.id, chunk.size = ceiling(NROW(all_jobs) / 100L))]
submitJobs(all_jobs, resources = resources.serial.default)

################################################################################# Analysis and Plots ##################################################################################################

# FIXME: see different_lambdas.R
