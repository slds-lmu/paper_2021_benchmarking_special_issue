library(data.table)
library(paradox)
library(mlr3misc)
library(mlr3extralearners) #@cubist_mars
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
#reg = makeExperimentRegistry(file.dir = NA, source = file.path(root, "irace", "optimization.R"))
reg = makeExperimentRegistry(file.dir = "/dss/dssfs02/lwp-dss-0001/pr74ze/pr74ze-dss-0000/ru84tad2/registry_surrogates", source = file.path(root, "irace", "optimization.R"))
saveRegistry(reg)

# table of all problems
instances_plan = readRDS(system.file("instances.rds", package = "mfsurrogates"))[cfg %in% c("lcbench", "rbv2_super")]
instances_plan[, targets := ifelse(cfg == "lcbench", "val_cross_entropy", "logloss")]
instances_plan[, lower := ifelse(cfg == "lcbench", 1, 3 ^ (-3))]
instances_plan[, upper := ifelse(cfg == "lcbench", 52, 1)]
instances_plan[, id_plan := 1:.N]
#instances_plan = instances_plan[cfg == "lcbench"]  # FIXME: only lcbench for gp comparison

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
lambda = irace_result$result_x_domain

imputepl = po("imputeoor", offset = 1, multiplier = 10) %>>% po("fixfactors") %>>% po("imputesample")
imputepl_cubist =  po("colapply", applicator = as.integer, affect_columns = selector_type("logical")) %>>% imputepl
imputepl_mars = po("colapply", applicator = as.integer, affect_columns = selector_type("logical")) %>>% po("encode") %>>% imputepl

kknn = GraphLearner$new(imputepl %>>% mlr3::lrn("regr.kknn", fallback = mlr3::lrn("regr.featureless"), encapsulate = c(train = "evaluate", predict = "evaluate")))
kknn_local = kknn$clone(deep = TRUE)
kknn_local$param_set$values$regr.kknn.k = 1
kknn_local$id = paste0(kknn_local$id, ".1")

gp = GraphLearner$new(imputepl %>>% mlr3::lrn("regr.km", optim.method = "gen", fallback = mlr3::lrn("regr.featureless"), encapsulate = c(train = "evaluate", predict = "evaluate")))

no = lrn("regr.featureless", id = "surrogate_turned_off")

learners = list(
  no = no,
  #gp = gp,
  #ranger = GraphLearner$new(imputepl %>>% mlr3::lrn("regr.ranger", fallback = mlr3::lrn("regr.featureless"), encapsulate = c(train = "evaluate", predict = "evaluate"))),
  #knn = kknn,
  #kknn_local = kknn_local,
  #cubist = GraphLearner$new(imputepl_cubist %>>% mlr3::lrn("regr.cubist", fallback = mlr3::lrn("regr.featureless"), encapsulate = c(train = "evaluate", predict = "evaluate"))),
  #mars = GraphLearner$new(imputepl_mars %>>% mlr3::lrn("regr.mars", fallback = mlr3::lrn("regr.featureless"), encapsulate = c(train = "evaluate", predict = "evaluate")))
)
learners = lapply(learners, function(x) { class(x) <- c("LearnerRegr", class(x)) ; x })

lambda$surrogate_learner = list(list(lambda$surrogate_learner)) # batchtools complains otherwise

lambda$random_interleave_fraction = 0.8  # 80% surrogate usage
lambda$random_interleave_fraction.end = 0.8  # 80% surrogate usage

id = "surrogate_learner"

lambdas = lapply(learners, function(learner)  {
  tmp = lambda
  tmp$surrogate_learner[[1L]] = list(learner)
  tmp
})
lambdas = rbindlist(lambdas, use.names = TRUE)

ids = addExperiments(
  prob.designs = prob_designs, 
  algo.designs = list(eval_ = lambdas),
  repls = 10L
)

addJobTags(ids, id)

# Standard resources used to submit jobs to cluster
resources.serial.default = list(
  walltime = 20L * 60L * 100L, memory = 1024L * 2L, clusters = "serial", max.concurrent.jobs = 100L
)

all_jobs = findJobs()
all_jobs[, chunk := batchtools::chunk(job.id, chunk.size = 100L)]
submitJobs(all_jobs, resources = resources.serial.default)

# FIXME: budget factor?

# get all jobs to investigate param "id"
findJobsHP = function(id, problem) {
  ijoin(findExperiments(prob.name = problem), findTagged(c(id, "baseline")))
}

