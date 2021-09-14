library(data.table)
setDTthreads(1L)
library(mfsurrogates)
library(mlr3misc)
library(lgr)

RhpcBLASctl::blas_set_num_threads(1L)
RhpcBLASctl::omp_set_num_threads(1L)

root = here::here()
workdir = file.path(root, "irace/data/surrogates")
source(file.path(root, "experiments/ofaatime2/optim2.R"))

eval_ = function(job, data, instance, budget_factor = 30L, ...) {
  data.table::setDTthreads(1L)
  RhpcBLASctl::blas_set_num_threads(1L)
  RhpcBLASctl::omp_set_num_threads(1L)

  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  root = here::here()
  workdir = file.path(root, "irace/data/surrogates")

  xs = list(...)

  # get surrogate model
  objective = if (instance$cfg == "nb301") {
    mfsurrogates::cfgs(instance$cfg, workdir = workdir)$get_objective(target_variables = instance$target)
  } else {
    mfsurrogates::cfgs(instance$cfg, workdir = workdir)$get_objective(task = instance$level, target_variables = instance$target)
  }
  if (instance$cfg == "rbv2_super") {
    objective$domain$params$trainsize$lower = 1 / 27
  }

  meta_objective = get_meta_objective_from_surrogate(objective, budgetfactor = budget_factor * instance$multiplier)
  res = mlr3misc::invoke(meta_objective, .args = xs)
  res$cfg = instance$cfg
  res$task = instance$level
  list(archive = res)
}

library(batchtools)
reg = makeExperimentRegistry(file.dir = "/gscratch/lschnei8/registry_ofaatime_14_09", source = file.path(root, "experiments/ofaatime2/optim2.R"))
#reg = makeExperimentRegistry(file.dir = NA, source = file.path(root, "experiments/ofaatime2/optim2.R"))
saveRegistry(reg)

instances = readRDS(system.file("instances.rds", package = "mfsurrogates"))
instances = instances[cfg %in% c("lcbench", "rbv2_super")]
instances[cfg == "lcbench", target := "val_cross_entropy"]
instances[cfg == "lcbench", multiplier := 1]
instances[cfg == "rbv2_super", target := "logloss"]
instances[cfg == "rbv2_super", multiplier := 1]
instances[, id_plan := 1:.N]

instances_nb301 = readRDS(system.file("instances.rds", package = "mfsurrogates"))[cfg == "nb301"]
instances_nb301[, target := "val_accuracy"]
instances_nb301[, multiplier := 1]

instances = rbind(instances, instances_nb301, fill = TRUE)
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

collected = readRDS("/pfs/tc1/project/mallet/mbinder1/collected.rds")
results = collected[curseed == -1 & mu == "muvary" & siman & infillsearch == "all"]
results_best = results[curseed == -1 & mu == "muvary" & siman & infillsearch == "all"][, .SD[which.max(lperf)], by = "objective"]
lambda_lcbench = results_best[objective == "lcbench"]$lambda[[1L]]
lambda_lcbench$batch_method = results_best[objective == "lcbench"]$batchmethod
lambda_rbv2_super = results_best[objective == "rbv2_super"]$lambda[[1L]]
lambda_rbv2_super$batch_method = results_best[objective == "rbv2_super"]$batchmethod

#iterate over the for loop below once with lambda = lambda_lcbench and lambda = lambda_rbv2_super
lambdas_cfg = list(lcbench = lambda_lcbench, rbv2_super = lambda_rbv2_super)
saveRDS(lambdas_cfg, "lambdas_cfg.rds")

repls = 30L

# FIXME: nasbench301?
for (cfg in c("lcbench", "rbv2_super")) {
  lambda = lambdas_cfg[[cfg]]
  prob_designs_selected = prob_designs[grepl(cfg, names(prob_designs))]

  # baseline experiments
  ids = addExperiments(
    prob.designs = prob_designs_selected,
    algo.designs = list(eval_ = as.data.table(lambda)),
    repls = repls
  )
  addJobTags(ids, c(cfg, "baseline"))

  # baseline experiments mu = 32
  prob_designs_selected_32 = map(prob_designs_selected, function(prob) {
    prob$multiplier = 32
    prob
  })
  lambdas = lambda
  lambdas$mu = 32
  lambdas = as.data.table(lambdas)
  ids = addExperiments(
    prob.designs = prob_designs_selected_32,
    algo.designs = list(eval_ = lambdas),
    repls = repls
  )
  addJobTags(ids, c(cfg, "baseline_mu_32"))

  # which surrogate
  id = "surrogate_learner"
  value = lambda[[id]]
  param = searchspace$params[[id]]
  lambdas =  lapply(param$levels, function(val) {
    insert_named(lambda, set_names(list(val), id))
  })
  lambdas = rbindlist(lambdas, use.names = TRUE)
  ids = addExperiments(
    prob.designs = prob_designs_selected,
    algo.designs = list(eval_ = lambdas),
    repls = repls
  )
  addJobTags(ids, c(cfg, id))

  # randomness
  lambdas = lambda
  lambdas$filter_algorithm = "tournament"
  lambdas$filter_select_per_tournament = 1L
  lambdas = as.data.table(lambdas)
  ids = addExperiments(
    prob.designs = prob_designs_selected,
    algo.designs = list(eval_ = lambdas),
    repls = repls
  )
  addJobTags(ids, c(cfg, "tournament_1"))

  lambdas = lambda
  firstval = sqrt(lambdas$filter_factor_first * lambdas$filter_factor_first.end)
  lastval = sqrt(lambdas$filter_factor_last * lambdas$filter_factor_last.end)
  lambdas$filter_factor_first = lambdas$filter_factor_first.end = firstval
  lambdas$filter_factor_last = lambdas$filter_factor_last.end = lastval
  lambdas$random_interleave_fraction = lambdas$random_interleave_fraction.end = mean(lambdas$random_interleave_fraction, lambdas$random_interleave_fraction.end)
  lambdas = as.data.table(lambdas)
  ids = addExperiments(
    prob.designs = prob_designs_selected,
    algo.designs = list(eval_ = lambdas),
    repls = repls
  )
  addJobTags(ids, c(cfg, "siman_off"))

  lambdas = lambda
  overallval = (lambdas$filter_factor_first * lambdas$filter_factor_first.end * lambdas$filter_factor_last * lambdas$filter_factor_last.end) ^ (1 / 4)
  lambdas$filter_factor_first = lambdas$filter_factor_first.end = lambdas$filter_factor_last = lambdas$filter_factor_last.end = overallval
  lambdas$random_interleave_fraction = lambdas$random_interleave_fraction.end = mean(lambdas$random_interleave_fraction, lambdas$random_interleave_fraction.end)
  lambdas = as.data.table(lambdas)
  ids = addExperiments(
    prob.designs = prob_designs_selected,
    algo.designs = list(eval_ = lambdas),
    repls = repls
  )
  addJobTags(ids, c(cfg, "siman_off_same_ff"))

  lambdas = lambda
  lambdas$random_interleave_fraction = lambdas$random_interleave_fraction.end = 0
  lambdas = as.data.table(lambdas)
  ids = addExperiments(
    prob.designs = prob_designs_selected,
    algo.designs = list(eval_ = lambdas),
    repls = repls
  )
  addJobTags(ids, c(cfg, "random_interleave_0"))

  lambdas = lambda
  lambdas$random_interleave_fraction = lambdas$random_interleave_fraction.end = 1
  lambdas = as.data.table(lambdas)
  ids = addExperiments(
    prob.designs = prob_designs_selected,
    algo.designs = list(eval_ = lambdas),
    repls = repls
  )
  addJobTags(ids, c(cfg, "random_interleave_1"))

  # which batch method
  id = "batch_method"
  value = lambda[[id]]
  param = searchspace$params[[id]]
  lambdas =  lapply(param$levels, function(val) {
    insert_named(lambda, set_names(list(val), id))
  })
  lambdas = rbindlist(lambdas, use.names = TRUE)
  ids = addExperiments(
    prob.designs = prob_designs_selected,
    algo.designs = list(eval_ = lambdas),
    repls = repls
  )
  addJobTags(ids, c(cfg, id))

  # filter with max budget
  id = "filter_with_max_budget"
  value = lambda[[id]]
  param = searchspace$params[[id]]
  lambdas =  lapply(param$levels, function(val) {
    insert_named(lambda, set_names(list(val), id))
  })
  lambdas = rbindlist(lambdas, use.names = TRUE)
  ids = addExperiments(
    prob.designs = prob_designs_selected,
    algo.designs = list(eval_ = lambdas),
    repls = repls
  )
  addJobTags(ids, c(cfg, id))

  # no multifidelity
  lambdas = lambda
  lambdas$budget_log_step = 100
  lambdas = as.data.table(lambdas)
  ids = addExperiments(
    prob.designs = prob_designs_selected,
    algo.designs = list(eval_ = lambdas),
    repls = repls
  )
  addJobTags(ids, c(cfg, "no_multifidelity"))

  # small mu
  lambdas = lambda
  lambdas$mu = 4
  if (round(4 * lambdas$survival_fraction) >= 3) {
    firstval = sqrt(lambdas$filter_factor_first * lambdas$filter_factor_first.end)
    lastval = sqrt(lambdas$filter_factor_last * lambdas$filter_factor_last.end)
    lambdas$filter_factor_first = lambdas$filter_factor_first.end = firstval
    lambdas$filter_factor_last = lambdas$filter_factor_last.end = lastval
  }
  lambdas = as.data.table(lambdas)
  ids = addExperiments(
    prob.designs = prob_designs_selected,
    algo.designs = list(eval_ = lambdas),
    repls = repls
  )
  addJobTags(ids, c(cfg, "small_mu"))
}

for (cfg in c("lcbench", "rbv2_super")) {
  lambda = lambdas_cfg[[cfg]]
  prob_designs_selected = prob_designs[grepl("nb301", names(prob_designs))]

  # baseline experiments
  ids = addExperiments(
    prob.designs = prob_designs_selected,
    algo.designs = list(eval_ = as.data.table(lambda)),
    repls = repls
  )
  addJobTags(ids, c("nb301", cfg, "baseline"))

  # baseline experiments mu = 32
  prob_designs_selected_32 = map(prob_designs_selected, function(prob) {
    prob$multiplier = 32
    prob
  })
  lambdas = lambda
  lambdas$mu = 32
  lambdas = as.data.table(lambdas)
  ids = addExperiments(
    prob.designs = prob_designs_selected_32,
    algo.designs = list(eval_ = lambdas),
    repls = repls
  )
  addJobTags(ids, c("nb301", cfg, "baseline_mu_32"))
}

tab = getJobTable()
baseline_jobs = tab[grepl("baseline", tab$tags)]$job.id
baseline_mu_32_jobs = tab[grepl("baseline_mu_32", tab$tags)]$job.id
baseline_jobs = setdiff(baseline_jobs, baseline_mu_32_jobs)
ablation_jobs = setdiff(tab$job.id, c(baseline_jobs, baseline_mu_32_jobs))
ablation_jobs = intersect(ablation_jobs, setdiff(findNotDone()$job.id, findQueued()$job.id))

baseline_jobs = findJobs(ids = baseline_jobs)
baseline_jobs[, chunk := batchtools::chunk(job.id, chunk.size = 5L)]
baseline_mu_32_jobs = findJobs(ids = baseline_mu_32_jobs)
ablation_jobs = findJobs(ids = ablation_jobs)
ablation_jobs[, chunk := batchtools::chunk(job.id, chunk.size = 5L)]

# standard resources used to submit jobs to cluster
resources.serial.default = list(
  walltime = 3600L * 12L, memory = 1024L * 1L, clusters = "serial", max.concurrent.jobs = 9999L
)

# large resources used to submit jobs to cluster
resources.serial.long = list(
  walltime = 3600L * 24L, memory = 1024L * 4L, clusters = "serial", max.concurrent.jobs = 9999L
)

submitJobs(baseline_mu_32_jobs, resources = resources.serial.long)
submitJobs(baseline_jobs, resources = resources.serial.default)
submitJobs(ablation_jobs, resources = resources.serial.default)

