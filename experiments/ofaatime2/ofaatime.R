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
instances = instances[cfg %in% c("lcbench", "rbv2_super") & test]
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

  # baseline experiments mu = 8
  prob_designs_selected_8 = map(prob_designs_selected, function(prob) {
    prob$multiplier = 8
    prob
  })
  lambdas = lambda
  lambdas$mu = 8
  lambdas = as.data.table(lambdas)
  ids = addExperiments(
    prob.designs = prob_designs_selected_8,
    algo.designs = list(eval_ = lambdas),
    repls = repls
  )
  addJobTags(ids, c(cfg, "baseline_mu_8"))

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

  # baseline experiments mu = 8
  prob_designs_selected_8 = map(prob_designs_selected, function(prob) {
    prob$multiplier = 8
    prob
  })
  lambdas = lambda
  lambdas$mu = 8
  lambdas = as.data.table(lambdas)
  ids = addExperiments(
    prob.designs = prob_designs_selected_8,
    algo.designs = list(eval_ = lambdas),
    repls = repls
  )
  addJobTags(ids, c("nb301", cfg, "baseline_mu_8"))
}

tab = getJobTable()
baseline_jobs = tab[grepl("baseline", tab$tags)]$job.id
baseline_mu_8_jobs = tab[grepl("baseline_mu_8", tab$tags)]$job.id
baseline_jobs = setdiff(baseline_jobs, baseline_mu_8_jobs)
ablation_jobs = setdiff(tab$job.id, c(baseline_jobs, baseline_mu_8_jobs))

baseline_jobs = findJobs(ids = baseline_jobs)
baseline_jobs[, chunk := batchtools::chunk(job.id, chunk.size = 5L)]
baseline_mu_8_jobs = findJobs(ids = baseline_mu_8_jobs)
ablation_jobs = findJobs(ids = ablation_jobs)
ablation_jobs[, chunk := batchtools::chunk(job.id, chunk.size = 5L)]

# standard resources used to submit jobs to cluster
resources.serial.default = list(
  walltime = 3600L * 12L, memory = 1024L * 2L, clusters = "serial", max.concurrent.jobs = 9999L
)

# large resources used to submit jobs to cluster
resources.serial.long = list(
  walltime = 3600L * 24L, memory = 1024L * 8L, clusters = "serial", max.concurrent.jobs = 9999L
)

submitJobs(baseline_mu_8_jobs, resources = resources.serial.long)
submitJobs(baseline_jobs, resources = resources.serial.default)
submitJobs(ablation_jobs, resources = resources.serial.default)

################################################################################# Analysis and Plots ##################################################################################################

library(data.table)
library(batchtools)
library(mlr3misc)

reg = loadRegistry(file.dir = "/gscratch/lschnei8/registry_ofaatime_14_09")
tags = batchtools::getUsedJobTags()
tab = getJobTable()

# baseline lcbench
config = "lcbench"
jobs = data.table(job.id = intersect(reg$tags[tag == "baseline"]$job.id, setdiff(reg$tags[tag == config]$job.id, reg$tags[tag == "nb301"]$job.id)))
jobs = findDone(jobs)
results_baseline = reduceResultsDataTable(fun = function(x, job) {
  budget_param = switch(job$instance$cfg, lcbench = "epoch", rbv2_super = "trainsize", nb301 = "epoch")
  archive = x$archive
  archive[, budget := round(exp(get(budget_param)))]
  stopifnot(all(archive$budget >= 1L & archive$budget <= 52L))
  archive[, cumbudget := cumsum(budget)]
  archive[, repl := job$repl]
  archive[, id := job$id]
  archive
}, ids = jobs)
results_baseline = rbindlist(results_baseline$result)
saveRDS(results_baseline, paste0("/home/lschnei8/ofaatime/results/results_baseline_lcbench.rds"))

# baseline rbv2_super
config = "rbv2_super"
jobs = data.table(job.id = intersect(reg$tags[tag == "baseline"]$job.id, setdiff(reg$tags[tag == config]$job.id, reg$tags[tag == "nb301"]$job.id)))
jobs = findDone(jobs)
results_baseline = reduceResultsDataTable(fun = function(x, job) {
  budget_param = switch(job$instance$cfg, lcbench = "epoch", rbv2_super = "trainsize", nb301 = "epoch")
  archive = x$archive
  archive[, budget := exp(get(budget_param))]
  stopifnot(all(archive$budget >= 0 & archive$budget <= 1))
  archive[, cumbudget := cumsum(budget)]
  archive[, repl := job$repl]
  archive[, id := job$id]
  archive
}, ids = jobs)
results_baseline = rbindlist(results_baseline$result)
saveRDS(results_baseline, paste0("/home/lschnei8/ofaatime/results/results_baseline_rbv2_super.rds"))

# baseline nb301 lcbench config
config = "lcbench"
jobs = data.table(job.id = intersect(reg$tags[tag == "baseline"]$job.id, intersect(reg$tags[tag == config]$job.id, reg$tags[tag == "nb301"]$job.id)))
jobs = findDone(jobs)
results_baseline = reduceResultsDataTable(fun = function(x, job) {
  budget_param = switch(job$instance$cfg, lcbench = "epoch", rbv2_super = "trainsize", nb301 = "epoch")
  archive = x$archive
  archive[, budget := round(exp(get(budget_param)))]
  stopifnot(all(archive$budget >= 1 & archive$budget <= 98))
  archive[, cumbudget := cumsum(budget)]
  archive[, repl := job$repl]
  archive[, id := job$id]
  archive
}, ids = jobs)
results_baseline = rbindlist(results_baseline$result)
saveRDS(results_baseline, paste0("/home/lschnei8/ofaatime/results/results_baseline_nb301_lcbench.rds"))

# baseline nb301 rbv2_super config
config = "rbv2_super"
jobs = data.table(job.id = intersect(reg$tags[tag == "baseline"]$job.id, intersect(reg$tags[tag == config]$job.id, reg$tags[tag == "nb301"]$job.id)))
jobs = findDone(jobs)
results_baseline = reduceResultsDataTable(fun = function(x, job) {
  budget_param = switch(job$instance$cfg, lcbench = "epoch", rbv2_super = "trainsize", nb301 = "epoch")
  archive = x$archive
  archive[, budget := round(exp(get(budget_param)))]
  stopifnot(all(archive$budget >= 1 & archive$budget <= 98))
  archive[, cumbudget := cumsum(budget)]
  archive[, repl := job$repl]
  archive[, id := job$id]
  archive
}, ids = jobs)
results_baseline = rbindlist(results_baseline$result)
saveRDS(results_baseline, paste0("/home/lschnei8/ofaatime/results/results_baseline_nb301_rbv2_super.rds"))

# ofaatime
ofaatime_tags = tags[- which(tags %in% c("baseline", "baseline_mu_32", "lcbench", "rbv2_super", "nb301"))]
for (config in c("lcbench", "rbv2_super")) {
  for (ofaatime_tag in ofaatime_tags) {
    jobs = data.table(job.id = intersect(tab[grepl(config, problem)][["job.id"]], reg$tags[tag %in% ofaatime_tag]$job.id))
    jobs = findDone(jobs)
    results = reduceResultsDataTable(fun = function(x, job) {
      ot = if (ofaatime_tag %in% c("surrogate_learner", "batch_method", "filter_with_max_budget")) {
        job$algo.pars[[ofaatime_tag]]
      } else {
        ofaatime_tag
      }
      budget_param = switch(job$instance$cfg, lcbench = "epoch", rbv2_super = "trainsize")
      archive = x$archive
      if (job$instance$cfg == "lcbench") {
        archive[, budget := round(exp(get(budget_param)))]
        stopifnot(all(archive$budget >= 1L & archive$budget <= 52L))
      } else if (job$instance$cfg == "rbv2_super") {
        archive[, budget := exp(get(budget_param))]
        stopifnot(all(archive$budget >= 0 & archive$budget <= 1))
      }
      archive[, cumbudget := cumsum(budget)]
      archive[, repl := job$repl]
      archive[, id := job$id]
      archive[, (ofaatime_tag) := ot]
      archive
    }, ids = jobs)
    results = rbindlist(results$result)
    saveRDS(results, paste0("/home/lschnei8/ofaatime/results/results_", ofaatime_tag, "_", config, ".rds"))
  }
}

for (config in c("lcbench", "rbv2_super")) {
  files = dir("results")[grepl(config, dir("results"))]
  stopifnot(length(files) == 11L)
  
  data = map(files, function(file) {
    tmp = readRDS(paste0("results/", file))
  })
  tmp = rbindlist(data, fill = TRUE)
  refs = setNames(tmp[, min(best), by = .(task, repl)], c("task", "repl", "min"))
  refs$range = tmp[, diff(range(best)), by = .(task, repl)]$V1
  
  data = map(data, function(x) {
    ot = colnames(x)[which(colnames(x) %in% ofaatime_tags)]
    by_vals =  c("eval_nr", "cumbudget")
    if (length(ot)) by_vals = c(by_vals, ot)
    tmp = x[refs, on = c("task", "repl")]
    tmp[, normalized_regret := (best - min) / range]
    agg = setNames(tmp[, mean(normalized_regret), by = by_vals], c(by_vals, "mean_normalized_regret"))
    agg$sd_normalized_regret = tmp[, sd(normalized_regret), by = by_vals]$V1
    agg$n = tmp[, length(normalized_regret), by = by_vals]$V1
    agg$se_normalized_regret = agg$sd_normalized_regret / sqrt(agg$n)
    agg
  })
  names(data) = gsub("results_|_lcbench|_rbv2_super|.rds", "", files)
  saveRDS(data, paste0("/home/lschnei8/ofaatime/results_agg/results_agg_", config, ".rds"))
}

