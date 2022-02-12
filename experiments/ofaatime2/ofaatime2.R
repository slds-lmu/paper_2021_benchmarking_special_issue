################################################################################# Batchtools Code for Ofaatime ##################################################################################################

library(data.table)
setDTthreads(1L)
library(mfsurrogates)
library(mlr3misc)
library(lgr)

RhpcBLASctl::blas_set_num_threads(1L)
RhpcBLASctl::omp_set_num_threads(1L)

root = here::here()
workdir = file.path(root, "experiments/ofaatime2/data/surrogates")
source(file.path(root, "experiments/ofaatime2/optim2.R"))

eval_ = function(job, data, instance, budget_factor = 30L, ...) {
  data.table::setDTthreads(1L)
  RhpcBLASctl::blas_set_num_threads(1L)
  RhpcBLASctl::omp_set_num_threads(1L)

  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  root = here::here()
  workdir = file.path(root, "experiments/ofaatime2/data/surrogates")

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
reg = makeExperimentRegistry(file.dir = "/gscratch/lschnei8/registry_ofaatime_15_09", source = file.path(root, "experiments/ofaatime2/optim2.R"))
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

source("ablation_prepare.R")

repls = 30L

# rq1 experiments
for (i in seq_len(nrow(rq.1.tbl))) {
  ids = addExperiments(
    prob.designs = prob_designs,
    algo.designs = list(eval_ = rq.1.tbl[i, ]),
    repls = repls
  )
  addJobTags(ids, paste0("rq1_", i))
}

# rq4 experiments
for (i in seq_len(nrow(rq.4.tbl))) {
  ids = addExperiments(
    prob.designs = prob_designs,
    algo.designs = list(eval_ = rq.4.tbl[i, ]),
    repls = repls
  )
  addJobTags(ids, paste0("rq4_", i))
}

# rq5a experiments
for (i in seq_len(nrow(rq.5a.tbl))) {
  ids = addExperiments(
    prob.designs = prob_designs,
    algo.designs = list(eval_ = rq.5a.tbl[i, ]),
    repls = repls
  )
  addJobTags(ids, paste0("rq5a_", i))
}
for (i in seq_len(nrow(rq.5a.tbl.cond))) {
  ids = addExperiments(
    prob.designs = prob_designs,
    algo.designs = list(eval_ = rq.5a.tbl.cond[i, ]),
    repls = repls
  )
  addJobTags(ids, paste0("rq5a_cond_", i))
}

# rq5b experiments
for (i in seq_len(nrow(rq.5b.tbl))) {
  ids = addExperiments(
    prob.designs = prob_designs,
    algo.designs = list(eval_ = rq.5b.tbl[i, ]),
    repls = repls
  )
  addJobTags(ids, paste0("rq5b_", i))
}
for (i in seq_len(nrow(rq.5b.tbl.cond))) {
  ids = addExperiments(
    prob.designs = prob_designs,
    algo.designs = list(eval_ = rq.5b.tbl.cond[i, ]),
    repls = repls
  )
  addJobTags(ids, paste0("rq5b_cond_", i))
}

# rq6 experiments
for (i in seq_len(nrow(rq.6.tbl))) {
  ids = addExperiments(
    prob.designs = prob_designs,
    algo.designs = list(eval_ = rq.6.tbl[i, ]),
    repls = repls
  )
  addJobTags(ids, paste0("rq6_", i))
}

# rq7 experiments
prob_designs_rq7 = map(prob_designs, function(pd) {
  pd$multiplier = 8
  pd
})
for (i in seq_len(nrow(rq.7.tbl.BUDGETFACTOR))) {
  ids = addExperiments(
    prob.designs = prob_designs_rq7,
    algo.designs = list(eval_ = rq.7.tbl.BUDGETFACTOR[i, ]),
    repls = repls
  )
  addJobTags(ids, paste0("rq7_", i))
}

# rq6_fix experiments
for (i in seq_len(nrow(rq.6.tbl_fix))) {
  ids = addExperiments(
    prob.designs = prob_designs,
    algo.designs = list(eval_ = rq.6.tbl_fix[i, ]),
    repls = repls
  )
  addJobTags(ids, paste0("rq6_fix_", i))
}


tab = getJobTable()
rq1jobs = findJobs(ids = tab[grepl("rq1_", tab$tags)]$job.id)
rq1jobs[, chunk := batchtools::chunk(job.id, chunk.size = 10L)]
rq4jobs = findJobs(ids = tab[grepl("rq4_", tab$tags)]$job.id)
rq4jobs[, chunk := batchtools::chunk(job.id, chunk.size = 10L)]
rq5ajobs = findJobs(ids = tab[grepl("rq5a_", tab$tags)]$job.id)
rq5ajobs[, chunk := batchtools::chunk(job.id, chunk.size = 10L)]
rq5bjobs = findJobs(ids = tab[grepl("rq5b_", tab$tags)]$job.id)
rq5bjobs[, chunk := batchtools::chunk(job.id, chunk.size = 10L)]
rq6jobs = findJobs(ids = tab[grepl("rq6_", tab$tags)]$job.id)
rq6jobs[, chunk := batchtools::chunk(job.id, chunk.size = 10L)]
rq6fixjobs = findJobs(ids = tab[grepl("rq6_fix_", tab$tags)]$job.id)
rq6fixjobs[, chunk := batchtools::chunk(job.id, chunk.size = 10L)]
rq7jobs = findJobs(ids = tab[grepl("rq7_", tab$tags)]$job.id)

# standard resources used to submit jobs to cluster
resources.serial.default = list(
  walltime = 3600L * 12L, memory = 1024L * 2L, max.concurrent.jobs = 9999L
)

# large resources used to submit jobs to cluster
resources.serial.long = list(
  walltime = 3600L * 24L, memory = 1024L * 8L, max.concurrent.jobs = 9999L
)

submitJobs(rq1jobs, resources = resources.serial.default)
submitJobs(rq4jobs, resources = resources.serial.default)
submitJobs(rq5ajobs, resources = resources.serial.default)
submitJobs(rq5bjobs, resources = resources.serial.default)
submitJobs(rq6jobs, resources = resources.serial.default)
submitJobs(rq6fixjobs, resources = resources.serial.default)
submitJobs(rq7jobs, resources = resources.serial.long)

jobs = rbind(findExpired(), findErrors())
jobs[, chunk := batchtools::chunk(job.id, chunk.size = 5L)]
submitJobs(jobs, resources = resources.serial.default)



################################################################################# Collect Results ##################################################################################################

library(data.table)
library(batchtools)
library(mlr3misc)

source("ablation_prepare.R")

reg = loadRegistry(file.dir = "/gscratch/lschnei8/registry_ofaatime_15_09")
tags = batchtools::getUsedJobTags()
tab = getJobTable()

save_results = function(tbl, rqx) {
  file = paste0("/home/lschnei8/ofaatime/results_new/results_", rqx, ".rds")
  results = map_dtr(seq_len(nrow(tbl)), function(i) {
    tagx = paste0(rqx, "_", i)
    jobs = data.table(job.id = reg$tags[tag == tagx]$job.id)
    gc()
    cat(dim(jobs)[1L], rqx, i, "\n")
    tmp = reduceResultsDataTable(fun = function(x, job) {q
      budget_param = switch(job$instance$cfg, lcbench = "epoch", rbv2_super = "trainsize", nb301 = "epoch")
      archive = x$archive
      if (job$instance$cfg == "lcbench") {
        archive[, budget := round(exp(get(budget_param)))]
        stopifnot(all(archive$budget >= 1L & archive$budget <= 52L))
      } else if (job$instance$cfg == "rbv2_super") {
        archive[, budget := exp(get(budget_param))]
        stopifnot(all(archive$budget >= 0 & archive$budget <= 1))
      } else if (job$instance$cfg == "nb301") {
        archive[, budget := round(exp(get(budget_param)))]
        stopifnot(all(archive$budget >= 1L & archive$budget <= 98L))
      }
      archive[, cumbudget := cumsum(budget)]
      archive[, repl := job$repl]
      archive[, id := job$id]
      archive[, rq := rqx]
      archive[, rqn := i]
    }, ids = jobs)
    rbindlist(tmp$result, fill = TRUE)
  }, .fill = TRUE)
  saveRDS(results, file)
}

# FIXME: rq.6.tbl_fix
experiments = list(
  tbl = list(rq.1.tbl, rq.4.tbl, rq.5a.tbl, rq.5a.tbl.cond, rq.5b.tbl, rq.5b.tbl.cond, rq.6.tbl, rq.6.tbl_fix, rq.7.tbl.BUDGETFACTOR),
  rqx = list("rq1", "rq4", "rq5a", "rq5a_cond", "rq5b", "rq5b_cond", "rq6", "rq6_fix", "rq7")
)

pmap(experiments, .f = save_results)
saveRDS(experiments, "/home/lschnei8/ofaatime/results_new/experiments.rds")

#rq.1.tbl
#rq.4.tbl
#rq.5a.tbl
#rq.5a.tbl.cond
#rq.5b.tbl
#rq.5b.tbl.cond
#rq.6.tbl
#rq.6.tbl_fix
#rq.7.tbl.BUDGETFACTOR

