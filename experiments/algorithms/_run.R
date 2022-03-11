# Setup script for initially setting up the benchmarks 

library(batchtools)
setwd(here::here())


source("experiments/algorithms/_config.R")

lapply(packages, require, character.only = TRUE)

# --- 1. SETUP REGISTRY ---

reg = safeSetupRegistry(registry_name, OVERWRITE, packages, "experiments/algorithms/_config.R")

# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

for (i in seq_along(surr_data)) {
	addProblem(
		name = names(surr_data)[i], 
		data = surr_data[[i]], 
		fun = readProblem, # sets up the optimization instances 
		reg = reg, 
		seed = 123L
	)	
}

for (i in 1:length(ALGORITHMS)) {
  addAlgorithm(name = names(ALGORITHMS)[i], reg = reg, fun = ALGORITHMS[[i]]$fun)  
}

addExperiments(
  reg = reg, 
  prob.designs = pdes,
  algo.designs = des, 
  repls = REPLS)


# --- 3. RUN ALL EXPERIMENTS --- 

data.table::setDTthreads(1)

# Cluster configuration
resources.serial.default = list(
  walltime = 3600L * 24L * 4L, memory = 1024L * 2L,
  clusters = "serial", max.concurrent.jobs = 1000L 
)

reg = loadRegistry("reg", writeable = TRUE)

tab = summarizeExperiments(by = c("job.id", "problem", "algorithm"))

tosubmit = tab
tosubmit = ijoin(tosubmit, findNotDone())
submitJobs(tosubmit, resources = resources.serial.default)


# --- 4. REDUCE ALL RESULTS --- 
storepath = "experiments//results/results_comparison_baselines"

MAX_BUDGET_SEQUENTIAL = list(
  lcbench = 7 * 52 * 30,
  rbv2_super = 38 * 1 * 30,
  nb301 = 34 * 98 * 30
)

# 1) LCBENCH 

prob = "lcbench"

budget_max = MAX_BUDGET_SEQUENTIAL[[prob]]

for (algo in c("smac_full_budget", "randomsearch_full_budget", "mlr3hyperband", "hpbster_bohb")) {
	res = getResultsTable(tab, algo, prob, filedir, budget_max) 
	res = ijoin(tab, res)
	dir.create(file.path(storepath, prob, "sequential"), recursive = TRUE)
	saveRDS(res, file.path(storepath, prob, "sequential", paste0(algo, ".rds")))
}

# 2) RBV2_SUPER 

prob = "rbv2_super"

budget_max = MAX_BUDGET_SEQUENTIAL[[prob]]

for (algo in c("randomsearch_full_budget", "mlr3hyperband", "hpbster_bohb")) {
	res = getResultsTable(tab, algo, prob, filedir, budget_max) 
	res = ijoin(tab, res)
	dir.create(file.path(storepath, prob, "sequential"), recursive = TRUE)
	saveRDS(res, file.path(storepath, prob, "sequential", paste0(algo, ".rds")))
}

for (algo in c("smac_full_budget")) {
	res = getResultsTable(tab, algo, prob, filedir, budget_max) 
	res = ijoin(tab, res)
	dir.create(file.path(storepath, prob, "sequential"), recursive = TRUE)
	saveRDS(res, file.path(storepath, prob, "sequential", paste0(algo, ".rds")))
}


# 3) NB301 
prob = "nb301"

budget_max = MAX_BUDGET_SEQUENTIAL[[prob]]

for (algo in c("smac_full_budget", "randomsearch_full_budget", "mlr3hyperband", "hpbster_bohb")) {
	res = getResultsTable(tab, algo, prob, filedir, budget_max) 
	res = ijoin(tab, res)
	dir.create(file.path(storepath, prob, "sequential"), recursive = TRUE)
	saveRDS(res, file.path(storepath, prob, "sequential", paste0(algo, ".rds")))
}


