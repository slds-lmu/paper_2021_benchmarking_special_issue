# Setup script for initially setting up the benchmarks 

library(batchtools)

source("experiments/config.R")

lapply(packages, require, character.only = TRUE)

# --- 1. SETUP REGISTRY ---

reg = safeSetupRegistry(registry_name, OVERWRITE, packages, "experiments/config.R")

# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

for (i in seq_along(surr_data)) {
	addProblem(
		name = names(surr_data)[i], 
		data = surr_data[[i]], 
		fun = readProblem,
		reg = reg
	)	
}

for (i in 1:length(ALGORITHMS)) {
  addAlgorithm(name = names(ALGORITHMS)[i], reg = reg, fun = ALGORITHMS[[i]]$fun)  
}

addExperiments(
  reg = reg, 
  prob.designs = pdes,
  algo.designs = ades, 
  repls = REPLS)


