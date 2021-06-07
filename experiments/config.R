# --- 0. SETUP ---

setwd(here::here())

source("experiments/helper.R")

source("experiments/algorithms/randomsearch.R")
source("experiments/algorithms/mlr3hyperband.R")
source("experiments/algorithms/mlrintermbo.R")
source("experiments/algorithms/smashy.R")
source("experiments/algorithms/hpbster.R")

# Test setup with reduced budget (see below) or real setup 
SETUP = "REAL"

switch(SETUP, 
	"TEST" = {
		# overwrite registry
		OVERWRITE = TRUE
    	# registry name for storing files on drive 
		registry_name = "reg_temp"
		# replications
		REPLS = 1L 
		# Budget multiplier: d * budget_upper * B_MULTIPLIER
		B_MULTIPLIER = 5
	},
	"REAL" = {
		# do never overwrite registry
		OVERWRITE = FALSE
		# termination criterion for each run
		registry_name = "reg"
		# replications
		REPLS = 30L 
		# Budget multiplier: d * budget_upper * B_MULTIPLIER
		B_MULTIPLIER = 30
	}
)

# Load packages
packages = c(
  "batchtools",  
  "data.table",
  "reticulate", 
  "mfsurrogates", 
  "paradox", 
  "checkmate", 
  "bbotk", 
  "mlr3hyperband", 
  "mlrintermbo", 
  "miesmuschel",
  "mlr3learners",
  "mlr3pipelines"
) 

lapply(packages, library, character.only = TRUE)

# --- 1. PROBLEM DESIGN ---

SURROGATE_LOCATION = c("experiments/problems/")

# TODO: Include randombot 
surrogates = c("nb301", "lcbench", "rbv2_super")

# Downloads all surrogate data 
options(timeout=60^2) # set very high timeout to make sure everything is downloaded
surr_data = lapply(surrogates, function(surr) {
	
	cfg = cfgs(surr, workdir = SURROGATE_LOCATION)
	cfg$setup(force = FALSE)

	# Store codomain manually for the python scripts
	saveRDS(cfg$codomain, file.path(SURROGATE_LOCATION, surr, "codomain.rds"))

	return(cfg)
})

names(surr_data) = surrogates

# TODO: Optimize cross entropy for nb301? 
pdes = list(nb301 = data.table(objectives = c("val_accuracy")), 
			lcbench = data.table(objectives = c("val_accuracy")), 
			rbv2_super = data.table(objectives = c("logloss"))
			)

# Problem design is a data.frame: 
pdes = lapply(names(pdes), function(pid) {

	## Read out the tasks 
	tasks = surr_data[[pid]]$param_set$params$OpenML_task_id$levels

	if (is.null(tasks))
		tasks = NA
	
	df = merge(x = tasks, y = pdes[[pid]])
	names(df)[1] = "task"

	# Redundant information, but makes it easier to filter the summarizeExperiments() - table later on
	df$nobjectives = lapply(df$objectives, length)
	df$objectives_scalar = lapply(df$objectives, function(x) paste(x, collapse = ", "))

	return(as.data.table(df))
})

names(pdes) = surrogates


# --- 2. ALGORITHM DESIGN ---

ALGORITHMS = list(
    randomsearch = list(fun = randomsearch, ades = data.table(full_budget = c(FALSE, TRUE))), 
    mlr3hyperband = list(fun = mlr3hyperband, ades = data.table(eta = 3)), 
    mlrintermbo = list(fun = mlrintermbo, ades = data.table(full_budget = c(FALSE, TRUE, FALSE, TRUE), multi.point = c(1L, 1L, 32L, 32L))), 
    # smashy = list(fun = smashy, ades = data.table()), 
    hpbster = list(fun = hpbster, ades = data.table(eta = 3, algorithm_type = c("hb", "bohb")))
)

des = lapply(ALGORITHMS, function(x) x$ades)


# instance = readProblem(surr_data[["nb301"]], 1, NA, objectives = c("val_accuracy"))
# instance = readProblem(surr_data[["lcbench"]], 1, "3945", objectives = c("val_accuracy"))

# NB301 takes approx. 44 minutes 

# TODO: mlrintermbo does not work on nb301
# TODO: if lower boundary is 0, it must be 0.01 (--> rbv2)