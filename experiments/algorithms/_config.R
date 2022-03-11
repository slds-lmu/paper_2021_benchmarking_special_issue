# --- 0. SETUP ---

setwd(here::here())

source("experiments/algorithms/helper.R")

source("experiments/algorithms/randomsearch.R")
source("experiments/algorithms/mlr3hyperband.R")
source("experiments/algorithms/mlrintermbo.R")
source("experiments/algorithms/hpbster.R")
source("experiments/algorithms/smac_.R")

# Test setup with reduced budget (see below) or real setup 
SETUP = "REAL"

switch(SETUP, 
	"TEST" = {
		# overwrite registry
		OVERWRITE = TRUE
    	# registry name for storing files on drive 
		registry_name = "reg_temp"
		# replications
		REPLS = 10L 
		# Budget multiplier: d * budget_upper * B_MULTIPLIER
		B_MULTIPLIER = 1L
		# PARALELLIZATION FACTOR
		PARALLELIZATION = 4L
	},
	"REAL" = {
		# do never overwrite registry
		OVERWRITE = FALSE
		# termination criterion for each run
		registry_name = "reg"
		# replications
		REPLS = 30L * 32L 
		# Budget multiplier: d * budget_upper * B_MULTIPLIER
		B_MULTIPLIER = 30
		# PARALELLIZATION FACTOR
		PARALLELIZATION = 32L		
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

# remotes::install_github("mlr-org/ParamHelpers@handle_long_reqs")
lapply(packages, library, character.only = TRUE)


# --- 1. PROBLEM DESIGN ---

SURROGATE_LOCATION = c("experiments/problems/")

surrogates = c("lcbench", "rbv2_super", "nb301") 

# Downloads all surrogate data 
options(timeout=60^2) # set very high timeout to make sure everything is downloaded
surr_data = lapply(surrogates, function(surr) {
	
	cfg = cfgs(surr, workdir = SURROGATE_LOCATION)
	cfg$setup()
	# Store codomain manually for the python scripts
	saveRDS(cfg$param_set, file.path(SURROGATE_LOCATION, surr, "param_set"))

	return(cfg)
})

names(surr_data) = surrogates

pdes = list(# nb301 = data.table(objectives = c("val_accuracy")), 
			lcbench = data.table(objectives = c("val_cross_entropy")), 
			rbv2_super = data.table(objectives = c("logloss")),
			nb301 = data.table(objectives = c("val_accuracy")) 
			)


# Problem design is a data.frame: 
pdes = lapply(names(pdes), function(pid) {

	# Get the parameter that contains the task ids 
	ps = surr_data[[pid]]$param_set
	tid = ps$ids(tags = "task_id")

	if (length(tid) == 0) {
		tasks = NA
	} else {
		tasks = ps$params[[tid]]$levels
	}

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
    randomsearch_full_budget = list(fun = randomsearch, ades = data.table(full_budget = TRUE)), 
    mlr3hyperband = list(fun = mlr3hyperband, ades = data.table(eta = 3)), # log-scale not relevant
    hpbster_hb = list(fun = hpbster, ades = data.table(eta = 3, algorithm_type = "hb")), # log-scale not relevant
    hpbster_bohb = list(fun = hpbster, ades = data.table(eta = 3, algorithm_type = "bohb")), # log-scale not relevant
    smac_full_budget = list(fun = smac, ades = data.table(full_budget = TRUE))
)

des = lapply(ALGORITHMS, function(x) x$ades)

