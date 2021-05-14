# --- 0. SETUP ---

setwd(here::here())

source("experiments/helper.R")

source("experiments/algorithms/randomsearch.R")
source("experiments/algorithms/mlr3hyperband.R")
source("experiments/algorithms/mlrintermbo.R")
source("experiments/algorithms/smashy.R")
source("experiments/algorithms/bohb.R")

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
	},
	"REAL" = {
		# do never overwrite registry
		OVERWRITE = FALSE
		# termination criterion for each run
		registry_name = "reg"
		# replications
		REPLS = 30L 
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
  "mlr3learners"
) 

lapply(packages, library, character.only = TRUE)

# --- 1. PROBLEM DESIGN ---

SURROGATE_LOCATION = c("experiments/problems/")

# TODO: Include randombot 
surrogates = c("nb301", "lcbench")

# Downloads all surrogate data 
surr_data = lapply(surrogates, function(surr) {
	
	cfg = cfgs(surr, workdir = SURROGATE_LOCATION)
	cfg$setup()

	return(cfg)
})

names(surr_data) = surrogates


# Problem design is a data.frame: 
# | tasks | objectives | 
pdes = lapply(surr_data, function(d) {

	## Read out the tasks 
	tasks = d$param_set$params$OpenML_task_id$levels

	if (is.null(tasks))
		tasks = NA

	## Read out the objectives (single- and multicrit) 
	codomain = d$codomain
	cdids = codomain$ids()

	if (length(cdids) >= 2) {
		objdf = data.table(objectives = list(c(cdids[1]), c(cdids[1:2])))
	}
	if (length(cdids) == 1) {
		objdf = data.table(objectives = list(c(cdids[1])))
	}
	
	df = merge(x = tasks, y = objdf)
	names(df)[1] = "task"

	# Redundant information, but makes it easier to filter the summarizeExperiments() - table later on
	df$nobjectives = lapply(df$objectives, length)
	df$objectives_scalar = lapply(df$objectives, function(x) paste(x, collapse = ", "))

	return(df)
})

names(pdes) = surrogates



# --- 2. ALGORITHM DESIGN ---

# TODO: Re-evaluate algorithm design 
# TODO: Ablation analysis 
ALGORITHMS = list(
    randomsearch = list(fun = randomsearch, ades = data.table(full_budget = c(FALSE, TRUE))), 
    mlr3hyperband = list(fun = mlr3hyperband, ades = data.table(eta = 2)), 
    mlrintermbo = list(fun = mlrintermbo, ades = data.table(full_budget = c(FALSE, TRUE), surrogate = "regr.randomForest")), 
    smashy = list(fun = smashy, ades = data.table()), 
    bohb = list(fun = bohb, ades = data.table(eta = 2)) 
)

ades = lapply(ALGORITHMS, function(x) x$ades)


# instance = readProblem(surr_data[["nb301"]], 1, NA, objectives = c("val_accuracy"))
# instance = readProblem(surr_data[["lcbench"]], 1, "3945", objectives = c("val_accuracy"))
