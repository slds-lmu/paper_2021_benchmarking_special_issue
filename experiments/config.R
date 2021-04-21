# --- 0. SETUP ---

source("experiments/helper.R")

source("experiments/algorithms/randomsearch.R")
source("experiments/algorithms/mlr3hyperband.R")


# - test or real setup for better testing - 
SETUP = "TEST"

switch(SETUP, 
	"TEST" = {
		# overwrite registry
		OVERWRITE = TRUE
		# termination criterion for each run
		RUNTIME_MAX = 60L
    # registry name for storing files on drive 
		registry_name = "reg_temp"
		# replications
		REPLS = 1L 
	},
	"REAL" = {
		# overwrite registry?
		OVERWRITE = FALSE
		# termination criterion for each run
		RUNTIME_MAX = 302400
    	# registry name for storing files on drive     
		registry_name = "reg"
		# replications
		REPLS = 30L
	}
)

# - packages - 
packages = c(
  "batchtools",  
  "data.table",
  "reticulate", 
  "mfsurrogates", 
  "paradox", 
  "checkmate", 
  "bbotk", 
  "data.table",
  "mlr3hyperband"
) 

lapply(packages, library, character.only = TRUE)


# --- 1. PROBLEM DESIGN ---

SURROGATE_LOCATION = c("experiments/problems/")

surrogates = c("nb301", "branin")# , "lcbench")# , "rbv2_aknn")

# Data: Surrogate instances provided by Flo and Lennart 
surr_data = lapply(surrogates, function(surr) {
	
	cfg = cfgs(surr, workdir = SURROGATE_LOCATION)
	cfg$setup()

	return(cfg)
})

names(surr_data) = surrogates


# Problem design contains the task, objectives (SO / MO)
pdes = lapply(surr_data, function(d) {

	## Read out the tasks 
	tasks = d$param_set$params$OpenML_task_id$levels

	if (is.null(tasks))
		tasks = NA

	## Read out the objectives (single- and multicrit) 
	codomain = d$codomain
	cdids = codomain$ids()

	if (length(cdids) >= 2) {
		objdf = data.table(type = c("SO", "MO"), objectives = list(c(cdids[1]), c(cdids[1:2])))
	}
	if (length(cdids) == 1) {
		objdf = data.table(type = c("SO"), objectives = list(c(cdids[1])))
	}
	
	df = merge(x = tasks, y = objdf)
	names(df)[1] = "task"

	return(df)
})

names(pdes) = surrogates


readProblem = function(data, job, task, type, objectives, ...) {

	# Get the objective function
	# For branin the interface is slghtly different 
	if (data$id != "Branin")
		obj = data$get_objective(target_variables = objectives)
	else 
		obj = data$get_objective()		


	if (type == "SO") {
		ins = OptimInstanceSingleCrit$new(
		  objective = obj,
		  terminator = trm("evals", n_evals = 30L) # TODO: Budget Terminator 
		)
	} 

	if (type == "MO") {
		ins = OptimInstanceMultiCrit$new(
		  objective = obj,
		  terminator = trm("evals", n_evals = 30L) # TODO: Budget Terminator 
		)		
	}

	return(ins)
}


# --- 2. ALGORITHM DESIGN ---

# TODO: Specify proper algorithm design 
ALGORITHMS = list(
    randomsearch = list(fun = randomsearch, ades = data.table()), 
    mlr3hyperband = list(fun = mlr3hyperband, ades = data.table())
)

ades = lapply(ALGORITHMS, function(x) x$ades)