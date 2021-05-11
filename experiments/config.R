# --- 0. SETUP ---

source("experiments/helper.R")

source("experiments/algorithms/randomsearch.R")
source("experiments/algorithms/mlr3hyperband.R")
source("experiments/algorithms/mlrintermbo.R")
source("experiments/algorithms/smashy.R")

# source("experiments/algorithms/bohb.R")


# - test or real setup for better testing - 
SETUP = "TEST"

switch(SETUP, 
	"TEST" = {
		# overwrite registry
		OVERWRITE = TRUE
		# termination criterion for each run
		BUDGET_MAX_FACTOR = 5L 
    	# registry name for storing files on drive 
		registry_name = "reg_temp"
		# replications
		REPLS = 1L 
	},
	"REAL" = {
		# overwrite registry?
		OVERWRITE = FALSE
		# termination criterion for each run
		BUDGET_MAX_FACTOR = 100L # Budget is lbmax * 100 * d
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
  "mlr3hyperband", 
  "mlrintermbo", 
  "miesmuschel",
  "mlr3learners"
) 

lapply(packages, library, character.only = TRUE)



# --- 1. PROBLEM DESIGN ---

SURROGATE_LOCATION = c("experiments/problems/")

surrogates = c("nb301", "lcbench") # , "fcnet")

# Data: Surrogate instances provided by Flo and Lennart 
surr_data = lapply(surrogates, function(surr) {
	
	cfg = cfgs(surr, workdir = SURROGATE_LOCATION)
	cfg$setup()

	# path = file.path(cfg$subdir, "domain.json")

	# WILL BE DONE BY LENNART AND FLO
	# if (surr == "branin")
	# 	path = file.path(cfg$workdir, "branin", "domain.json")

	# also transform and save the domain (needed for python calls)
	# if (!file.exists(path))
	# 	convertParamSetConfigspace(cfg = cfg, path = path)

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
		objdf = data.table(objectives = list(c(cdids[1]), c(cdids[1:2])))
	}
	if (length(cdids) == 1) {
		objdf = data.table(objectives = list(c(cdids[1])), nobjectives = 1)
	}
	
	df = merge(x = tasks, y = objdf)
	names(df)[1] = "task"

	df$nobjectives = lapply(df$objectives, length)
	df$objectives_scalar = lapply(df$objectives, function(x) paste(x, collapse = ", "))

	return(df)
})

names(pdes) = surrogates


readProblem = function(data, job, task, objectives, ...) {

	nobjectives = length(objectives)

	dom = data$param_set

	# Get the upper budget limit
	param_ids = dom$ids()
	budget_idx = which(dom$tags %in% c("budget", "fidelity"))
	budget_lower = dom$lower[budget_idx]
	budget_upper = dom$upper[budget_idx]

	if (length(budget_idx) > 1) {
		# modify the param_set
		for (i in seq(2, length(budget_idx))) {
			data$param_set$params[[param_ids[budget_idx[i]]]]$tags = paste0("budget_", i)
		}
	}

	# We give a total budget of lbmax * 100 * d
	BUDGET_MAX = BUDGET_MAX_FACTOR * budget_upper * length(param_ids)

	# Get the objective function
	# For branin the interface is slghtly different 

	if (is.na(task)) {
		obj = data$get_objective(target_variables = objectives)		
	} else {
		obj = data$get_objective(task = task, target_variables = objectives)	
	}		
		
	if (nobjectives == 1) {
		ins = OptimInstanceSingleCrit$new(
		  objective = obj,
		  terminator = trm("budget", budget = BUDGET_MAX, aggregate = sum) 
		)
	} 

	if (nobjectives > 1) {
		ins = OptimInstanceMultiCrit$new(
		  objective = obj,
		  terminator = trm("budget", budget = BUDGET_MAX, aggregate = sum) 
		)		
	}


	return(list(name = data$model_name, ins = ins, task = task)) 
}


# --- 2. ALGORITHM DESIGN ---

# TODO: Specify proper algorithm design --> Ablation analysis 
ALGORITHMS = list(
    randomsearch = list(fun = randomsearch, ades = data.table(full_budget = c(FALSE, TRUE))), 
    mlr3hyperband = list(fun = mlr3hyperband, ades = data.table(eta = c(3))), 
    mlrintermbo = list(fun = mlrintermbo, ades = data.table(full_budget = c(FALSE, TRUE), surrogate = "regr.randomForest")), 
    smashy = list(fun = smashy, ades = data.table()) 
)

ades = lapply(ALGORITHMS, function(x) x$ades)


# instance = readProblem(surr_data[["nb301"]], 1, NA, objectives = c("val_accuracy"))
# instance = readProblem(surr_data[["lcbench"]], 1, "3945", objectives = c("val_accuracy"))

# Problems: 
# - Trafos abändern --> LCBench
# - Budget bei Smashy? 