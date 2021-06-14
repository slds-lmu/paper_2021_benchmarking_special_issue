# workdir where our surrogates are
WORKDIR = "experiments/problems/"

# mlrintermbo
optimizer = OptimizerInterMBO$new()

# Works for lcbench
cfg = cfgs("lcbench", workdir = WORKDIR)
obj = cfg$get_objective(task = "3945", target_variables = "val_accuracy")   

ins = OptimInstanceSingleCrit$new(
  objective = obj,
  terminator = trm("budget", budget = 50, aggregate = sum) 
)

optimizer$optimize(ins) # Works well 


# Does not work for nb301
cfg = cfgs("nb301", workdir = WORKDIR)
obj = cfg$get_objective(target_variables = "val_accuracy")   

ins = OptimInstanceSingleCrit$new(
  objective = obj,
  terminator = trm("budget", budget = 10000, aggregate = sum) 
)

optimizer$optimize(ins)


# Also not if the surrogate model is changed (was my first assumption)	
cls = ins$search_space$class
is_mixed = any(!cls %in% c("ParamDbl", "ParamInt"))

if (is_mixed)
	surrogate = makeMlr3Surrogate(is.numeric = FALSE, is.noisy = TRUE, has.dependencies = FALSE) 

optimizer$param_set$values = list(surrogate.learner = surrogate)

optimizer$optimize(ins)
