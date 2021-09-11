# workdir where our surrogates are
WORKDIR = "experiments/problems/"

optimizer = OptimizerHyperband$new()

# Works for lcbench, but not for rbv2_super
cfg = cfgs("rbv2_super", workdir = WORKDIR)
obj = cfg$get_objective(task = "1040", target_variables = "logloss")   
# cfg = cfgs("lcbench", workdir = WORKDIR)
# obj = cfg$get_objective(task = "3945", target_variables = "val_cross_entropy")   

ins = OptimInstanceSingleCrit$new(
  objective = obj,
  terminator = trm("budget", budget = 50, aggregate = sum) 
)

optimizer$optimize(ins) # Throws an error 
