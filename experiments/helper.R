safeSetupRegistry = function(registry_name, overwrite, packages, def) {

  if (!dir.exists(dirname(registry_name)))
    dir.create(dirname(registry_name), recursive = TRUE)

  if (file.exists(registry_name)) {
    if (overwrite) {
      unlink(registry_name, recursive = TRUE)
      reg = makeExperimentRegistry(file.dir = registry_name, seed = 123L,
        packages = packages, source = def)
    } else {
      reg = loadRegistry(registry_name, writeable = TRUE)
    }
  } else {
      reg = makeExperimentRegistry(file.dir = registry_name, seed = 123L,
        packages = packages, source = def)
  }
  return(reg)
}


convertParamSetConfigspace = function(cfg, path) {

  library(reticulate)
  cs = import("ConfigSpace")
  json = import("ConfigSpace")$read_and_write$json

  objective = cfg$get_objective()

  config_space = cs$ConfigurationSpace()
  domain = objective$domain

  # TODO: TRAFO! 

  for (par in domain$params) {

    is_budget = par$id == "budget"

    if (par$id == "budget") {
      new_id = "fidelity"
    } else {
      new_id = par$id
    }

    if (par$class == "ParamFct") {
      config_space$add_hyperparameter(cs$CategoricalHyperparameter(
        name = new_id, choices = par$levels))
    }

    if (par$class == "ParamInt") {
      config_space$add_hyperparameter(cs$UniformIntegerHyperparameter(
        name = new_id, lower = par$lower, upper = par$upper))
    }

    if (par$class == "ParamDbl") {
      config_space$add_hyperparameter(cs$UniformIntegerHyperparameter(
        name = new_id, lower = par$lower, upper = par$upper, trafo = trafo))
    }

  }

  py <- import_builtins()

  with(py$open(file.path(path, "domain.json"), "w") %as% file, {
    file$write(json$write(config_space))
  })  

}



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
