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