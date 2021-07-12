library(tictoc)
library(profvis)

source("./irace/optimization.R")

eval = function(xs, irace_instance) {
  # stop time for irace
  t0 = Sys.time()

  # get surrogate model
  cfg = cfgs(irace_instance$cfg, workdir = workdir)
  objective = cfg$get_objective(task = irace_instance$level, target_variables = irace_instance$targets)
  
  # create search space
  domain = objective$domain
  param_ids = domain$ids()
  budget_idx = which(domain$tags %in% c("budget", "fidelity"))
  budget_id = param_ids[budget_idx]
  budget_lower = irace_instance$lower
  budget_upper = irace_instance$upper
  params_to_keep = param_ids[- budget_idx]

  search_space = ParamSet$new(domain$params[params_to_keep])
  search_space$add(ParamDbl$new(id = budget_id, lower = log(budget_lower), upper = log(budget_upper), tags = "budget"))
  domain_tafo = domain$trafo
  search_space$trafo = function(x, param_set) {
    if (!is.null(domain_tafo)) x = domain_tafo(x, param_set)
    x[budget_id] = if (domain$params[[budget_id]]$class == "ParamInt") as.integer(exp(x[[budget_id]])) else exp(x[[budget_id]])
    x
  }
  search_space$deps = domain$deps

  # calculate smashy budget
  budget_limit = 40 * 30 * 0.1

  # call smashy with configuration parameter in xs
  instance = mlr3misc::invoke(opt_objective, objective = objective, budget_limit = budget_limit, 
    search_space = search_space, .args = xs)

  cols_y = instance$archive$cols_y

  # the objective is maximized
  objective_multiplicator = unname(instance$objective_multiplicator) * -1

  # filter for experiments with highest budget only
  if (TRUE) instance$archive$data = instance$archive$data[get(budget_id) == max(get(budget_id)), ]

  # apply target_trafo
  if (!is.null(irace_instance$target_trafo)) instance$archive$data = irace_instance$target_trafo(instance$archive$data)

 
  y = if (instance$objective$codomain$length > 1) {
    # hypervolume
    mat = as.matrix(instance$archive$data[, cols_y, with = FALSE])
    mat = sweep(mat, 2, objective_multiplicator, `*`)
    miesmuschel:::domhv(mat, nadir = irace_instance$nadir * objective_multiplicator) 
  } else {
    # best performance
    as.numeric(instance$archive$best()[, cols_y, with = FALSE]) * objective_multiplicator
  }

  list(y = y, time = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}


# multi
# 450 Seconds
set.seed(7345)
workdir = "./irace/data/surrogates"

# takes 3h on cluster
xdt = as.data.table(structure(list(budget_log_step = -0.1019, mu = 5.0882, 
    sample = "random", survival_fraction = 0.6229, filter_algorithm = "progressive", 
    surrogate_learner = "knn", filter_with_max_budget = TRUE, 
    filter_factor_first = 1.9189, filter_factor_last = 4.9117, 
    filter_select_per_tournament = 0.9473, random_interleave_fraction = 0.7033, 
    filter_factor_first.end = 5.9871, filter_factor_last.end = 1.5959, 
    filter_select_per_tournament.end = 0.3562, random_interleave_fraction.end = 0.8914, 
    random_interleave_random = FALSE), row.names = "13", class = "data.frame"))

design = Design$new(meta_search_space, xdt, remove_dupl = FALSE)
xs = design$transpose(trafo = TRUE)[[1]]
irace_instance = list(
  cfg = "rbv2_super",
  level = "458",
  lower = 3^(-3),
  upper = 1,
  target_trafo = function(data) {
    data$timepredict = log(data$timepredict)
    data
  },
  nadir = c(2, 0.6961427),
  targets = c("mmce", "timepredict")
)

tic()
res = eval(xs, irace_instance)
toc()

# single
# 9 seconds 

set.seed(7345)
irace_instance = list(
  cfg = "rbv2_super",
  level = "458",
  lower = 3^(-3),
  upper = 1,
  targets = "logloss"
)

tic()
res = eval(xs, irace_instance)
toc()