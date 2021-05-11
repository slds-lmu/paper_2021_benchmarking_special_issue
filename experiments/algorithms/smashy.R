smashy = function(data, job, instance) {

	ins = instance$ins$clone()
	objective = ins$objective

	budget_idx = which(ins$search_space$tags %in% c("budget", "fidelity"))
	budget_id = ins$search_space$ids()[budget_idx]

	search_space_old = ins$search_space
	param_ids = search_space_old$ids()

	# Smashy needs another instance 
	budget_lower = search_space_old$params[[budget_idx]]$lower
	budget_upper = search_space_old$params[[budget_idx]]$upper

	search_space_new = lapply(names(search_space_old$params), function(x) {
		if (x == budget_id) {
			out = to_tune(p_int(budget_lower, budget_upper, logscale = TRUE, tags = "budget"))
		} else {
			out = to_tune()
		}
	})

	names(search_space_new) = names(search_space_old$params)

	search_space = objective$domain$search_space(search_space_new)

	trafo_old = ins$search_space$trafo

	search_space$trafo = function(x, param_set) {
	  x = trafo_old(x, param_set)
	  x[[budget_idx]] = as.integer(exp(x[[budget_idx]]))
	  x
	}

	# Adapt terminator (transformation due to log-scale in budget)
	# d * 100 * lbmax
	terminator = trm("budget", budget = length(param_ids) * BUDGET_MAX_FACTOR * budget_upper, aggregate = function(x) sum(exp(as.numeric(x))))

	ins = OptimInstanceSingleCrit$new(objective = objective, terminator = terminator, search_space = search_space)

	scalor = scl("one") # scl("nondom") for multi-objective
	selector = sel("best", scalor)
	surrogate_learner = mlr3::lrn("regr.kknn", fallback = mlr3::lrn("regr.featureless"), encapsulate = c(train = "evaluate", predict = "evaluate"))
	filtor = ftr("surprog", surrogate_learner = surrogate_learner, surrogate_selector = selector,
	  filter.pool_factor = 3, filter.pool_factor_last =  4.4)
	interleaving_filtor = ftr("maybe", filtor, p = 0.83, random_choice = FALSE)

	optimizer = opt("sumohb", 
	  filtor = interleaving_filtor, 
	  selector = selector, 
	  mu = 55,
	  survival_fraction = 0.09,
	  sampling = paradox::generate_design_random, 
	  fidelity_steps = 109, # ~ (52 - 1)/exp(-0.75) + 1
	  filter_with_max_budget = TRUE
	)

	start_t = Sys.time()
	optimizer$optimize(ins)
	end_t = Sys.time()

    return(list(archive = ins$archive, runtime = end_t - start_t))
}
