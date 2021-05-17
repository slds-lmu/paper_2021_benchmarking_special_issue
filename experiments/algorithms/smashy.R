smashy = function(data, job, instance) {

	ins = instance$ins$clone()
	objective = ins$objective
	search_space_old = ins$search_space

	param_ids = search_space_old$ids()

	cids = ins$objective$codomain$ids()

	budget_idx = which(ins$search_space$tags %in% c("budget", "fidelity"))
	budget_id = param_ids[budget_idx]
	budget_lower = search_space_old$params[[budget_idx]]$lower
	budget_upper = search_space_old$params[[budget_idx]]$upper

	BUDGET_MAX = compute_total_budget(budget_upper, budget_lower, 2)
	params_to_keep = param_ids[- budget_idx]

	# Get all parameters except the budget parameter 
	search_space_new = ParamSet$new(search_space_old$params[params_to_keep])
	search_space_new$add(ParamDbl$new(id = budget_id, lower = log(budget_lower), upper = log(budget_upper), tags = "budget"))

	trafo_old = search_space_old$trafo

	search_space_new$trafo = function(x, param_set) {
	  if (!is.null(trafo_old))
		x = trafo_old(x, param_set)
	  x$epoch = as.integer(exp(x$epoch))
	  x
	}

	terminator = trm("budget", budget = BUDGET_MAX, aggregate = function(x) sum(exp(as.numeric(x))))

	ins = OptimInstanceSingleCrit$new(objective = objective, terminator = terminator, search_space = search_space_new)

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

	# To make it uniform in the end we rename the budget
	archive = ins$archive$data
	names(archive)[which(names(archive) == budget_id)] = "budget"
	names(archive)[which(names(archive) %in% cids)] = ifelse(length(cids) == 1, "performance", paste0("loss_", seq_len(length(length(cids)))))

    return(list(archive = archive, runtime = end_t - start_t))
}
