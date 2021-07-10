mlrintermbo = function(data, job, instance, full_budget, surrogate, multi.point = 1L, log_scale = TRUE) {

	ins = instance$ins

	budget_idx = which(ins$search_space$tags %in% c("budget", "fidelity"))
	budget_id = ins$search_space$ids()[budget_idx]

	cids = ins$objective$codomain$ids()

	if (full_budget) {
		ts = as.integer(ins$search_space$params[[budget_id]]$upper)
		search_space = ins$search_space$clone()
		search_space$subset(setdiff(search_space$ids(), budget_id))
      	search_space$add(ParamInt$new(budget_id, lower = ts, upper = ts, default = ts, tags = "constant"))
		ins$search_space = search_space
	} else {
		if (log_scale) {
			domain = ins$objective$domain
			param_ids = domain$ids()			
			params_to_keep = param_ids[- budget_idx]
			search_space = ParamSet$new(domain$params[params_to_keep])

			budget_lower = domain$params[[budget_id]]$lower
			budget_upper = domain$params[[budget_id]]$upper

  			search_space = ParamSet$new(domain$params[params_to_keep])
  			search_space$add(ParamDbl$new(id = budget_id, lower = log(budget_lower), upper = log(budget_upper), tags = "budget"))
  			domain_tafo = domain$trafo
			search_space$trafo = function(x, param_set) {
				if (!is.null(domain_tafo)) x = domain_tafo(x, param_set)
				x[budget_id] = if (domain$params[[budget_id]]$class == "ParamInt") as.integer(exp(x[[budget_id]])) else exp(x[[budget_id]])
				x
			}
			search_space$deps = domain$deps

			ins = OptimInstanceSingleCrit$new(objective = ins$objective, search_space = search_space, terminator = trm("budget", budget = ins$terminator$param_set$values$budget, aggregate = function(x) sum(exp(as.numeric(x)))))
		}
	}

	optimizer = OptimizerInterMBO$new()
	
	optimizer$param_set$values$propose.points = multi.point

	start_t = Sys.time()
	optimizer$optimize(ins)
	end_t = Sys.time()

	# To make it uniform in the end we rename the budget
	archive = ins$archive$data
	names(archive)[which(names(archive) == budget_id)] = "budget"
	names(archive)[which(names(archive) %in% cids)] = ifelse(length(cids) == 1, "performance", paste0("loss_", seq_len(length(length(cids)))))

    return(list(archive = archive, runtime = end_t - start_t))
}
