randomsearch = function(data, job, instance, full_budget) {

	ins = instance$ins$clone()

	# get the budget parameter
	budget_idx = which(ins$search_space$tags %in% c("budget", "fidelity"))
	budget_id = ins$search_space$ids()[budget_idx]

	trafo_old = ins$search_space$trafo

	if (full_budget) {
		# hand over a trafo that returns a constant value 
		ins$search_space$trafo = function(x, param_set) {
			if (!is.null(trafo_old))
				x = trafo_old(x, param_set)
			x$epoch = as.integer(ins$search_space$params$epoch$upper)
			x
		}
	} 

	# TODO: RANDOMSEARCH ALWAYS SHALL OPTIMIZE ON LOG-SCALE IF BUDGET IS NOT FULL
	
	start_t = Sys.time()
	opt('random_search')$optimize(ins)
	end_t = Sys.time()

	if (full_budget) {
		ins$archive$data$epoch = ins$search_space$params$epoch$upper
	}

    return(list(archive = ins$archive, runtime = end_t - start_t))
}
