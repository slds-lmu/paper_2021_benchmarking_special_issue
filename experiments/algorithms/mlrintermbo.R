mlrintermbo = function(data, job, instance, full_budget, surrogate) {

	# TODO: Algorithm Design of mlrmbo

	ins = instance$ins

	budget_idx = which(ins$search_space$tags %in% c("budget", "fidelity"))
	budget_id = ins$search_space$ids()[budget_idx]

	trafo_old = ins$search_space$trafo

	if (full_budget) {
		# hand over a trafo that returns a constant value 
		ins$search_space$trafo = function(x, param_set) {
			if (!is.null(trafo_old))
				x = trafo_old(x, param_set)
			x[[budget_idx]] = ins$search_space$params[[budget_idx]]$upper
			x
		}
	} 

	optimizer = OptimizerInterMBO$new()

	start_t = Sys.time()
	optimizer$optimize(ins)
	end_t = Sys.time()

    return(list(archive = ins$archive$data, runtime = end_t - start_t))
}
