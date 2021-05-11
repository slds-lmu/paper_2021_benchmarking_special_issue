randomsearch = function(data, job, instance, full_budget) {

	ins = instance$ins$clone()

	# get the budget parameter
	budget_idx = which(ins$search_space$tags %in% c("budget", "fidelity"))
	budget_id = ins$search_space$ids()[budget_idx]

	trafo_old = ins$search_space$trafo

	if (full_budget) {
		# hand over a trafo that returns a constant value 
		ins$search_space$trafo = function(x, param_set) {
			x = trafo_old(x, param_set)
			x$epoch = ins$search_space$params$epoch$upper
			x
		}
	} # else {
	# 	budget_log = ParamDbl$new(id = budget_id, lower = log(ins$search_space$params$epoch$lower), upper = log(ins$search_space$params$epoch$upper), tag = c("budget"))
		
	# 	search_space_new = ins$search_space$clone()
	# 	search_space_new = search_space_new$subset(ins$search_space$ids()[- budget_idx])
	# 	search_space_new$add(budget_log)

	# 	# otherwise, we search on a log-scale 
	# 	search_space_new$trafo = function(x, param_set) {
	# 	  x$epoch = as.integer(round(exp(x$epoch)))
	# 	  x
	# 	}

	# 	ins$search_space = search_space_new
	# }

	# TODO: RANDOMSEARCH ALWAYS SHALL OPTIMIZE ON LOG-SCALE IF BUDGET IS NOT FULL
	opt('random_search')$optimize(ins)

	if (full_budget) {
		ins$archive$data$epoch = ins$search_space$params$epoch$upper
	}

    return(list(archive = ins$archive))
}
