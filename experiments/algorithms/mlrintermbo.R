mlrintermbo = function(data, job, instance, full_budget, surrogate) {

	# TODO: Algorithm Design of mlrmbo

	trafo_old = ins$search_space$trafo

	if (full_budget) {
		# hand over a trafo that returns a constant value 
		ins$search_space$trafo = function(x, param_set) {
			x = trafo_old(x, param_set)
			x$epoch = ins$search_space$params$epoch$upper
			x
		}
	} 

	optimizer = OptimizerInterMBO$new()

	optimizer$optimize(instance$ins)

    return(list(archive = instance$ins$archive))
}
