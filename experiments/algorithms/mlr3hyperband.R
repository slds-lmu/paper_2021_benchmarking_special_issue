mlr3hyperband = function(data, job, instance, eta) {

	# TODO: Algorithm Design of Hyperband

	optimizer = OptimizerHyperband$new()
	optimizer$param_set$values$eta = eta

	start_t = Sys.time()
	optimizer$optimize(instance$ins)
	end_t = Sys.time()

    return(list(archive = instance$ins$archive, runtime = end_t - start_t))
}
