mlr3hyperband = function(data, job, instance, eta) {

	# TODO: Algorithm Design of Hyperband

	optimizer = OptimizerHyperband$new()
	optimizer$param_set$values$eta = eta

	optimizer$optimize(instance$ins)

    return(list(archive = instance$archive))
}
