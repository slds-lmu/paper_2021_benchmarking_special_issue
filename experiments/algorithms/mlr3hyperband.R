mlr3hyperband = function(data, job, instance) {

	optimizer = OptimizerHyperband$new()

	optimizer$optimize(instance)

    return(list(archive = instance$archive))
}
