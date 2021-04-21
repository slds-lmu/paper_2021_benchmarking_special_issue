randomsearch = function(data, job, instance) {

	# TODO: RANDOMSEARCH ALWAYS SHALL OPTIMIZE WITH FULL BUDGET

	opt('random_search')$optimize(instance)

    return(list(archive = instance$archive))
}
