randomsearch = function(data, job, instance, full_budget) {

	ins = instance$ins$clone()

	# get the budget parameter
	budget_idx = which(ins$search_space$tags %in% c("budget", "fidelity"))
	budget_id = ins$search_space$ids()[budget_idx]

	cids = ins$objective$codomain$ids()

	if (full_budget) {
		ts = list(as.integer(ins$search_space$params$epoch$upper))
		names(ts) = budget_id
		ins$search_space$values = c(ins$search_space$values, ts)
	} 

	# TODO: RANDOMSEARCH ALWAYS SHALL OPTIMIZE ON LOG-SCALE IF BUDGET IS NOT FULL
	start_t = Sys.time()
	opt('random_search')$optimize(ins)
	end_t = Sys.time()

	# To make it uniform in the end we rename the budget
	archive = ins$archive$data
	names(archive)[which(names(archive) == budget_id)] = "budget"
	names(archive)[which(names(archive) %in% cids)] = ifelse(length(cids) == 1, "performance", paste0("loss_", seq_len(length(length(cids)))))

    return(list(archive = archive, runtime = end_t - start_t))
}
