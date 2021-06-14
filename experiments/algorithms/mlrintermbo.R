mlrintermbo = function(data, job, instance, full_budget, surrogate, multi.point) {

	ins = instance$ins

	# # If no simple sequential BO is performed, we limit the number of evaluations
	# if (multi.point == 1L) {
	# 	ins$terminator$param_set$values$budget = ins$terminator$param_set$values$budget / 32L
	# }

	budget_idx = which(ins$search_space$tags %in% c("budget", "fidelity"))
	budget_id = ins$search_space$ids()[budget_idx]

	cids = ins$objective$codomain$ids()

	if (full_budget) {
		ts = as.integer(ins$search_space$params[[budget_id]]$upper)
		search_space = ins$search_space$clone()
		search_space$subset(setdiff(search_space$ids(), budget_id))
      	search_space$add(ParamInt$new(budget_id, lower = ts, upper = ts, default = ts, tags = "constant"))
		ins$search_space = search_space
	} 

	optimizer = OptimizerInterMBO$new()
	
	start_t = Sys.time()
	optimizer$optimize(ins)
	end_t = Sys.time()

	# To make it uniform in the end we rename the budget
	archive = ins$archive$data
	names(archive)[which(names(archive) == budget_id)] = "budget"
	names(archive)[which(names(archive) %in% cids)] = ifelse(length(cids) == 1, "performance", paste0("loss_", seq_len(length(length(cids)))))

    return(list(archive = archive, runtime = end_t - start_t))
}
