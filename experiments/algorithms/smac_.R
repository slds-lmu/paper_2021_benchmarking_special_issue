# SMAC

# Run smac in a deterministic fashion 

smac = function(data, job, instance, full_budget, ...) {

	ins = instance$ins$clone()
	objective = ins$objective
	search_space_old = ins$search_space

	cid = objective$codomain$ids()
	objective_multiplier = ifelse(objective$codomain$tags[[cid]] == "maximize", -1, 1)

	param_ids = search_space_old$ids()

	budget_idx = which(ins$search_space$tags %in% c("budget", "fidelity"))
	budget_id = param_ids[budget_idx]
	budget_lower = search_space_old$params[[budget_idx]]$lower
	budget_upper = search_space_old$params[[budget_idx]]$upper

	# total budget the run is given
	# Compute the total budget * 32 (such that we can reconstruct parallelization afterwards)
	total_budget = ins$terminator$param_set$values$budget

	start_t = Sys.time()
    out = system2('/usr/bin/python3', c("experiments/algorithms/smac_.py", 
    	" --problem ", instance$name, " --tempdir ", job$external.dir, 
    	" --task ", instance$task, " --budget_param ",  budget_id, 
    	" --minbudget ", budget_lower, " --maxbudget ", budget_upper, 
    	" --full_budget ", full_budget, " --total_budget ", total_budget, 
    	" --objective ", cid, " --objective_multiplier ", objective_multiplier))
	end_t = Sys.time()

	if (out != 0)
		stop("Error in execution of smac.")

    return(list(runtime = end_t - start_t))
}
