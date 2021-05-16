# HB implementation of hpbandster

hb = function(data, job, instance, eta) {

	ins = instance$ins$clone()
	objective = ins$objective
	search_space_old = ins$search_space

	param_ids = search_space_old$ids()

	budget_idx = which(ins$search_space$tags %in% c("budget", "fidelity"))
	budget_id = param_ids[budget_idx]
	budget_lower = search_space_old$params[[budget_idx]]$lower
	budget_upper = search_space_old$params[[budget_idx]]$upper

	# total budget the run is given
	fullbudget = ins$terminator$param_set$values$budget

	start_t = Sys.time()
    system(paste0("python3 experiments/algorithms/hb.py --problem ", instance$name, " --tempdir ", job$external.dir, " --task ", instance$task, " --minbudget ", budget_lower, " --maxbudget ", budget_upper, " --eta ",  eta, " --fullbudget ", fullbudget))
	end_t = Sys.time()

    return(list(runtime = end_t - start_t))
}
