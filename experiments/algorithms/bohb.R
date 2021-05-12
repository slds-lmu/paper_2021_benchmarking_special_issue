# - BOHB

# Taken from 

bohb = function(data, job, instance, eta) {

	ins = instance$ins$clone()
	objective = ins$objective
	search_space_old = ins$search_space

	param_ids = search_space_old$ids()

	budget_idx = which(ins$search_space$tags %in% c("budget", "fidelity"))
	budget_id = param_ids[budget_idx]
	budget_lower = search_space_old$params[[budget_idx]]$lower
	budget_upper = search_space_old$params[[budget_idx]]$upper

	start_t = Sys.time()
    system(paste0("python3 experiments/algorithms/bohb.py --problem ", instance$name, " --tempdir ", job$external.dir, " --task ", instance$task, " --minbudget ", budget_lower, " --maxbudget ", budget_upper, " --eta ",  eta))
	end_t = Sys.time()

    # get result
    # res = read.csv(paste0(job$external.dir, "/res.csv", sep = ","))

    return(list(runtime = end_t - start_t))
}
