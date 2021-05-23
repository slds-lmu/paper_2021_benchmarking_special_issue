# BOHB

# Budget of BOHB: 
# Number of brackets: max_SH_iter = -int(np.log(min_budget/max_budget)/np.log(eta)) + 1 (probably statistically more stable than our computation)
# e.g., for max_budget = 98 and eta = 3 it is 4
# Budgets: budgets = max_budget * np.power(eta, -np.linspace(self.max_SH_iter-1, 0, self.max_SH_iter))
# e.g., 98 * 3^(-3:0) = c(3.6, 10.8, 32.6, 98)
# Within a bracket: 
# s is the number of SH rungs 
# n0 = int(np.floor((max_SH_iter)/(s+1)) * self.eta**s) (Configs to start with)
# ns = [max(int(n0*(self.eta**(-i))), 1) for i in range(s+1)] (Configs for the brackets: ni)


# TODO: Check whether bohb/hb maximizes! --> DONE, it minimizes. Always needs to be transformed
# TODO: Get pandas data.frame does not give me the right thing! --> No, but I managed to do python logging
# TODO: It seems that only the most exploratory bracket is returned --> Fixed, one has to set the n_iters to at least the number of brackets

hpbster = function(data, job, instance, eta, algorithm_type) {

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
	fullbudget = ins$terminator$param_set$values$budget

	start_t = Sys.time()
    out = system2('python3', c("experiments/algorithms/hpbster.py",  "--alg", algorithm_type, 
    	" --problem ", instance$name, " --tempdir ", job$external.dir, 
    	" --task ", instance$task, " --minbudget ", budget_lower, 
    	" --maxbudget ", budget_upper, " --eta ",  eta, 
    	" --fullbudget ", fullbudget, " --objective ", cid, 
    	" --objective_multiplier ", objective_multiplier), 
    stderr = paste0(job$external.dir, "/err.txt"))
	end_t = Sys.time()

	if (out != 0)
		stop("Error in execution of hpbandster.")

    return(list(runtime = end_t - start_t))
}
