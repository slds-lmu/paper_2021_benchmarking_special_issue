# SMAC

# Run smac in a deterministic fashion 

smac_hb = function(data, job, instance, eta, algorithm_type, ...) {

	source("experiments/helper.R")

	ins = instance$ins$clone()
	bid = ins$search_space$ids(tags = c("budget"))

	objective = ins$objective
	search_space_old = ins$search_space

	cid = objective$codomain$ids()
	objective_multiplier = ifelse(objective$codomain$tags[[cid]] == "maximize", -1, 1)

	param_ids = search_space_old$ids()

	budget_lower = search_space_old$params[[bid]]$lower
	budget_upper = search_space_old$params[[bid]]$upper

	total_budget = ins$terminator$param_set$values$budget # * PARALLELIZATION

	budget_per_hb_run = compute_total_budget(budget_upper, budget_lower, eta)
	evals_per_hb_run = compute_total_evals(budget_upper, budget_lower, eta)
	total_number_of_evals = ceiling(total_budget / budget_per_hb_run * evals_per_hb_run * 1.1)

	start_t = Sys.time()
    out = system2('python ', c("experiments/algorithms/smac_hyperband.py", 
    	" --alg ", algorithm_type, 
    	" --problem ", instance$name, " --tempdir ", job$external.dir, 
    	" --task ", instance$task, " --budget_param ",  bid, 
    	" --minbudget ", budget_lower, " --maxbudget ", budget_upper, 
    	" --eta ", eta, 
    	" --total_budget ", total_number_of_evals, 
    	" --objective ", cid, " --objective_multiplier ", objective_multiplier, 
    	" --seed ", job$seed))
	end_t = Sys.time()

	if (out != 0)
		stop("Error in execution of smac.")

    return(list(runtime = end_t - start_t))
}



