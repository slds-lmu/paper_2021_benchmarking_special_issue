# SMAC

# Run smac in a deterministic fashion 

smac = function(data, job, instance, full_budget, log_scale = TRUE, multi.point = 1L, ...) {

	ins = instance$ins$clone()
	bid = ins$search_space$ids(tags = c("budget"))

	objective = ins$objective
	search_space_old = ins$search_space

	cid = objective$codomain$ids()
	objective_multiplier = ifelse(objective$codomain$tags[[cid]] == "maximize", -1, 1)

	param_ids = search_space_old$ids()

	budget_lower = search_space_old$params[[bid]]$lower
	budget_upper = search_space_old$params[[bid]]$upper

	total_budget = ins$terminator$param_set$values$budget 

	# For the multipoint variant, we increase the budget by the parallelization factor
	total_budget = total_budget * multi.point

	# job = list(external.dir = ".", seed = 21)
	# full_budget = 100
	# log_scale = TRUE

	start_t = Sys.time()
    out = system2('python3', c("experiments/algorithms/smac_.py", 
    	" --problem ", instance$name, " --tempdir ", job$external.dir, 
    	" --task ", instance$task, " --budget_param ",  bid, 
    	" --minbudget ", budget_lower, " --maxbudget ", budget_upper, 
    	" --budget_on_log ", log_scale, 
    	" --full_budget ", full_budget, " --total_budget ", total_budget, 
    	" --objective ", cid, " --objective_multiplier ", objective_multiplier, 
    	" --seed ", job$seed))
	end_t = Sys.time()

	if (out != 0)
		stop("Error in execution of smac.")

    return(list(runtime = end_t - start_t))
}
