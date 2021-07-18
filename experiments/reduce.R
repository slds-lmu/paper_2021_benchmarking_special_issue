source("experiments/config.R")

# Load real registry
reg = loadRegistry("reg_sequential", writeable = TRUE)

tab = summarizeExperiments(by = c("job.id", "problem", "task", "nobjectives", "objectives_scalar", "algorithm", "algorithm_type", "eta", "full_budget"))


done = ijoin(tab, findDone())
done = done[, .SD[which.min(job.id)], by = c("algorithm", "problem")]
res = reduceResultsDataTable(done$job.id, function(x) x$runtime)
ijoin(res, done)

# Reduce all results in separate folders 
# get_runtime_overview = function(tab) {
# 	res = reduceResultsDataTable(ijoin(tab, findDone()), function(x) as.numeric(x$runtime, units = "secs"))
# 	bla = ijoin(res, tab)[, c("result", "algorithm", "full_budget", "algorithm_type", "multi.point")]

# 	return(bla)
# }

# runtimes = get_runtime_overview(tab[algorithm == "smac", ])
# runtimes[, mean(result[[1]]) / 60, by = c("algorithm", "algorithm_type", "full_budget", "multi.point")]
# saveRDS(runtimes, "experiments/results/runtimes.rds")

# Reduce results on a per task level

path = "experiments/results_sequential/prepared_files"

prob = "lcbench"
algos = c("randomsearch_full_budget", "mlr3hyperband", "hpbster_hb", "hpbster_bohb", "smac_full_budget")

# Check if all the runs are complete
sub_tab = tab[problem %in% prob & algorithm %in% algos, ]
sub_tab = ijoin(sub_tab, findDone())
table(sub_tab$task, sub_tab$algorithm)


for (algo in algos) {
	tored = sub_tab[problem == prob & algorithm == algo, ]

	if (nrow(tored) < 30) {
		stop(paste0("Stop: ", nrow(tored), " < ", 30))
	} else {
		print(paste("Reducing: ", algo))
		
		res = reduceResultsDataTable(tored, function(x) {
			x$archive[, c("budget", "performance")]
		})

		if (algo %in% c("hpbster_hb", "hpbster_bohb", "smac", "smac_full_budget")) {

			library(reticulate)
			pd = import("pandas")

			updates = lapply(res$job.id, function(jid) {
				
				df = NULL

				# read the result 
				path = file.path(reg$file.dir, "external", jid, "results.pkl")

				if (file.exists(path)) {

					if (algo %in% c("hpbster_hb", "hpbster_bohb")) {
						df = pd$read_pickle(path)$get_pandas_dataframe()
						df = as.data.table(df)
						names(df)[which(names(df) == "loss")] = "performance"
					} 
					if (algo %in% c("smac", "smac_full_budget")) {
						df = as.data.table(pd$read_pickle(path))
					}

					if (tab[job.id == jid, ]$objectives == "val_accuracy")
						df$performance = (-1) * df$performance

					df = df[, c("budget", "performance")]
				} else {
					warning(paste0("Results file does not exist for ", jid))
				}

				return(df)
			})	

			res$result = updates	
		}
	}
	
	res = ijoin(tab, res)	

	savepath = file.path(path, prob)

	dir.create(savepath, recursive = TRUE)

	saveRDS(res, file.path(savepath, paste0(algo, ".rds")))
}


# Also store the smashy results accordingly 
path_orig_files = file.path("experiments", "results_sequential", "original_files")
problem = "lcbench"

f = file.path(path_orig_files, problem, "results_baseline_lcbench_config_lcbench.rds") #paste0("results_lambda_", problem, ".rds"))

df = as.data.table(readRDS(f))

res = lapply(unique(df$repl), function(r) {
	out = lapply(unique(df$task), function(t) {
		out = df[repl == r & task == t, c("epoch", "val_cross_entropy")]
		out$epoch = exp(out$epoch)
		names(out) = c("budget", "performance")
		newdata = data.table(problem = problem, task = t, nobjectives = 1, objectives_scalar = "val_cross_entropy", algorithm = "smashy_config_lcbench", algorithm_type = NA, eta = NA, full_budget = NA, .count = 1)
		newdata$result = list(out)
		return(newdata)
	})
	do.call(rbind, out)
})
res = do.call(rbind, res)
res$job.id = -2000 - seq_len(nrow(res))

saveRDS(res, file.path(path, "smashy.rds"))