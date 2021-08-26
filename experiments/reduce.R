source("experiments/config.R")

# Load real registry
reg = loadRegistry("reg_sequential", writeable = TRUE)

tab = summarizeExperiments(by = c("job.id", "problem", "task", "objectives", "algorithm", "eta", "full_budget"))

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

prob = "rbv2_super"
algos = c("randomsearch_full_budget", "mlr3hyperband", "hpbster_bohb", "smac_full_budget")#, "smac_hb", "smac_bohb")

# Check if all the runs are complete
sub_tab = tab[problem %in% prob & algorithm %in% algos, ]
sub_tab = ijoin(sub_tab, findDone())
table(sub_tab$task, sub_tab$algorithm)


for (algo in algos) {
	tored = sub_tab[problem == prob & algorithm == algo, ]

	# if (nrow(tored) < 30) {
	# 	stop(paste0("Stop: ", nrow(tored), " < ", 30))
	# } else {
		print(paste("Reducing: ", algo))
		
		res = reduceResultsDataTable(tored, function(x) {
			x$archive[, c("budget", "performance")]
		})

		if (algo %in% c("hpbster_hb", "hpbster_bohb", "smac", "smac_full_budget", "smac_hb", "smac_bohb")) {

			library(reticulate)
			pd = import("pandas")

			updates = lapply(res$job.id, function(jid) {

				print(jid)
				
				df = NULL
				path = file.path(reg$file.dir, "external", jid)

				if (algo %in% c("hpbster_hb", "hpbster_bohb")) {
					
					library(jsonlite)
					library(dplyr)	
					
					if (file.exists(file.path(path, "results.json"))) {
						start = Sys.time()

						df = readLines(file.path(path, "results.json")) %>% lapply(function(x) {
							tryCatch({
								input = fromJSON(x, flatten = TRUE)
								out = cbind(matrix(input[[1]], nrow = 1), input[[2]], input[[4]]$loss, input[[3]]$submitted)
								out
							}, error = function(cond) return(NULL))})
						df = do.call(rbind, df)
						df = as.data.table(df)
						colnames(out) = c("cid1", "cid2", "cid3", "budget", "performance", "submitted")
						rownames(df) = NULL

						# configs = readLines(file.path(path, "configs.json")) %>% lapply(fromJSON)
						# configs = lapply(configs, function(x) {
						# 	cbind(t(as.data.table(x[[1]])), do.call(cbind, x[[2]]))
						# })
						# configs = as.data.table(do.call(rbind, configs))
						# colnames(configs)[1:3] = c("cid1", "cid2", "cid3")
						# df = merge(df, configs, all.x = TRUE, by = c("cid1", "cid2", "cid3"))

						df = df[order(df$submitted, df$cid1, df$cid3), ]
						if (prob != "rbv2_super") {
							df$budget = round(df$budget) # Correction to match the actual budget 
						}
						end = Sys.time()
						
						print(end - start)
					} else {
						warning(paste0("Results file does not exist for ", jid))
					}
				} 

				if (algo %in% c("smac", "smac_full_budget", "smac_bohb", "smac_hb")) {
					if (file.exists(file.path(path, "results.pkl"))) {
						df = as.data.table(pd$read_pickle(file.path(path, "results.pkl")))
						df$budget = round(df$budget)
					} else {
						warning(paste0("Results file does not exist for ", jid))
					}
				}
					
				# if (tab[job.id == jid, ]$objectives == "val_accuracy")
				# 	df$performance = (-1) * df$performance
				
				return(df)
			})	

			res$result = updates	
		}
	# }
	
	res = ijoin(tab, res)	

	savepath = file.path(path, prob)

	dir.create(savepath, recursive = TRUE)

	saveRDS(res, file.path(savepath, paste0(algo, "_30.rds")))
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