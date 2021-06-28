source("experiments/config.R")

# Load real registry
reg = loadRegistry("reg", writeable = TRUE)

tab = summarizeExperiments(by = c("job.id", "problem", "task", "nobjectives", "objectives_scalar", "algorithm", "algorithm_type", "eta", "full_budget"))

# Reduce all results in separate folders 

get_runtime_overview = function(tab) {
	res = reduceResultsDataTable(ijoin(tab, findDone()), function(x) as.numeric(x$runtime, units = "secs"))
	bla = ijoin(res, tab)[, c("result", "algorithm", "full_budget", "algorithm_type")]

	return(bla)
}

runtimes = get_runtime_overview(tab)
runtimes[, mean(result[[1]]) / 60, by = c("algorithm", "algorithm_type", "full_budget")]
saveRDS(runtimes, "experiments/results/runtimes.rds")


# Reduce results on a per task level 
path = "experiments/results"

problems = unique(tab$problem)
algos = unique(tab$algorithm)

status_summary = NULL

tasks = unique(tab$task)

for (prob in c("lcbench")) {
	for (tsk in unique(tab[problem == prob, ]$task)) {
		for (algo in algos) {
			if (!is.na(tsk)) {
				tored = tab[problem == prob & task %in% tsk & algorithm == algo, ]
			} else {
				tored = tab[problem == prob & algorithm == algo, ]				
			}
			toreduce = ijoin(tored, findDone())

			notdone = ijoin(tored, findNotDone())

			status_summary = rbind(status_summary, data.table(problem = prob, task = tsk, algorithm = algo, done = nrow(toreduce), open = nrow(notdone)))
			
			if (nrow(toreduce) > 0) {

				print(paste("Reducing: ", algo, "for task", tsk))
				
				res = reduceResultsDataTable(toreduce, function(x) {
					x$archive[, c("budget", "performance")]
				})

				if (algo %in% c("hpbster", "smac")) {

					library(reticulate)
					pd = import("pandas")

					updates = lapply(res$job.id, function(jid) {
						
						df = NULL

						# read the result 
						path = file.path(reg$file.dir, "external", jid, "results.pkl")

						if (file.exists(path)) {

							if (algo == "hpbster"){
								df = pd$read_pickle(path)$get_pandas_dataframe()
								df = as.data.table(df)
								names(df)[which(names(df) == "loss")] = "performance"
							} 
							if (algo == "smac") {
								df = as.data.table(pd$read_pickle(path))
							}


							if (tab[job.id == jid, ]$objectives == "val_accuracy")
								df$performance = (-1) * df$performance

							df = df[, c("budget", "performance")]
						} else {
							warning(paste0(jid, " not implemented yet."))
						}

						return(df)
					})	

					res$result = updates	
				}

				res = ijoin(tab, res)	

				if (!is.na(tsk)) {
					savepath = file.path(path, prob, tsk)
				} else {
					savepath = file.path(path, prob)
				}

				dir.create(savepath, recursive = TRUE)

				saveRDS(res, file.path(savepath, paste0(algo, ".rds")))
			}
		}
	}
}

saveRDS(status_summary, "experiments/results/status_summary.rds")



toreduce = tab[problem == prob & task == tsk, ]
toreduce = ijoin(toreduce, findDone())

res = reduceResultsDataTable(toreduce)
res = ijoin(tab, res)

toupdate = res[which(res$algorithm_type %in% c("bohb", "hb")), ]$job.id

for (tsk in tasks) {

	out = lapply(algos, function(algo) 	readRDS(file.path(path, prob, tsk, paste0(algo, ".rds"))))
	df = lapply(out, function(x) {
		if (any(names(x) == "V2")) {
			names(x)[which(names(x) == "V2")] == "result"
		}
		outn = lapply(1:nrow(x), function(i) {
			mm = cbind(x[i, - c("result")], x[i, c("result")]$result[[1]])
			mm$budget_cum = cumsum(mm$budget)
			mm
		})
		do.call(rbind, outn)
	})
	df = do.call(rbind, df)

	saveRDS(df, file.path(path, prob, tsk, paste0("result_sum.rds")))
}










