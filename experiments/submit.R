# Load packages

packages = c(
  "batchtools",  
  "data.table",
  "reticulate", 
  "mfsurrogates", 
  "paradox", 
  "checkmate", 
  "bbotk", 
  "mlr3hyperband", 
  "mlrintermbo", 
  "miesmuschel",
  "mlr3learners",
  "mlr3pipelines"
) 

lapply(packages, library, character.only = TRUE)



# Submit to cluster

resources.serial.default = list(
  walltime = 3600L * 24L * 2L, memory = 1024L * 2L,
  clusters = "serial", max.concurrent.jobs = 1000L # get name from lrz homepage)
)

# Load real registriy
reg = loadRegistry("reg", writeable = TRUE)

tab = summarizeExperiments(by = c("job.id", "problem", "task", "nobjectives", "objectives_scalar", "algorithm", "algorithm_type", "eta", "full_budget"))

# Testing every version of algorithm / problem with full budget

# First complete the following tasks

tasks = c("126026", "126029", "189908", "7593")

tosubmit = tab[problem == "lcbench" & task %in% tasks, ]
tosubmit = ijoin(tosubmit, findNotDone())
tosubmit = tosubmit[- which(job.id %in% findOnSystem()$job.id), ]

# For random search, take a chunk.size of 50 because they run through within seconds
tosubmit_rs = tosubmit[algorithm == "randomsearch", ]
tosubmit_rs$chunk = chunk(tosubmit_rs$job.id, chunk.size = 50)

submitJobs(tosubmit_rs, resources = resources.serial.default)

# If mbo is trained on full budget, runtime is 20 minutes
# If mbo is trained not on full budget (--> more configs, surrogate more expensive), runtime is 35 mins
tosubmit_mbo = tosubmit[algorithm == "mlrintermbo", ]
tosubmit_mbo$chunk = chunk(tosubmit_mbo$job.id, chunk.size = 1)

submitJobs(tosubmit, resources = resources.serial.default)


# No batches for hpbster, this does not work!!! 
tosubmit_hpbster = tosubmit[algorithm_type == "bohb", ]

submitJobs(findExpired(), resources = resources.serial.default)




# Check whether those first jobs are done
table(ijoin(tab, findDone())[task %in% tasks, ]$algorithm)
table(ijoin(tab, findSubmitted())[task %in% tasks, ]$algorithm)




# Resubmit hpbster for which we have no results 
job.ids = list.dirs("reg/external/", full.names = FALSE)
out = lapply(job.ids, function(job.id) {
	data.frame(job.id = as.numeric(job.id), records.exist = file.exists(file.path("reg/external/", job.id, "results.pkl")))
})
out = do.call(rbind, out)

# Find the hpbster ones that are done
out = ijoin(out, findDone())
out = ijoin(tab, out)
resubmit = out[which(!records.exist), ]

resubmit$chunk = chunk(resubmit$job.id, chunk.size = 120)

submitJobs(resubmit[chunk %in% 1:2, ], resources = resources.serial.default)























# Check the runtime of the different versions
# mlrintermbo (full budget = TRUE): 	20 min
# mlrintermbo (full_budget = FALSE): 	34 min
# mlr3hyperband:  						7 sec
# hpbster:		 						1.1 min

res = reduceResultsDataTable(ijoin(tab, findDone())[algorithm == "hpbster", ], function(x) x$runtime)
bla = ijoin(res, tab)[, c("result", "algorithm", "full_budget", "algorithm_type")]
bla[, mean(result[[1]]), by = full_budget]



# Reduce results on a per task level 

path = "experiments/results"

prob = "lcbench"
tsk = "126026"
algos = unique(tab$algorithm)

for (tsk in tasks) {
	for (algo in algos) {
		tored = tab[problem == prob & task %in% tsk & algorithm == algo, ]
		toreduce = ijoin(tored, findDone())

		notdone = ijoin(tored, findNotDone())

		# if (nrow(notdone) == 0) {

			print(paste("Reducing: ", algo, "for task", tsk))
			
			res = reduceResultsDataTable(toreduce, function(x) {
				x$archive[, c("budget", "performance")]
			})

			if (algo == "hpbster") {

				library(reticulate)
				pd = import("pandas")

				updates = lapply(res$job.id, function(jid) {
					
					df = NULL

					# read the result 
					path = file.path(reg$file.dir, "external", jid, "results.pkl")

					if (file.exists(path)) {
						df = pd$read_pickle(path)$get_pandas_dataframe()
						df = as.data.table(df)

						names(df)[which(names(df) == "loss")] = "performance"

						if (tab[job.id == jid, ]$objectives == "val_accuracy")
							df$performance = (-1) * df$performance

						df = df[, c("budget", "performance")]
					} else {
						warning(paste0(jid, " not implemented yet."))
					}

					return(df)
				})			
			}

			res = ijoin(tab, res)		
			dir.create(file.path(path, prob, tsk), recursive = TRUE)

			saveRDS(res, file.path(path, prob, tsk, paste0(algo, ".rds")))
		# } else {
		# 	print(paste0("For ", algo, " and ", tsk, " experiments are not completed yet."))
		# }
	}
}



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

























# Visualize the outcome 
library(batchtools)

res = reduceResultsDataTable()
res = ijoin(tab, res)

# Draw learning curves (per task per problem averaged over iterations)
dfs = lapply(seq_len(nrow(res)), function(i) {
	out = cbind(res[i, - c("result")], res[i, ]$result[[1]]$archive$data[, c("epoch", "val_accuracy")])
	out$y_cum = cummax(out$val_accuracy)

	if (out$algorithm == "smashy") {
		out$epoch = exp(out$epoch)
	}

	out$budget_cum = cumsum(out$epoch)

	out
})

df = do.call(rbind, dfs)

df$algorithm_variant = paste0(df$algorithm, ifelse(!df$full_budget | is.na(df$full_budget), "", "_fullbudget"))


library(ggplot2)

p = ggplot(data = df, aes(x = log(budget_cum), y = y_cum, colour = algorithm_variant, group = job.id)) + geom_line() 
p




# Test locally if everything works 
reg = loadRegistry("reg_temp", writeable = TRUE)

tab = summarizeExperiments(by = c("job.id", "problem", "task", "nobjectives", "objectives_scalar", "algorithm", "algorithm_type", "eta", "full_budget"))

# Testing every version of algorithm / problem with full budget
tosubmit = tab[, .SD[which.min(job.id)], by = c("problem", "task", "algorithm", "algorithm_type", "full_budget")]
tosubmit_lcbench = tosubmit[, .SD[which.min(task)], by = c("problem", "algorithm", "full_budget")]

submitJobs(tosubmit_lcbench)

tosubmit_nb301 = tosubmit[problem == "nb301", ]
submitJobs(tosubmit_nb301)


