# TODO: Clean up the script 

reg = loadRegistry("reg_temp", writeable = TRUE)

resources.serial.default = list(
  walltime = 3600L * 24L * 2L, memory = 1024L * 2L,
  clusters = "serial", max.concurrent.jobs = 1000L # get name from lrz homepage)
)

tab = summarizeExperiments(by = c("job.id", "problem", "task", "nobjectives", "objectives_scalar", "algorithm", "eta", "full_budget"))

tosubmit = tab[nobjectives == 1, ]

# Testing every version of algorithm / problem / task with full budget
tosubmit = tosubmit[, .SD[which.min(job.id)], by = c("problem", "task", "algorithm", "full_budget")]
tosubmit = ijoin(tosubmit, findNotDone())
tosubmit = tosubmit[algorithm != "bohb", ]

# tosubmit = rbind(tosubmit[problem == "nb301", ], tosubmit[task == "189873", ])

tosubmit$chunk = chunk(tosubmit$job.id, chunk.size = 5)

submitJobs(tosubmit, resources = resources.serial.default)

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
