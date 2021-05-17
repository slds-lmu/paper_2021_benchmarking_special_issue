# TODO: Clean up the script 

# Test locally if everything works 
reg = loadRegistry("reg_temp", writeable = TRUE)

tab = summarizeExperiments(by = c("job.id", "problem", "task", "nobjectives", "objectives_scalar", "algorithm", "algorithm_type", "eta", "full_budget"))

# Testing every version of algorithm / problem with full budget
tosubmit = tab[, .SD[which.min(job.id)], by = c("problem", "task", "algorithm", "algorithm_type", "full_budget")]
tosubmit_lcbench = tosubmit[, .SD[which.min(task)], by = c("problem", "algorithm", "full_budget")]

submitJobs(tosubmit_lcbench)

tosubmit_nb301 = tosubmit[problem == "nb301", ]
submitJobs(tosubmit_nb301)


res = reduceResultsDataTable(findDone(), function(x) as.data.table(x$archive[, c("budget", "performance")]))
res = ijoin(tab, res)

toupdate = res[which(res$algorithm %in% c("bohb", "hb")), ]$job.id

updates = lapply(toupdate, function(jid) {
	# read the result 
	library(reticulate)
	pd = import("pandas")

	path = file.path(reg$file.dir, "external", jid, "results.pkl")

	df = pd$read_pickle(path)$get_pandas_dataframe()
	df = as.data.table(df)

	names(df)[which(names(df) == "loss")] = "performance"

	if (reg$defs[def.id == jid]$prob.pars[[1]]$objectives == "val_accuracy")
		df$performance = (-1) * df$performance

	return(df[, c("budget", "performance")])
})









tosubmit = ijoin(tosubmit, findNotDone())
tosubmit = tosubmit[algorithm == "bohb", ]

# tosubmit = rbind(tosubmit[problem == "nb301", ], tosubmit[task == "189873", ])

tosubmit$chunk = chunk(tosubmit$job.id, chunk.size = 5)






# Visualize the outcome 

resources.serial.default = list(
  walltime = 3600L * 24L * 2L, memory = 1024L * 2L,
  clusters = "serial", max.concurrent.jobs = 1000L # get name from lrz homepage)
)


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
