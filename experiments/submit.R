source("experiments/config.R")

# Standard resources used to submit jobs to cluster
resources.serial.default = list(
  walltime = 3600L * 24L * 2L, memory = 1024L * 2L,
  clusters = "serial", max.concurrent.jobs = 1000L 
)

# Load real registry
reg = loadRegistry("reg", writeable = TRUE)

tab = summarizeExperiments(by = c("job.id", "problem", "task", "nobjectives", "objectives_scalar", "algorithm", "algorithm_type", "eta", "full_budget", "multi.point", "log_scale"))

# Testing every version of algorithm / problem with full budget
# tasks = c("126026", "126029", "189908", "7593")

tosubmit = tab[problem == "lcbench", ]
tosubmit = ijoin(tosubmit, findNotDone())
tosubmit = tosubmit[- which(job.id %in% findOnSystem()$job.id), ]


# 1. RANDOMSERACH 
# Time: ~ 4 minutes
tosubmit_rs = tosubmit[algorithm == "randomsearch" & full_budget == TRUE, ]
tosubmit_rs$chunk = chunk(tosubmit_rs$job.id, chunk.size = 350)

submitJobs(tosubmit_rs, resources = resources.serial.default)



# 2. mlrintermbo 
# Time (full budget = TRUE): 	 min
# Time (full_budget = FALSE): 	3.85 h
tosubmit_mbo = tosubmit[algorithm == "mlrintermbo" & full_budget == TRUE, ]
tosubmit_mbo$chunk = chunk(tosubmit_mbo$job.id, chunk.size = 15)

submitJobs(tosubmit_mbo, resources = resources.serial.default)



# 3. BOHB
# Time: ~ 60 minutes
tosubmit_hpbster = tosubmit[algorithm_type == "bohb", ]
tosubmit_hpbster$chunk = chunk(tosubmit_hpbster$job.id, chunk.size = 310)
tosubmit_hpbster = tosubmit_hpbster[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(tosubmit_hpbster, resources = resources.serial.default)



# 4. HB
# Time: ~ ?? minutes
tosubmit_hpbster = tosubmit[algorithm_type == "hb", ]
tosubmit_hpbster$chunk = chunk(tosubmit_hpbster$job.id, chunk.size = 350)
tosubmit_hpbster = tosubmit_hpbster[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(tosubmit_hpbster, resources = resources.serial.default)


# 5. mlr3hyperband
# Time: ~ 4 minutes 
tosubmit_hb = tosubmit[algorithm == "mlr3hyperband", ]
tosubmit_hb$chunk = chunk(tosubmit_hb$job.id, chunk.size = 350)
tosubmit_hb = tosubmit_hb[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(tosubmit_hb, resources = resources.serial.default)



# 6. smac
# Time: ~ 4 minutes 
# Only send half of the tasks for now
tasks = unique(tosubmit$task)[1:15]
tosubmit_smac = tosubmit[task %in% tasks & algorithm == "smac" & multi.point == 1, ] # multi.point = 1 only uses 1/32 of the budget than when it is run with multi.point NA
# Only run 5 replications for now
tosubmit_smac[, repl := 1:.N, by = c("problem", "task", "full_budget")]
tosubmit_smac = tosubmit_smac[repl <= 10, ]
tosubmit_smac$chunk = chunk(tosubmit_smac$job.id, chunk.size = 10)
tosubmit_smac = tosubmit_smac[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(tosubmit_smac[chunk == 1, ], resources = resources.serial.default)





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
# hpbster:		 						22 sec
















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




