source("experiments/config.R")

# Standard resources used to submit jobs to cluster
resources.serial.default = list(
  walltime = 3600L * 24L * 2L, memory = 1024L * 2L,
  clusters = "serial", max.concurrent.jobs = 1000L 
)

# Load real registry
reg = loadRegistry("reg_sequential", writeable = TRUE)

tab = summarizeExperiments(by = c("job.id", "problem", "task", "algorithm", "algorithm_type", "eta", "full_budget", "log_scale"))

# Testing every version of algorithm / problem with full budget
# tasks = c("126026", "126029", "189908", "7593")

tosubmit = tab[problem == "rbv2_super", ]
tosubmit = ijoin(tosubmit, findNotDone())
tosubmit = tosubmit[- which(job.id %in% findOnSystem()$job.id), ]


# 1. RANDOMSERACH (Budget factor 32)
# Time: ~ 4 minutes (lcbench)
#       ~ 90 - 120 minutes (rbv2_super; bc. of log-scale? )
tosubmit_rs = tosubmit[algorithm == "randomsearch_full_budget", ]
tosubmit_rs$chunk = chunk(tosubmit_rs$job.id, chunk.size = 15)

submitJobs(tosubmit_rs, resources = resources.serial.default)


# 2. mlrintermbo (NOT SUBMITTED YET)
# Time: ~ X minutes (lcbench)
#       ~ X minutes (rbv2_super)
tosubmit_mbo = tosubmit[algorithm == "mlrintermbo_full_budget", ]
tosubmit_mbo$chunk = chunk(tosubmit_mbo$job.id, chunk.size = 50)

submitJobs(tosubmit_mbo, resources = resources.serial.default)



# 3. BOHB
# Time: ~ 1.3 minutes (lcbench)
#       ~ 20 minutes (rbv2_super)
tosubmit_hpbster = tosubmit[algorithm == "hpbster_bohb", ]
tosubmit_hpbster$chunk = chunk(tosubmit_hpbster$job.id, chunk.size = 50)
tosubmit_hpbster = tosubmit_hpbster[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(tosubmit_hpbster, resources = resources.serial.default)



# 4. HB
# Time: ~ 0.5 minutes (lcbench)
#       ~ 1.5 minutes (rbv2_super)
tosubmit_hpbster = tosubmit[algorithm == "hpbster_hb", ]
tosubmit_hpbster$chunk = chunk(tosubmit_hpbster$job.id, chunk.size = 350)
tosubmit_hpbster = tosubmit_hpbster[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(tosubmit_hpbster, resources = resources.serial.default)


# 5. mlr3hyperband (Budget factor 32)
# Time: ~ 5 minutes (lcbench)
#       ~ 90 minutes (rbv2_super)
tosubmit_hb = tosubmit[algorithm == "mlr3hyperband", ]
tosubmit_hb$chunk = chunk(tosubmit_hb$job.id, chunk.size = 20)
tosubmit_hb = tosubmit_hb[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(tosubmit_hb, resources = resources.serial.default)


# 6. smac
# Time: ~ 5 minutes (lcbench)
#       ~ 120 minutes (rbv2_super)
tosubmit_smac = tosubmit[algorithm == "smac_full_budget", ] # multi.point = 1 only uses 1/32 of the budget than when it is run with multi.point NA
tosubmit_smac$chunk = chunk(tosubmit_smac$job.id, chunk.size = 6)
tosubmit_smac = tosubmit_smac[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(tosubmit_smac, resources = resources.serial.default)


# 7. Focussearch (NOT SUBMITTED YET )
# Time: ~ X minutes 
tosubmit_fs = tosubmit[algorithm == "focussearch_full_budget", ] # multi.point = 1 only uses 1/32 of the budget than when it is run with multi.point NA
tosubmit_fs$chunk = chunk(tosubmit_fs$job.id, chunk.size = 5)
tosubmit_fs = tosubmit_fs[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(tosubmit_fs, resources = resources.serial.default)


# Check whether those first jobs are done
table(ijoin(tab, findDone())[task %in% tasks, ]$algorithm)
table(ijoin(tab, findSubmitted())[task %in% tasks, ]$algorithm)





# STATUS 13.07.2021

## SEQUENTIAL EXPERIMENTS

### BRANIN 

### - Registry: reg_branin (locally submitted)
### - Completed: 
###		-
### - Submitted: 
### 	- randomsearch_full_budget
###		- mlr3hyperband
### 	- mlrintermbo_full_budget
###		- hpbster_hb
###		- hpbster_bohb
###		- smac_full_budget
### - Not submitted:  
###   - random_search
###   - mlrintermbo
###   - smac

### RBV2_SUPER (89 tasks)

# TODO: RESUBMIT HPBSTER (budget is transformed into int, it does not make sense! )

### - Registry: reg_sequential (LRZ)
### - Test run (to get the time): 
###		- randomsearch_full_budget: 12391; 90 minutes 
###		- hpbster_hb: 3 Minutes 
###		- hpbster_bohb: 20 Minutes 
###		- smac_full_budget: 120 minutes
### - Completed: 
###		- hpbster_hb
###		- hpbster_bohb
### - Submitted: 
### 	- randomsearch_full_budget
###		- mlr3hyperband 
### - Not submitted:  
###		- smac_full_budget (COULDN'T SUBMIT ALL YET)
###   - random_search
###   - mlrintermbo (BUG)
###   - smac
### 	- mlrintermbo_full_budget (BUG)


### LCBENCH (35 tasks)

### - Registry: reg_sequential (LRZ)
### - Test run (to get the time): 
### - Completed: 
### 	- randomsearch_full_budget
###		- hpbster_hb
###		- hpbster_bohb
###		- smac_full_budget
###		- mlr3hyperband
### - Submitted: 
### - Not submitted:  
###   - mlrintermbo_full_budget
###   - random_search
###   - smac
### 	- mlrintermbo





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




