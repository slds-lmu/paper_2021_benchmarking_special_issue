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

tosubmit = tab[problem == "lcbench", ]
tosubmit = ijoin(tosubmit, findNotDone())
tosubmit = tosubmit[- which(job.id %in% findOnSystem()$job.id), ]


# 1. RANDOMSERACH (Budget factor 32)
tosubmit_rs = tosubmit[algorithm == "randomsearch_full_budget", ]
tosubmit_rs$chunk = chunk(tosubmit_rs$job.id, chunk.size = 15)
submitJobs(tosubmit_rs, resources = resources.serial.default)


# 2. mlrintermbo (NOT SUBMITTED YET)
tosubmit_mbo = tosubmit[algorithm == "mlrintermbo_full_budget", ]
tosubmit_mbo$chunk = chunk(tosubmit_mbo$job.id, chunk.size = 50)

submitJobs(tosubmit_mbo, resources = resources.serial.default)


# 3. BOHB
tosubmit_hpbster = tosubmit[algorithm == "hpbster_bohb", ]
tosubmit_hpbster$chunk = batchtools::chunk(tosubmit_hpbster$job.id, chunk.size = 40)
tosubmit_hpbster = tosubmit_hpbster[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(tosubmit_hpbster, resources = resources.serial.default)


# 4. HB
tosubmit_hpbster = tosubmit[algorithm == "hpbster_hb", ]
tosubmit_hpbster$chunk = chunk(tosubmit_hpbster$job.id, chunk.size = 350)
tosubmit_hpbster = tosubmit_hpbster[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(tosubmit_hpbster, resources = resources.serial.default)


# 5. mlr3hyperband (Budget factor 32)
tosubmit_hb = tosubmit[algorithm == "mlr3hyperband", ]
tosubmit_hb$chunk = chunk(tosubmit_hb$job.id, chunk.size = 10)
tosubmit_hb = tosubmit_hb[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(tosubmit_hb, resources = resources.serial.default)


# 6. smac
tosubmit_smac = tosubmit[algorithm == "smac_full_budget", ] # multi.point = 1 only uses 1/32 of the budget than when it is run with multi.point NA
tosubmit_smac$chunk = chunk(tosubmit_smac$job.id, chunk.size = 6)
tosubmit_smac = tosubmit_smac[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(tosubmit_smac, resources = resources.serial.default)


# 7. smac hb
tosubmit_smac = tosubmit[algorithm == "smac_hb", ] # multi.point = 1 only uses 1/32 of the budget than when it is run with multi.point NA
tosubmit_smac$chunk = batchtools::chunk(tosubmit_smac$job.id, chunk.size = 200)
tosubmit_smac = tosubmit_smac[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(tosubmit_smac, resources = resources.serial.default)


# 7. smac bohb
tosubmit_smac = tosubmit[algorithm == "smac_bohb", ] # multi.point = 1 only uses 1/32 of the budget than when it is run with multi.point NA
tosubmit_smac$chunk = batchtools::chunk(tosubmit_smac$job.id, chunk.size = 50)
tosubmit_smac = tosubmit_smac[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(tosubmit_smac, resources = resources.serial.default)


# 9. Focussearch (Budget factor 32)
tosubmit_fs = tosubmit[algorithm == "focussearch_full_budget", ] # multi.point = 1 only uses 1/32 of the budget than when it is run with multi.point NA
tosubmit_fs$chunk = batchtools::chunk(tosubmit_fs$job.id, chunk.size = 534)
tosubmit_fs = tosubmit_fs[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(tosubmit_fs, resources = resources.serial.default)




# STATUS 19.07.2021

## SEQUENTIAL EXPERIMENTS

# NEXT ONES TO SUBMIT: 
# * SMAC HB / BOHB FOR LCBENCH AND RBV2SUPER


### RBV2_SUPER (89 tasks)

# TODO: RESUBMIT HPBSTER (budget is transformed into int, it does not make sense! )

### - Registry: reg_sequential (LRZ)
### - Test run (to get the time): 

### - Completed: 
###		- randomsearch_full_budget: 98 minutes 
### - Submitted: 
###		- hpbster_hb: 3.1 minutes 
###		- hpbster_bohb: 20 Minutes / to be verified 
###		- smac_full_budget: 120 minutes
###		- mlr3hyperband: 101 minutes 
###		- focussearch_full_budget: 1.2 minutes
### - Not submitted:  
###   - random_search
###   - mlrintermbo (BUG)
### 	- mlrintermbo_full_budget (BUG)
###   - smac


### LCBENCH (35 tasks)

### - Registry: reg_sequential (LRZ)
### - Test run (to get the time): 
### - Completed: 
### 	- randomsearch_full_budget: 5.5 minutes 
###		- hpbster_hb: 0.58 minutes 
###		- hpbster_bohb: 1.3 minutes
###		- smac_full_budget: 5.9 minutes 
###		- mlr3hyperband: 4.8 minutes 
###		- smac_hb: 1.4 minutes 
###		- smac_bohb: 15 minutes 
###		- focussearch_full_budget: 0.25 minutes
### - Submitted: 
### - Not submitted:  
###   - mlrintermbo_full_budget
### 	- mlrintermbo
###   - random_search
###   - smac


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
