source("experiments/config.R")

### CLUSTER CONFIGURATION ### 

resources.serial.default = list(
  walltime = 3600L * 24L * 2L, memory = 1024L * 2L,
  clusters = "serial", max.concurrent.jobs = 1000L 
)

### REGISTRY AND JOBS TO BE SUBMITTED ### 

reg = loadRegistry("reg_sequential", writeable = TRUE)

tab = summarizeExperiments(by = c("job.id", "problem", "task", "algorithm"))

tosubmit = tab[problem == "rbv2_super", ]
tosubmit = ijoin(tosubmit, findNotDone())
# tosubmit = tosubmit[- which(job.id %in% findOnSystem()$job.id), ]


### TODO            ###

# [x] Check what impact deterministic = TRUE has? --> Multiple evaluations of a single config with different seeds
# [x] Check whether budget is correctly computed for hpbandster hb / bohb
# [x] Submit remaining experiments for smac on lcbench
# [ ] Submit remaining experiments for rbv2_super
# [ ] Submit everything for nasbench



### STATUS OF JOBS  ### 

##  LCBENCH         ## 
##  (35 TAKS)       ##

### - Registry: reg_sequential (LRZ)
### - Completed: 
###   - randomsearch_full_budget: 5.50 mins  (Budget factor 32) 
###   - hpbster_hb:               16.0 mins  (Evaluation bug -- ordering of archive -- was fixed!)
###   - hpbster_bohb:             4.74 mins  (Budget factor increased for evaluation; Evaluation bug -- ordering of archive -- was fixed!)
###   - mlr3hyperband:            4.8 minutes 
###   - focussearch_full_budget:  0.25 minutes
### - Submitted: 
###   - smac_full_budget:         5.9 minutes (Re-submit after version update of SMAC on cluster)
###     batched in chunks to run for a total of 32 hours (18. August, 13 Uhr)


##  RBV2_SUPER      ## 
##  (89 TAKS)       ##

### - Registry: reg_sequential (LRZ)
### - Completed: 
###   - randomsearch_full_budget: 98 mins      (Budget factor 32) 
###   - mlr3hyperband:            101 minutes  (Budget factor 32)
### - Submitted: 
###   - hpbster_hb (DEPRIO):      120 mins (Resubmit after update of evaluation script -- ordering of archive)
###   - hpbster_bohb:             120 mins (20?) (Resubmit after update of evaluation script -- ordering of archive)

### - Not submitted: 
###   - smac_full_budget:         30 minutes (Re-submit after version update of SMAC on cluster)
###   - focussearch_full_budget:  1.2 minutes  


##  BRANIN      ## 
##  (1 TASK)       ##

### - Registry: reg_branin (locally submitted)
### - Completed: 
###   -
### - Submitted: 
###   - randomsearch_full_budget
###   - mlr3hyperband
###   - mlrintermbo_full_budget
###   - hpbster_hb
###   - hpbster_bohb
###   - smac_full_budget
### - Not submitted:  
###   - random_search
###   - mlrintermbo
###   - smac



### Not submitted (not needed for final evaluation)

###   - mlrintermbo (smac is a good baseline)
###   - randomsearch (not on full budget)
###   - smac_hb: 1.4 minutes (RUNNING, ERROR FIXED)
###   - smac_bohb: 15 minutes  (RUNNING, ERROR FIXED)
###   - focussearch 


## 1: Randomsearch (with budget factor 32 to allow for parallel execution)
(tosubmit_rs = tosubmit[algorithm == "randomsearch_full_budget", ])
tosubmit_rs = tosubmit_rs[, .SD[1:30], by = c("task")]
tosubmit_rs$chunk = chunk(tosubmit_rs$job.id, chunk.size = 15)
submitJobs(tosubmit_rs, resources = resources.serial.default)


# 2: BOHB -- PROBLEM: TAKING TOO LONG
(tosubmit_hpbster = tosubmit[algorithm == "hpbster_bohb", ])
tosubmit_hpbster = tosubmit_hpbster[, .SD[1:30], by = c("task")]
tosubmit_hpbster$chunk = batchtools::chunk(tosubmit_hpbster$job.id, chunk.size = 15)
tosubmit_hpbster = tosubmit_hpbster[- which(job.id %in% findOnSystem()$job.id), ]

for (ch in unique(tosubmit_hpbster$chunk)) {
  Sys.sleep(10)
  submitJobs(tosubmit_hpbster[chunk == ch, ], resources = resources.serial.default)
  getStatus(tosubmit_hpbster[chunk == ch, ])
}


# 3: HB -- DEPRIORITIZE FOR RBV2_SUPER (--> we have the mlr3hyperband experiments)
(tosubmit_hpbster = tosubmit[algorithm == "hpbster_hb", ])
tosubmit_hpbster = tosubmit_hpbster[, .SD[1:30], by = c("task")]
tosubmit_hpbster$chunk = batchtools::chunk(tosubmit_hpbster$job.id, chunk.size = 15)
tosubmit_hpbster = tosubmit_hpbster[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(tosubmit_hpbster, resources = resources.serial.default)


# 4: mlr3hyperband (Budget factor 32)
(tosubmit_hb = tosubmit[algorithm == "mlr3hyperband", ])
tosubmit_hb = tosubmit_hb[, .SD[1:30], by = c("task")]
tosubmit_hb$chunk = chunk(tosubmit_hb$job.id, chunk.size = 10)
tosubmit_hb = tosubmit_hb[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(tosubmit_hb, resources = resources.serial.default)


# 5: smac (as we have 32 replications to ensure the parallel setup, we have to submit much more experiments)
# FIRST DO FOR TEST INSTANCES ONLY
instances = readRDS("../paper_2021_multi_fidelity_surrogates/inst/instances.rds")
test_instances = instances[test == TRUE & cfg == "rbv2_super", ]$level

(tosubmit_smac = tosubmit[algorithm == "smac_full_budget", ]) 
tosubmit_smac$chunk = batchtools::chunk(tosubmit_smac$job.id, chunk.size = 96)
tosubmit_smac = tosubmit_smac[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(tosubmit_smac, resources = resources.serial.default)
# This will run for almost 4 days (end of the week)






