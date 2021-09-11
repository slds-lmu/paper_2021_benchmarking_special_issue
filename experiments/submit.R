data.table::setDTthreads(1)

source("experiments/config.R")

### CLUSTER CONFIGURATION ### 

resources.serial.default = list(
  walltime = 3600L * 24L * 4L, memory = 1024L * 2L,
  clusters = "serial", max.concurrent.jobs = 1000L 
)

# 4000 MB on teton

### REGISTRY AND JOBS TO BE SUBMITTED ### 

reg = loadRegistry("reg_sequential", writeable = TRUE)

tab = summarizeExperiments(by = c("job.id", "problem", "task", "algorithm", "objectives"))

# tosubmit = tab[problem == "lcbench", ]
tosubmit = tab[problem == "rbv2_super", ]
tosubmit = ijoin(tosubmit, findNotDone())



### STATUS OF JOBS  ### 

##  LCBENCH         ## 
##  (35 TAKS)       ##
##  ru59sol2

##  RBV2_SUPER      ## 
##  (89 TAKS)       ##
##  ru59sol2 
##  except for smac_full_budget --> Wyoming


##  NB301
##  di25pic2



table(ijoin(tosubmit, findDone())$algorithm)
table(ijoin(tosubmit, findRunning())$algorithm)

## 1: Randomsearch (with budget factor 32 to allow for parallel execution)
(tosubmit_rs = tosubmit[algorithm == "randomsearch_full_budget", ])
tosubmit_rs = tosubmit_rs[, .SD[1:30], by = c("task")]
tosubmit_rs$chunk = chunk(tosubmit_rs$job.id, chunk.size = 15)
submitJobs(tosubmit_rs, resources = resources.serial.default)


# 2: BOHB -- PROBLEM: TAKING TOO LONG
(tosubmit_hpbster = tosubmit[algorithm == "hpbster_bohb", ])
tosubmit_hpbster = tosubmit_hpbster[, .SD[1:30], by = c("task")]
tosubmit_hpbster$chunk = batchtools::chunk(tosubmit_hpbster$job.id, chunk.size = 20)
tosubmit_hpbster = tosubmit_hpbster[- which(job.id %in% findOnSystem()$job.id), ]

submitJobs(tosubmit_hpbster, resources = resources.serial.default)


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


### - Registry: reg_sequential (LRZ)
### - Completed: 
###   - randomsearch_full_budget: 5.50 mins  (Budget factor 32) 
###   - hpbster_hb:               16.0 mins  (Evaluation bug -- ordering of archive -- was fixed!)
###   - hpbster_bohb:             4.74 mins  (Budget factor increased for evaluation; Evaluation bug -- ordering of archive -- was fixed!)
###   - mlr3hyperband:            4.8 minutes 
###   - focussearch_full_budget:  0.25 minutes
### - Submitted: 
###   - smac_full_budget:         5.9 minutes (all submitted; expected AUGUST 26nd, 6PM)

