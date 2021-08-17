source("experiments/config.R")

# Load real registry
reg = loadRegistry("reg_temp", writeable = TRUE)

tab = summarizeExperiments(by = c("job.id", "problem", "task", "nobjectives", "objectives_scalar", "algorithm", "algorithm_type", "eta", "full_budget", "log_scale"))

prob = "lcbench"


BUDGET_UPPER = list(
	"lcbench" = 52, 
	"nb301" = 98,
	"branin" = 1, 
	"rbv2_super" = 1
)

MAX_BUDGETS = list(
	"lcbench" = 52 * B_MULTIPLIER * 8, 
	"nb301" = 35 * B_MULTIPLIER * 98,
	"branin" = 1 * B_MULTIPLIER * 3, 
	"rbv2_super" = 1 * B_MULTIPLIER * 41
)


## Randomsearch (not full budget)
out = testJob(tab[algorithm == "focussearch" & problem == prob & full_budget == FALSE, ][1, ]) 
# expect that budget is tuned over on a log-scale 
assert_true(length(unique(out$archive$budget)) > 1)
assert_true(max(out$archive$budget) <= log(BUDGET_UPPER[[prob]]))
sum(exp(out$archive$budget))
MAX_BUDGETS[[prob]] * 4


## Randomsearch (full budget)
out = testJob(tab[algorithm == "randomsearch_full_budget" & problem == prob, ][1, ]) 
assert_true(all(out$achive$budget == BUDGET_UPPER[[prob]]))
assert_true(sum(out$archive$budget) >= MAX_BUDGETS[[prob]] * PARALLELIZATION)


## mlr3hyperband
out = testJob(tab[algorithm == "mlr3hyperband" & problem == prob, ][1, ]) 
assert_true(sum(out$archive$budget) >= MAX_BUDGETS[[prob]] * PARALLELIZATION)


## mlrintermbo
out = testJob(tab[algorithm == "mlrintermbo" & problem == prob, ][1, ]) 
# expect that budget is tuned over on a log-scale 
assert_true(length(unique(out$archive$budget)) > 1)
assert_true(max(out$archive$budget) <= log(BUDGET_UPPER[[prob]]))
sum(exp(out$archive$budget))
MAX_BUDGETS[[prob]]


## mlrintermbo (full_budget)
out = testJob(tab[algorithm == "mlrintermbo_full_budget" & problem == prob, ][1, ]) 
# expect that budget is tuned over on a log-scale 
assert_true(all(out$achive$budget == BUDGET_UPPER[[prob]]))
assert_true(sum(out$archive$budget) >= MAX_BUDGETS[[prob]])


## hpbster_hb / bohb
jid = tab[algorithm == "hpbster_bohb" & problem == prob, ][1, ]$job.id
out = testJob(jid) 
# Read the outcome
library(reticulate)
pd = import("pandas")
path = file.path(reg$file.dir, "external", jid, "results.json")
df = pd$read_pickle(path)
df = as.data.table(df)
names(df)[which(names(df) == "loss")] = "performance"
sum(df$budget)
MAX_BUDGETS[[prob]] * PARALLELIZATION

assert_true(all(out$achive$budget == BUDGET_UPPER[[prob]]))
assert_true(sum(out$archive$budget) >= MAX_BUDGETS[[prob]])

# Compute the schedule for LCBench 

library(jsonlite)
library(dplyr)	

path = file.path(reg$file.dir, "external", jid)

df = readLines(file.path(path, "results.json")) %>% lapply(fromJSON)
df = lapply(df, function(x) cbind(cid1 = x[[1]][1], cid2 = x[[1]][2], cid3 = x[[1]][3], budget = x[[2]], loss = x[[4]]$loss, as.data.frame(t(unlist(x[[3]])))))
configs = readLines(file.path(path, "configs.json")) %>% lapply(fromJSON)
configs = lapply(configs, function(x) cbind(cid1 = x[[1]][1], cid2 = x[[1]][2], cid3 = x[[1]][3], as.data.frame(t(unlist(x[[2]])))))
df = do.call(rbind, df)
configs = do.call(rbind, configs)
df = merge(df, configs, all.x = TRUE, by = c("cid1", "cid2", "cid3"))
df = df[order(df$submitted, df$cid1, df$cid3), ]
names(df)[which(names(df) == "loss")] = "performance"
df$budget = round(df$budget) # Correction to match the actual budget 

schedule = setDT(df)[, .N, by = c("cid1", "budget")]
saveRDS(schedule, "experiments/problems/lcbench/schedule_hpbster.rds")
schedule$cores_used = 32
schedule$budget_on_cores = schedule$budget * schedule$cores_used
schedule$filled_capacity = schedule$N / schedule$cores_used
sum(schedule$budget_on_cores)






## SMAC
jid = tab[algorithm == "smac" & problem == prob, ][1, ]$job.id
out = testJob(jid) 
library(reticulate)
pd = import("pandas")
path = file.path(reg$file.dir, "external", jid, "results.pkl")
df = as.data.table(pd$read_pickle(path))
sum(df$budget)
MAX_BUDGETS[[prob]]

## SMAC (full budget)
jid = tab[algorithm == "smac_full_budget" & problem == prob, ][2, ]$job.id
out = testJob(jid) 
library(reticulate)
pd = import("pandas")
path = file.path(reg$file.dir, "external", jid, "results.pkl")
df = as.data.table(pd$read_pickle(path))
sum(df$budget)
MAX_BUDGETS[[prob]]

## SMAC (full budget)
jid = tab[algorithm == "smac_hb" & problem == prob, ][1, ]$job.id
out = testJob(jid) 
library(reticulate)
pd = import("pandas")
path = file.path(reg$file.dir, "external", jid, "results.pkl")
df = as.data.table(pd$read_pickle(path))
sum(df$budget)
MAX_BUDGETS[[prob]] * 4

## SMAC (full budget)
jid = tab[algorithm == "smac_bohb" & problem == prob, ][1, ]$job.id
out = testJob(jid) 
library(reticulate)
pd = import("pandas")
path = file.path(reg$file.dir, "external", jid, "results.pkl")
df = as.data.table(pd$read_pickle(path))
sum(df$budget)
MAX_BUDGETS[[prob]]


## SMAC (full budget)
jid = tab[algorithm == "focussearch_full_budget" & problem == prob, ][1, ]$job.id
out = testJob(jid) 


