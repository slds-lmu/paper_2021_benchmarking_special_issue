source("experiments/config.R")

# Load real registry
reg = loadRegistry("reg_temp", writeable = TRUE)

tab = summarizeExperiments(by = c("job.id", "problem", "task", "nobjectives", "objectives_scalar", "algorithm", "algorithm_type", "eta", "full_budget", "log_scale"))

prob = "rbv2_super"


BUDGET_UPPER = list(
	"lcbench" = 52, 
	"nb301" = 98
)

MAX_BUDGETS = list(
	"lcbench" = 52 * B_MULTIPLIER * 8, 
	"nb301" = 35 * B_MULTIPLIER * 98
)


## Randomsearch (not full budget)
out = testJob(tab[algorithm == "randomsearch" & problem == prob & full_budget == FALSE, ][1, ]) 
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
path = file.path(reg$file.dir, "external", jid, "results.pkl")
df = pd$read_pickle(path)$get_pandas_dataframe()
df = as.data.table(df)
names(df)[which(names(df) == "loss")] = "performance"
sum(df$budget)
MAX_BUDGETS[[prob]] * PARALLELIZATION

assert_true(all(out$achive$budget == BUDGET_UPPER[[prob]]))
assert_true(sum(out$archive$budget) >= MAX_BUDGETS[[prob]])




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
jid = tab[algorithm == "smac_full_budget" & problem == prob, ][1, ]$job.id
out = testJob(jid) 
library(reticulate)
pd = import("pandas")
path = file.path(reg$file.dir, "external", jid, "results.pkl")
df = as.data.table(pd$read_pickle(path))
sum(df$budget)
MAX_BUDGETS[[prob]]