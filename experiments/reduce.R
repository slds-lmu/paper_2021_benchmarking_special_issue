source("experiments/config.R")
source("experiments/helper.R")


# Load real registry
reg = loadRegistry("reg_sequential", writeable = TRUE)

tab = summarizeExperiments(by = c("job.id", "problem", "task", "objectives", "algorithm", "full_budget"))


# Reduce results from cluster
filedir = reg$file.dir

storepath = "experiments/results/reduced_files"

MAX_BUDGET_SEQUENTIAL = list(
  lcbench = 7 * 52 * 30,
  rbv2_super = 38 * 1 * 30,
  nb301 = 34 * 98 * 30
)

# 1) LCBENCH 
# ALL COMPUTED ON RU59SOL2 (Julias account)

# Stored in 
# /dss/dssfs02/lwp-dss-0001/pr74ze/pr74ze-dss-0000/ru59sol2/repos/paper_2021_benchmarking_special_issue/experiments/results/reduced_files/lcbench

prob = "lcbench"

budget_max = MAX_BUDGET_SEQUENTIAL[[prob]]

for (algo in c("smac_full_budget", "randomsearch_full_budget", "mlr3hyperband", "hpbster_bohb")) {
	res = getResultsTable(tab, algo, prob, filedir, budget_max) 
	res = ijoin(tab, res)
	dir.create(file.path(storepath, prob, "sequential"), recursive = TRUE)
	saveRDS(res, file.path(storepath, prob, "sequential", paste0(algo, ".rds")))
}


# 2) RBV2 
# ALL COMPUTED ON RU59SOL2 (Julias account) EXCEPT FOR SMAC_FULL_BUDGET (WYOMING)

# Stored in 
# /dss/dssfs02/lwp-dss-0001/pr74ze/pr74ze-dss-0000/ru59sol2/repos/paper_2021_benchmarking_special_issue/experiments/results/reduced_files/rbv2_super
# (smac_full_budget) /gscratch/jmoosbau/repos/paper_2021_benchmarking_special_issue/experiments/results/reduced_files/rbv2_super/sequential

prob = "rbv2_super"

budget_max = MAX_BUDGET_SEQUENTIAL[[prob]]

for (algo in c("randomsearch_full_budget", "mlr3hyperband", "hpbster_bohb")) {
	res = getResultsTable(tab, algo, prob, filedir, budget_max) 
	res = ijoin(tab, res)
	dir.create(file.path(storepath, prob, "sequential"), recursive = TRUE)
	saveRDS(res, file.path(storepath, prob, "sequential", paste0(algo, ".rds")))
}

for (algo in c("smac_full_budget")) {
	res = getResultsTable(tab, algo, prob, filedir, budget_max) 
	res = ijoin(tab, res)
	dir.create(file.path(storepath, prob, "sequential"), recursive = TRUE)
	saveRDS(res, file.path(storepath, prob, "sequential", paste0(algo, ".rds")))
}


# 3) NB301 
# ALL COMPUTED ON DI25PIC (Martins account) EXCEPT FOR SMAC_FULL_BUDGET (WYOMING)

# /dss/dssfs02/lwp-dss-0001/pr74ze/pr74ze-dss-0000/di25pic2/benchmarkingsi_experiments_julia_nb301/paper_2021_benchmarking_special_issue/experiments/results/reduced_files/nb301

prob = "nb301"

budget_max = MAX_BUDGET_SEQUENTIAL[[prob]]

for (algo in c("smac_full_budget", "randomsearch_full_budget", "mlr3hyperband", "hpbster_bohb")) {
	res = getResultsTable(tab, algo, prob, filedir, budget_max) 
	res = ijoin(tab, res)
	dir.create(file.path(storepath, prob, "sequential"), recursive = TRUE)
	saveRDS(res, file.path(storepath, prob, "sequential", paste0(algo, ".rds")))
}










































# Budget boundaries we use (to reduce dataset size)
budget_max = MAX_BUDGET_SEQUENTIAL[[prob]]

# budget "stops" at which we are looking at the cumulative minimum 
# don't want the files to get too large
budgets = exp(seq(log(1), log(budget_max), length.out = 150)) # representable on log scale
budgets = c(budgets, seq(1, budget_max, length.out = 150)) # representable on normal scale
budgets = budgets[order(budgets)]

storepath = paste0("results/reduced_files/", prob) 
dir.create(storepath, recursive = TRUE)

for (algo in c("randomsearch_full_budget", "mlr3hyperband", "hpbster_bohb")) {
	res = getResultsTableSequential(tab, algo, prob, filedir, budgets) 
	res = ijoin(tab, res)
	saveRDS(res, file.path(storepath, paste0(algo, ".rds")))
}


# 2) RBV2_SUPER
# COMPUTED ON RU59SOL2 (Julias account)
# EXCEPT FOR SMAC_FULL_BUDGET

prob = "rbv2_super"
type = "sequential"

# Budget boundaries we use (to reduce dataset size)
budget_max = MAX_BUDGET_SEQUENTIAL[[prob]]

# budget "stops" at which we are looking at the cumulative minimum 
# don't want the files to get too large
budgets = exp(seq(log(3^(-3)), log(budget_max), length.out = 150)) # representable on log scale
budgets = c(budgets, seq(3^(-3), budget_max, length.out = 150)) # representable on normal scale
budgets = budgets[order(budgets)]

storepath = paste0("results/reduced_files/", prob) 
dir.create(storepath, recursive = TRUE)

for (algo in c("smac_full_budget", "randomsearch_full_budget", "mlr3hyperband", "hpbster_bohb")) {
	res = getResultsTableSequential(tab, algo, prob, filedir, budgets) 
	res = ijoin(tab, res)
	saveRDS(res, file.path(storepath, paste0(algo, ".rds")))
}



# 3) NB301 
# ALL COMPUTED ON DI25PIC2 (Martins account)

prob = "nb301"

# Budget boundaries we use (to reduce dataset size)
budget_max = MAX_BUDGET_SEQUENTIAL[[prob]]

# budget "stops" at which we are looking at the cumulative minimum 
# don't want the files to get too large
budgets = exp(seq(log(1), log(budget_max), length.out = 150)) # representable on log scale
budgets = c(budgets, seq(1, budget_max, length.out = 150)) # representable on normal scale
budgets = budgets[order(budgets)]

storepath = paste0("results/reduced_files/", prob) 
dir.create(storepath, recursive = TRUE)

for (algo in c("smac_full_budget", "randomsearch_full_budget", "mlr3hyperband", "hpbster_bohb")) {
	res = getResultsTableSequential(tab, algo, prob, filedir, budgets, objective_multiplier = -1) 
	res = ijoin(tab, res)
	saveRDS(res, file.path(storepath, paste0(algo, ".rds")))
}

# PARALLEL ANALYSIS 
for (algo in c("smac_full_budget", "randomsearch_full_budget", "mlr3hyperband", "hpbster_bohb")) {

	res = getResultsTableSequentialParallel(tab, algo, prob, filedir, budgets, objective_multiplier = -1) 
	res = ijoin(tab, res)
	saveRDS(res, file.path(storepath, paste0(algo, ".rds")))
}
