# test budget for bohb

library(purrr)
library(reticulate)
pd = import("pandas")

tab = summarizeExperiments(by = c("job.id", "problem", "task", "nobjectives", "objectives_scalar", "algorithm", "eta", "full_budget"))

tosubmit = tab[algorithm %in% c("hb", "bohb", "mlr3hyperband") & nobjectives == 1, ]
tosubmit = tosubmit[, .SD[which.min(job.id)], by = c("algorithm")]

submitJobs(tosubmit)
res = reduceResultsDataTable(findDone())
res = ijoin(tab, res)

# Update because we are doing everything in an external storage
toupdate = res[algorithm %in% c("hb", "bohb"), ]$job.id
updates = lapply(toupdate, function(id) {
	
	config = res[job.id == id, ]
	arch = pd$read_pickle(paste0("reg_temp/external/", id, "/results.pkl"))$get_all_runs()

	arch = lapply(arch, function(x) data.frame(budget = x$budget, loss = x$loss, bracket = x$config_id[[1]] + 1))
	arch = do.call(rbind, arch)

	# update names
	if (config$problem == "lcbench") {
		names(arch)[which(names(arch) == "loss")] = "val_accuracy"
		names(arch)[which(names(arch) == "budget")] = "epoch"
	}

	if (config$problem == "nb301") {
		names(arch)[which(names(arch) == "loss")] = "val_accuracy"
		names(arch)[which(names(arch) == "budget")] = "epoch"
	}	

	list(archive = as.data.table(arch), runtime = res[job.id == id, ][[1]][[1]])
})

res[job.id %in% toupdate, ]$result = updates

# get budget per bracket
budget = lapply(res$result, function(x) x$archive[, sum(epoch), by = c("bracket")])
names(budget) = res$problem


res$total_budget = budget



# Compute the total budget
out = lapply(updates, function(el) sum(el$archive$budget))
names(out) = res$problem

sum(myres$budget)

hyperband_brackets = function(R, eta) {
  result = data.frame()
  smax = floor(log(R, eta))
  B = (smax + 1) * R

  for (s in smax:0) {
    n = ceiling((B / R) * ((eta^s) / (s + 1)))
    r = R * eta^(-s)

    for (i in 0:s) {
      ni = floor(n * eta^(-i))
      ri = r * eta^i
      result = rbind(result, c(s, i, ri, ni, ri * ni))
    }
  }
  set_names(result, c("bracket", "bracket_stage", "budget_scaled", "n_configs", "total_budget"))
}

budget_lcbench = hyperband_brackets(52, 3)
sum(budget_lcbench$total_budget)

budget_nb301 = hyperband_brackets(98, 3)
setDT(budget_nb301)[, sum(total_budget), by = c("bracket")]



# TODO: FIND OUT THE DETAILS 

# The budget bohb takes for one hyperband run
# $nb301
#    bracket  V1
# 1:       1 490
# 2:       2 392
# 3:       3 294
# 4:       4 392
# 5:       5 490

# The budget our hyperband takes 
#    bracket       V1
# 1:       4 490.0000
# 2:       3 439.1852
# 3:       2 424.6667
# 4:       1 457.3333
# 5:       0 490.0000


# Total budget for hpbandster computes like this
eta = 3
budget_upper = 98
budget_lower = 1
max_SH_iter = - as.integer(log(budget_lower / budget_upper) / log(eta)) + 1
budgets = budget_upper * eta^(- seq(max_SH_iter - 1, 0, length.out = max_SH_iter))

# For five iterations:
iteration = 4
s = max_SH_iter - 1 - iteration %% max_SH_iter
# number of configurations in that bracket
n0 = as.integer(floor((max_SH_iter)/(s+1)) * eta^s)
ns = sapply(seq(0, s + 1), function(i) max(as.integer(n0 * eta^(-i)), 1))
num_configs=ns
budgets = budgets[(-s-1):1]