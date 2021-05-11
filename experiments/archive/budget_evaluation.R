# Total budget for LCBench
budget_upper = 52
budget_lower = 1

compute_total_budget = function(bupper, blower, eta) {
	smax = floor(log(bupper / blower, eta))
	B = (smax + 1) * bupper
	brackets = seq(0, smax)

	out = lapply(brackets, function(s) {
		n = ceiling(B / bupper * eta^s / (s + 1))
		r = bupper * eta^(-s)
		out = lapply(seq(0, s), function(i) {
			ni = floor(n * eta^(-i))
			ri = r * eta^i
			ni * ri
		})
		sum(unlist(out))
	})
	sum(unlist(out))
}


total_number_of_evals = compute_total_budget(budget_upper, budget_lower, 3)

# Compare to a random search full run: 
total_number_of_evals / budget_upper # 15 full evaluations of random search only? 


total_number_of_evals = compute_total_budget(budget_upper, budget_lower, 2)

# Compare to a random search full run: 
total_number_of_evals / budget_upper # 35 with a eta of 2


# Total number of evaluations for the other usecase
budget_upper = 98

total_number_of_evals = compute_total_budget(budget_upper, budget_lower, 3)

# Compare to a random search full run: 
total_number_of_evals / budget_upper # 15 full evaluations of random search only? 
