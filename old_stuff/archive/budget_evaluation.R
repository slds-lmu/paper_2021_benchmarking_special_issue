# Total budget for LCBench
budget_upper = 52
budget_lower = 1

library(mlr3hyperband)
library(purrr)

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


brackets = hyperband_brackets(budget_upper, 2)
total_budget = sum(brackets$total_budget)

# Compare to a random search full run: 
total_budget / budget_upper 

brackets = hyperband_brackets(budget_upper, 3)
total_budget = sum(brackets$total_budget)

# Compare to a random search full run: 
total_budget / budget_upper 

# Total number of evaluations for the other usecase
budget_upper = 98
brackets = hyperband_brackets(budget_upper, 3)
total_budget = sum(brackets$total_budget)
print(total_budget)

# Compare to a random search full run: 
total_budget / budget_upper # 23 full evaluations of random search only? 

total_number_of_evals = compute_total_budget(budget_upper, budget_lower, 2)
print(total_number_of_evals)

total_number_of_evals / budget_upper