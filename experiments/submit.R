library(ggplot2)

tab = summarizeExperiments(by = c("job.id", "problem", "task", "nobjectives", "objectives_scalar", "algorithm", "eta", "full_budget"))

tosubmit = tab[nobjectives == 1, ]
tosubmit = tosubmit[task == "3945", ]

submitJobs(tosubmit)

# Visualize the outcome 

res = reduceResultsDataTable()
res = ijoin(tab, res)

