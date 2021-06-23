# SCRIPT TO CREATE FIGURES, TABLES, AND GRAPHICS

library(data.table)
library(ggplot2)

# Learning Curves for the different problems 
problem = "lcbench"
dirs = list.dirs("results/lcbench")
dirs = dirs[2:length(dirs)]

algorithms = c("hpbster", "mlr3hyperband", "mlrintermbo", "randomsearch")


# TODO: 
# 1) Learning Curve Plots
# - Mark quantiles for ancertainties
# - Parallel analyzation
# - Mark end of the initial design 
# - Mark Brackets 
# - Show task
# - Analyze why mlr3hyperband works so well
# - Distinguish between train and test instances 


# 2) Quantile Plots 


# 3) epsilon-Plots (budget epsilon to best performance)


# 4) Rank comparison plots 


# 5) Ranks per budget unit --> Learning curves; to aggregate


# LATER:
# - Also run hyperband baseline 
# - Put plotting functions into helper 



for (d in dirs) {

	print(d)

	path = paste0(d, "/", algorithms, ".rds")
	
	dfs = lapply(path, function(d) {
		df = readRDS(d)
		df_out = lapply(seq(1, nrow(df)), function(i) {
			dh = df[i, ]
			dh$result = NULL
			dh$multi.point = NULL
			cbind(dh, df[i, ]$result[[1]])
		})
		do.call(rbind, df_out)
	})

	dfs = do.call(rbind, dfs)
	dfs = as.data.table(dfs)

	dfs[, budget_cum := cumsum(budget), by = c("job.id")]
	dfs[, nexps := length(unique(job.id)), by = c("problem", "task", "algorithm", "algorithm_type", "full_budget")]

	any(dfs$nexps < 30)

	dfs[algorithm == "hpbster", ]$algorithm = "bohb"
	dfs[full_budget == TRUE, ]$algorithm = paste0(dfs[full_budget == TRUE, ]$algorithm, "_full_budget")
	dfs$budget_cum_log = log(dfs$budget_cum / 52, 10)


	# Compute quantiles 
	quantiles = seq(0, 4, by = 0.25)
	df_sub = lapply(quantiles, function(q) {
		cbind(dfs[budget_cum_log <= q, .SD[which.max(performance)], by = c("job.id")], q = q)
	})
	df_sub = do.call(rbind, df_sub)
	df_sub = df_sub[, .(perf_mean = mean(performance), perf_sd = sd(performance)), by = c("q", "algorithm", "task")]

	p = ggplot(data = df_sub, aes(x = q, y = perf_mean, colour = algorithm, fill = algorithm)) 
	p = p + geom_line()
	p = p + geom_ribbon(aes(ymin = perf_mean - perf_sd, ymax = perf_mean + perf_sd), alpha = 0.1, colour = NA)
	p = p + theme_bw()
	p = p + scale_x_continuous(breaks = seq(0, 4, by = 1),
	        labels= 10^seq(0, 4))
	p = p + xlab("Budget spent (in multiples of full budget)")
	p = p + ylab("Mean validation accuracy")

	ggsave(file.path("figures/learning_curves", paste0(strsplit(d, "/")[[1]][3], ".png")), p, width = 8, height = 4)

}
