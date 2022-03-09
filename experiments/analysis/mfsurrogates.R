library(data.table)
library(mlr3misc)

lcbench = readRDS("lcbench_results.rds")
names(lcbench) = c("id", "repl", "cfg", "task", "algorithm", "eval_nr", "budget", "performance")
ymin = lcbench[, .(ymin = min(performance)), by = .(task)]
ymax = lcbench[algorithm == "randomsearch_full_budget", .(ymax = median(performance)), by = .(task)]
lcbench = merge(lcbench, ymax, by = "task")
lcbench = merge(lcbench, ymin, by = "task")

lcbench[, nr := (performance - ymin) / (ymax - ymin)]
lcbench[, best := cummin(nr), by = .(id)]
lcbench[, cumbudget := cumsum(budget), by = .(id)]

drop = lcbench[, .(not_full = max(cumbudget) < 1 * 52 * 30 * 7), by = .(id)]
drop = drop[not_full == TRUE]$id
lcbench = lcbench[id %nin% drop]

lcbench_agg_repl = lcbench[, .(mean_best = mean(best), sd_best = sd(best), nr = .N), by = .(algorithm, eval_nr, cumbudget, task)]
lcbench_agg_repl[, se_best := sd_best / sqrt(nr)]

benchmark = "lcbench"
p = ggplot(aes(y = mean_best, x = cumbudget, colour = algorithm, fill = algorithm), data = lcbench_agg_repl) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_best - se_best, ymax = mean_best + se_best), colour = NA, alpha = 0.1) +
  xlab("Budget in Multiples of Max Budget") +
  ylab("Mean Normalized Regret") +
  labs(title = paste0(benchmark, " Test Instances"), colour = "Algorithm", fill = "Algorithm") +
  facet_wrap(vars(task))
p = p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))
p = p + scale_colour_Publication() + scale_fill_Publication()

