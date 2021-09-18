library(data.table)
library(mlr3misc)

### lcbench
lcbench = readRDS("lcbench_results.rds")
names(lcbench) = c("id", "repl", "cfg", "task", "algorithm", "eval_nr", "budget", "performance")
lcbench_smashy = readRDS("smashy_lcbench_results.rds")
lcbench_smashy[, id := as.character(id)]
lcbench_smashy[, algorithm := paste0(rq, "_", rqn)]

lcbench = rbind(lcbench, lcbench_smashy[, c("id", "repl", "cfg", "task", "algorithm", "eval_nr", "budget", "performance")])
rm(lcbench_smashy)
gc()

lcbench[, ymin := min(performance), by = .(task)]
lcbench[, ymax := max(performance), by = .(task)]

lcbench[, nr := (performance - ymin) / (ymax - ymin)]
lcbench[, best := cummin(nr), by = .(id)]
lcbench[, cumbudget := cumsum(budget), by = .(id)]

drop = lcbench[, .(not_full = max(cumbudget) < 1 * 52 * 30 * 7), by = .(id)]
drop = drop[not_full == TRUE]$id
lcbench = lcbench[id %nin% drop]

lcbench_agg = lcbench[, .(mean_best = mean(best), sd_best = sd(best), n = .N), by = .(algorithm, eval_nr, cumbudget)]
saveRDS(lcbench_agg, "lcbench_agg_results.rds")



### rbv2_super
rbv2_super = readRDS("rbv2_super_results.rds")
names(rbv2_super) = c("id", "repl", "cfg", "task", "algorithm", "eval_nr", "budget", "performance")
rbv2_super_smashy = readRDS("smashy_rbv2_super_results.rds")
rbv2_super_smashy[, id := as.character(id)]
rbv2_super_smashy[, algorithm := paste0(rq, "_", rqn)]

rbv2_super = rbind(rbv2_super, rbv2_super_smashy[, c("id", "repl", "cfg", "task", "algorithm", "eval_nr", "budget", "performance")])
rm(rbv2_super_smashy)
gc()

rbv2_super[, ymin := min(performance), by = .(task)]
rbv2_super[, ymax := max(performance), by = .(task)]

rbv2_super[, nr := (performance - ymin) / (ymax - ymin)]
rbv2_super[, best := cummin(nr), by = .(id)]
rbv2_super[, cumbudget := cumsum(budget), by = .(id)]

drop = rbv2_super[, .(not_full = max(cumbudget) < 1 * 1 * 30 * 37), by = .(id)]
drop = drop[not_full == TRUE]$id
rbv2_super = rbv2_super[id %nin% drop]

rbv2_super_agg = rbv2_super[, .(mean_best = mean(best), sd_best = sd(best), n = .N), by = .(algorithm, eval_nr, cumbudget)]
saveRDS(rbv2_super_agg, "rbv2_super_agg_results.rds")



### nb301
nb301 = readRDS("nb301_results.rds")
names(nb301) = c("id", "repl", "cfg", "task", "algorithm", "eval_nr", "budget", "performance")
nb301_smashy = readRDS("smashy_nb301_results.rds")
nb301_smashy[, id := as.character(id)]
nb301_smashy[, algorithm := paste0(rq, "_", rqn)]

nb301 = rbind(nb301, nb301_smashy[, c("id", "repl", "cfg", "task", "algorithm", "eval_nr", "budget", "performance")])
rm(nb301_smashy)
gc()

nb301[, ymin := min(performance), by = .(task)]
nb301[, ymax := max(performance), by = .(task)]

nb301[, nr := (performance - ymin) / (ymax - ymin)]
nb301[, best := cummin(nr), by = .(id)]
nb301[, cumbudget := cumsum(budget), by = .(id)]

drop = nb301[, .(not_full = max(cumbudget) < 1 * 98 * 30 * 30), by = .(id)]  # FIXME: actually should be 34 but smac did not finish
drop = drop[not_full == TRUE]$id
nb301 = nb301[id %nin% drop]

nb301_agg = nb301[, .(mean_best = mean(best), sd_best = sd(best), n = .N), by = .(algorithm, cumbudget)]
saveRDS(nb301_agg, "nb301_agg_results.rds")

