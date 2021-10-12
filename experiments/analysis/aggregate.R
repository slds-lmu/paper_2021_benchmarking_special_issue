# TODO:
# standard error based on random intercept models
# lmer, lmer4 plot standard error

library(data.table)
library(mlr3misc)

### lcbench
lcbench = readRDS("lcbench_results.rds")
names(lcbench) = c("id", "repl", "cfg", "task", "algorithm", "eval_nr", "budget", "performance")
lcbench_smashy = readRDS("smashy_lcbench_results.rds")
lcbench_smashy[, id := as.character(id)]
lcbench_smashy[, algorithm := paste0(rq, "_", rqn)]
lcbenchx = readRDS("/project/mallet/mbinder1/toplot.rds")
lcbenchx[, job.id := paste0("b", job.id)]
lcbenchx[!is.na(rq) & !is.na(rqn), algorithm := paste0(rq, "_", rqn)]
lcbenchx[parallel == TRUE, algorithm := paste0(algorithm, "_", "parallel")]
lcbenchx[, cfg := "lcbench"]
lcbenchx = lcbenchx[, c("job.id", "task", "algorithm", "perf_cum", "budget_cum", "parallel", "cfg")]
names(lcbenchx) = c("id", "task", "algorithm", "performance", "cumbudget", "parallel", "cfg")

lcbench = rbind(lcbench, lcbench_smashy[, c("id", "repl", "cfg", "task", "algorithm", "eval_nr", "budget", "performance")], lcbenchx, fill = TRUE)
rm(lcbench_smashy)
gc()

ymin = lcbench[is.na(parallel), .(ymin = min(performance)), by = .(task)]
ymax = lcbench[algorithm == "randomsearch_full_budget", .(ymax = median(performance)), by = .(task)]
lcbench = merge(lcbench, ymax, by = "task")
lcbench = merge(lcbench, ymin, by = "task")

lcbench[, nr := (performance - ymin) / (ymax - ymin)]
lcbench[, best := cummin(nr), by = .(id)]
lcbench[is.na(parallel), cumbudget := cumsum(budget), by = .(id)]

#drop = lcbench[is.na(parallel), .(not_full = max(cumbudget) < 1 * 52 * 30 * 7), by = .(id)]
#drop = drop[not_full == TRUE]$id
#lcbench = lcbench[id %nin% drop]

map(c(1 * 52, 10 * 52, 100 * 52, 30 * 7 * 52), function(dcb) {
  lcbench_ = lcbench[cumbudget <= dcb]
  lcbench_[, mcb := max(cumbudget), by = .(id)]
  lcbench_ = lcbench_[cumbudget == mcb]
  saveRDS(lcbench_, paste0("lcbench_", dcb, ".rds"))
})

lcbench_agg_repl = lcbench[, .(mean_best = mean(best), sd_best = sd(best), nr = .N), by = .(algorithm, cumbudget, task)]  # FIXME: used to include eval_nr
lcbench_agg_repl_task = lcbench_agg_repl[, .(mean_mean_best = mean(mean_best), sd_mean_best = sd(mean_best), nt = .N), by = .(algorithm, cumbudget)]  # FIXME: used to include eval_nr
lcbench_agg_repl_task[, se_mean_best := sd_mean_best / sqrt(nt)]
saveRDS(lcbench_agg_repl_task, "lcbench_agg_results_new.rds")



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
ymax = rbv2_super[algorithm == "randomsearch_full_budget", .(ymax = median(performance)), by = .(task)]
rbv2_super = merge(rbv2_super, ymax, by = "task")

rbv2_super[, nr := (performance - ymin) / (ymax - ymin)]
rbv2_super[, best := cummin(nr), by = .(id)]
rbv2_super[, cumbudget := cumsum(budget), by = .(id)]

#drop = rbv2_super[, .(not_full = max(cumbudget) < 1 * 1 * 30 * 37), by = .(id)]
#drop = drop[not_full == TRUE]$id
#rbv2_super = rbv2_super[id %nin% drop]

map(c(1 * 1, 10 * 1, 100 * 1, 30 * 38 * 1), function(dcb) {
  rbv2_super_ = rbv2_super[cumbudget <= dcb]
  rbv2_super_[, mcb := max(cumbudget), by = .(id)]
  rbv2_super_ = rbv2_super_[cumbudget == mcb]
  saveRDS(rbv2_super_, paste0("rbv2_super_", dcb, ".rds"))
})

rbv2_super_agg_repl = rbv2_super[, .(mean_best = mean(best), sd_best = sd(best), nr = .N), by = .(algorithm, eval_nr, cumbudget, task)]
rbv2_super_agg_repl_task = rbv2_super_agg_repl[, .(mean_mean_best = mean(mean_best), sd_mean_best = sd(mean_best), nt = .N), by = .(algorithm, eval_nr, cumbudget)]
rbv2_super_agg_repl_task[, se_mean_best := sd_mean_best / sqrt(nt)]
saveRDS(rbv2_super_agg_repl_task, "rbv2_super_agg_results.rds")



### nb301
nb301 = readRDS("nb301_results.rds")
names(nb301) = c("id", "repl", "cfg", "task", "algorithm", "eval_nr", "budget", "performance")
nb301_smashy = readRDS("smashy_nb301_results.rds")
nb301_smashy[, id := as.character(id)]
nb301_smashy[, algorithm := paste0(rq, "_", rqn)]

nb301 = rbind(nb301, nb301_smashy[, c("id", "repl", "cfg", "task", "algorithm", "eval_nr", "budget", "performance")])
rm(nb301_smashy)
gc()

nb301[, ymin := min(performance)]
nb301$ymax = nb301[algorithm == "randomsearch_full_budget", median(performance)]

nb301[, nr := (performance - ymin) / (ymax - ymin)]
nb301[, best := cummin(nr), by = .(id)]
nb301[, cumbudget := cumsum(budget), by = .(id)]

#drop = nb301[, .(not_full = max(cumbudget) < 1 * 98 * 30 * 30), by = .(id)]  # FIXME: actually should be 34 but smac did not finish
#drop = drop[not_full == TRUE]$id
#nb301 = nb301[id %nin% drop]

map(c(1 * 98, 10 * 98, 100 * 98, 30 * 34 * 98), function(dcb) {
  nb301_ = nb301[cumbudget <= dcb]
  nb301_[, mcb := max(cumbudget), by = .(id)]
  nb301_ = nb301_[cumbudget == mcb]
  saveRDS(nb301_, paste0("nb301_", dcb, ".rds"))
})

nb301_agg_repl = nb301[, .(mean_best = mean(best), sd_best = sd(best), nr = .N), by = .(algorithm, eval_nr, cumbudget)]
nb301_agg_repl[, se_best := sd_best / sqrt(nr)]
saveRDS(nb301_agg_repl, "nb301_agg_results.rds")

nb301[, mcb := max(cumbudget), by = .(id)]
nb301 = nb301[cumbudget == mcb]
saveRDS(nb301, "nb301_final_performance.rds")

