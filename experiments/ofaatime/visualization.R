library(data.table)
library(future)
library(future.apply)
library(ggplot2)
library(ggpubr)
library(batchtools)
library(mlr3misc)

plan(multicore, workers = 2)
loadRegistry("registry_new")

### baseline
baseline = findTagged("baseline")
results_baseline = map_dtr(baseline$job.id, function(id) {
  job = makeJob(id)
  archive = loadResult(id)$archive
  res = archive$data[, - c("x_domain", "timestamp")]
  budget_par = if (job$prob.pars$cfg == "lcbench") "epoch" else "trainsize"
  res$cfg = job$prob.pars$cfg
  res$level = job$prob.pars$level
  res$repl = job$repl
  best = map_dbl(seq_len(NROW(res)), function(i) {
    min(res[1:i, archive$cols_y, with = FALSE])
  })
  res$best = best
  res[, cumbudget := cumsum(exp(get(budget_par)))]
  res[, evaluation := seq_len(.N)]
  res
}, .fill = TRUE)

saveRDS(results_baseline, "/home/user/ofatime/results_baseline.rds")

### ofaatime
# FIXME does not work for tags like surrogate_turned_off (because we set random_interleave_fraction and random_interleave_fraction.end)
tags = batchtools::getUsedJobTags()[-1L]
future_lapply(tags, FUN = function(tag) {
  tagged = findTagged(tag)
  tmp = map_dtr(tagged$job.id, function(id) {
    job = makeJob(id)
    archive = loadResult(id)$archive
    res = archive$data[, - c("x_domain", "timestamp")]
    budget_par = if (job$prob.pars$cfg == "lcbench") "epoch" else "trainsize"
    tag_value = job$algo.pars[[tag]]
    if (tag == "surrogate_learner") tag_value = tag_value[[1L]]$id
    res$cfg = job$prob.pars$cfg
    res$level = job$prob.pars$level
    res$repl = job$repl
    res$tag = tag_value
    names(res)[which(names(res) == "tag")] = tag
    best = map_dbl(seq_len(NROW(res)), function(i) {
      min(res[1:i, archive$cols_y, with = FALSE])
    })
    res$best = best
    res[, cumbudget := cumsum(exp(get(budget_par)))]
    res[, evaluation := seq_len(.N)]
    res
  }, .fill = TRUE)
  saveRDS(tmp, paste0("/home/user/ofatime/results_", tag, ".rds"))
})

### additional surrogate_learner
loadRegistry("registry_surrogates")
tag = batchtools::getUsedJobTags()
tagged = findTagged(tag)
tmp = map_dtr(tagged$job.id, function(id) {
  job = makeJob(id)
  archive = loadResult(id)$archive
  res = archive$data[, - c("x_domain", "timestamp")]
  budget_par = if (job$prob.pars$cfg == "lcbench") "epoch" else "trainsize"
  tag_value = job$algo.pars[[tag]]
  if (tag == "surrogate_learner") tag_value = tag_value[[1L]]$id
  res$cfg = job$prob.pars$cfg
  res$level = job$prob.pars$level
  res$repl = job$repl
  res$tag = tag_value
  names(res)[which(names(res) == "tag")] = tag
  best = map_dbl(seq_len(NROW(res)), function(i) {
    min(res[1:i, archive$cols_y, with = FALSE])
  })
  res$best = best
  res[, cumbudget := cumsum(exp(get(budget_par)))]
  res[, evaluation := seq_len(.N)]
  res
}, .fill = TRUE)
saveRDS(tmp, paste0("/home/user/ofatime/results_", tag, "_detailed.rds"))



library(ggplot2)
library(ggpubr)

# visualization based on mean mean normalized regret
# FIXME: does not work for tags like surrogate_turned_off (because we set random_interleave_fraction and random_interleave_fraction.end)
visualize_results = function(results, results_baseline, results_min_max, tag, lambda) {
  results_baseline[[tag]] = ifelse(tag == "surrogate_learner", yes = lambda[[tag]][[1L]][[1L]]$id, no = lambda[[tag]])
  results = map_dtr(unique(results_baseline$repl), function(r) {
    results_baseline_ = results_baseline[repl == r]
    results_ = results[repl == r]
    results_min_max_ = results_min_max[repl == r]
    setkeyv(results_min_max_, c("cfg", "level"))

    results_baseline_ = cbind(results_baseline_, results_min_max_[results_baseline_[, c("cfg", "level")], c("y_min", "y_max", "y_diff")])
    results_baseline_[, normalized_regret := (best - y_min) / (y_diff)]
    results_baseline_mean = setNames(results_baseline_[, mean(normalized_regret), by = .(batch_nr, cfg, get(tag), evaluation, cumbudget)], c("batch_nr", "cfg", tag, "evaluation", "cumbudget", "mean_normalized_regret"))
    results_ = cbind(results_, results_min_max_[results_[, c("cfg", "level")], c("y_min", "y_max", "y_diff")])
    results_[, normalized_regret := (best - y_min) / (y_diff)]
    results_mean = setNames(results_[, mean(normalized_regret), by = .(batch_nr, cfg, get(tag), evaluation, cumbudget)], c("batch_nr", "cfg", tag, "evaluation", "cumbudget", "mean_normalized_regret"))

    tmp = rbind(results_baseline_mean, results_mean)
    tmp$repl = r
    setkeyv(tmp, c("batch_nr", "cfg", tag, "evaluation", "cumbudget", "repl"))
    tmp
  })
  results_mean = setNames(results[, mean(mean_normalized_regret), by = .(batch_nr, cfg, get(tag), evaluation, cumbudget)], c("batch_nr", "cfg", tag, "evaluation", "cumbudget", "mean_mean_normalized_regret"))
  results_mean$sd_mean_normalized_regret = results[, sd(mean_normalized_regret), by = .(batch_nr, cfg, get(tag), evaluation, cumbudget)]$V1
  results_mean[, upper := mean_mean_normalized_regret + sd_mean_normalized_regret]
  results_mean[, lower := mean_mean_normalized_regret - sd_mean_normalized_regret]

  p1 = ggplot(aes_string(x = "cumbudget", y = "log(mean_mean_normalized_regret)", colour = "as.factor(get(tag))"), data = results_mean[cfg == "lcbench"]) +
    geom_line() +
    labs(title = paste0("lcbench - default: ", results_baseline[[tag]][1L]), colour = tag) +
    theme_minimal()
  
  p2 = ggplot(aes_string(x = "cumbudget", y = "log(mean_mean_normalized_regret)", colour = "as.factor(get(tag))"), data = results_mean[cfg == "rbv2_super"]) +
    geom_line() +
    labs(title = paste0("rbv2_super - default: ", results_baseline[[tag]][1L]), colour = tag) +
    theme_minimal()
  
  g = ggarrange(p1, p2, nrow = 1L, ncol = 2L, common.legend = TRUE)
  ggsave(file = paste0("ofa_", gsub("\\.", "_", tag), ".png"), width = 10, height = 8, plot = g)
}



# visualization based on mean mean normalized regret for surrogate detailed
visualize_results_surrogate_detailed = function(results, lambda) {
  tag = "surrogate_learner"
  default = lambda[[tag]]
  results_min_max = map_dtr(c("lcbench", "rbv2_super"), function(config) {
    target_variable = if(config == "lcbench") "val_cross_entropy" else "logloss"
    results_min_max = setNames(results[cfg == config, min(get(target_variable)), by = .(repl, level)], c("repl", "level", "y_min"))
    results_min_max$y_max = results[cfg == config, max(get(target_variable)), by = .(repl, level)]$V1
    results_min_max$cfg = config
    results_min_max
  })
  setkeyv(results_min_max, c("repl", "level", "cfg"))
  results_min_max[, y_diff := y_max - y_min]

  results = map_dtr(unique(results$repl), function(r) {
    results_ = results[repl == r]
    results_min_max_ = results_min_max[repl == r]
    setkeyv(results_min_max_, c("cfg", "level"))

    results_ = cbind(results_, results_min_max_[results_[, c("cfg", "level")], c("y_min", "y_max", "y_diff")])
    results_[, normalized_regret := (best - y_min) / (y_diff)]
    results_mean = setNames(results_[, mean(normalized_regret), by = .(batch_nr, cfg, get(tag), evaluation, cumbudget)], c("batch_nr", "cfg", tag, "evaluation", "cumbudget", "mean_normalized_regret"))

    tmp = results_mean
    tmp$repl = r
    setkeyv(tmp, c("batch_nr", "cfg", tag, "evaluation", "cumbudget", "repl"))
    tmp
  })
  results_mean = setNames(results[, mean(mean_normalized_regret), by = .(batch_nr, cfg, get(tag), evaluation, cumbudget)], c("batch_nr", "cfg", tag, "evaluation", "cumbudget", "mean_mean_normalized_regret"))
  results_mean$sd_mean_normalized_regret = results[, sd(mean_normalized_regret), by = .(batch_nr, cfg, get(tag), evaluation, cumbudget)]$V1
  results_mean[, upper := mean_mean_normalized_regret + sd_mean_normalized_regret]
  results_mean[, lower := mean_mean_normalized_regret - sd_mean_normalized_regret]

  p1 = ggplot(aes_string(x = "cumbudget", y = "log(mean_mean_normalized_regret)", colour = "as.factor(get(tag))", fill = "as.factor(get(tag))"), data = results_mean[cfg == "lcbench"]) +
    geom_line() +
    geom_ribbon(aes_string(x = "cumbudget", ymin = "log(lower)", ymax = "log(upper)"), colour = NA, linetype = 0, alpha = 0.25, show.legend = FALSE) +
    labs(title = paste0("lcbench - default: ", default), colour = tag) +
    theme_minimal() +
    theme(legend.text = element_text(size = rel(0.6)))
  
  p2 = ggplot(aes_string(x = "cumbudget", y = "log(mean_mean_normalized_regret)", colour = "as.factor(get(tag))", fill = "as.factor(get(tag))"), data = results_mean[cfg == "rbv2_super"]) +
    geom_line() +
    geom_ribbon(aes_string(x = "cumbudget", ymin = "log(lower)", ymax = "log(upper)"), colour = NA, linetype = 0, alpha = 0.25, show.legend = FALSE) +
    labs(title = paste0("rbv2_super - default: ", default), colour = tag) +
    theme_minimal() +
    theme(legend.text = element_text(size = rel(0.6)))
  
  g = ggarrange(p1, p2, nrow = 1L, ncol = 2L, common.legend = TRUE)
  ggsave(file = "ofa_surrogate_learner_detailed.png", width = 16, height = 8, plot = g)
}

# get y_min and y_max for normalized regret
results_baseline = readRDS("results_baseline.rds")
setkeyv(results_baseline, c("repl", "level", "cfg"))
results_min_max = map_dtr(c("lcbench", "rbv2_super"), function(config) {
  target_variable = if(config == "lcbench") "val_cross_entropy" else "logloss"
  results_min_max = setNames(results_baseline[cfg == config, min(get(target_variable)), by = .(repl, level)], c("repl", "level", "y_min"))
  results_min_max$y_max = results_baseline[cfg == config, max(get(target_variable)), by = .(repl, level)]$V1
  results_min_max$cfg = config
  results_min_max
})
setkeyv(results_min_max, c("repl", "level", "cfg"))
results_min_max[, y_diff := y_max - y_min]

all = setdiff(dir()[grepl(".rds", dir())], c("results_baseline.rds", "results_surrogate_learner_detailed.rds"))
for (tag in all) {
  cat(tag, "\n")
  dat = readRDS(tag)
  setkeyv(dat, c("repl", "level", "cfg"))
  tmp = map_dtr(c("lcbench", "rbv2_super"), function(config) {
    target_variable = if(config == "lcbench") "val_cross_entropy" else "logloss"
    results_min_max = setNames(dat[cfg == config, min(get(target_variable)), by = .(repl, level)], c("repl", "level", "y_min"))
    results_min_max$y_max = dat[cfg == config, max(get(target_variable)), by = .(repl, level)]$V1
    results_min_max$cfg = config
    results_min_max
  })
  setkeyv(tmp, c("repl", "level", "cfg"))
  replace = which(results_min_max$y_min > tmp$y_min)
  results_min_max$y_min[replace] = tmp$y_min[replace]
  replace = which(results_min_max$y_max < tmp$y_max)
  results_min_max$y_max[replace] = tmp$y_max[replace]
  results_min_max
}

tags = gsub("results_|.rds", "", setdiff(all, "results_surrogate_turned_off.rds"))
future_lapply(tags, FUN = function(tag) {
  results = readRDS(paste0("results_", tag, ".rds"))
  irace_result = readRDS("irace_instance.rda")
  lambda = irace_result$result_x_domain
  kknn = lambda$surrogate_learner
  ranger = irace_result$archive$data$x_domain[[1L]]$surrogate_learner
  lambda$surrogate_learner = list(list(kknn))
  visualize_results(results, results_baseline, results_min_max, tag, lambda)
})



### anova / decision tree
library(car)
library(xtable)
library(rpart)
library(rpart.plot)

irace_result = readRDS("irace_instance.rda")
lambda = irace_result$result_x_domain
lambda$surrogate_learner = lambda$surrogate_learner$id

dat = readRDS("results_baseline.rds")
results = map_dtr(unique(dat$repl), function(r) {
  tmp = setNames(dat[repl == r, min(best), by = .(cfg, level)], c("cfg", "level", "best"))
  data.table(best = mean(tmp$best), repl = r)
})
results = cbind(results, as.data.table(lambda))

all = setdiff(dir()[grepl(".rds", dir())], c("results_baseline.rds", "results_surrogate_learner_detailed.rds"))
tags = gsub("results_|.rds", "", all)
# FIXME: does not work for tags like surrogate_turned_off (because we set random_interleave_fraction and random_interleave_fraction.end)
for(tag in tags) {
  dat = readRDS(paste0("results_", tag, ".rds"))
  results_tmp = map_dtr(unique(dat$repl), function(r) {
    tmp = dat[repl == r, ]
    tmp = setNames(tmp[, min(best), by = .(cfg, level, get(tag))], c("cfg", "level", tag, "best"))
    tmp = setNames(tmp[, mean(best), by = .(get(tag))], c(tag, "best"))
    tmp$repl = r
    tmp
  })
  drop = which(names(lambda) == tag)
  results_tmp = cbind(results_tmp, as.data.table(lambda[-drop]))
  results = rbind(results, results_tmp)
}

results$surrogate_turned_off = FALSE
results[random_interleave_fraction == 0 & random_interleave_fraction.end == 0, ][["surrogate_turned_off"]] = TRUE
as_factor = c(names(lambda), "surrogate_turned_off")
results[, (as_factor) := lapply(.SD, as.factor), .SDcols = as_factor]

frm = paste0("best ~ ", paste0(c(names(lambda[-3]), "surrogate_turned_off"), collapse = " + "))
aov = Anova(lm(as.formula(frm), data = results))
xtable(aov)

rp = rpart(frm, data = results)
png("ofa_rpart.png", width = 1200, height = 600)
rpart.plot(rp)
dev.off()

