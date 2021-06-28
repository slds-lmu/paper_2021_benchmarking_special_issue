library(data.table)
library(future)
library(future.apply)
library(ggplot2)
library(ggpubr)

#plan(multicore, workers = 16)
loadRegistry("registry_new")

baseline = findTagged("baseline")
results_baseline = map_dtr(baseline$job.id, function(id) {
  job = makeJob(id)
  archive = loadResult(id)$archive
  data = archive$data
  best = map_dbl(seq_len(NROW(data)), function(i) {
    min(data[1:i, archive$cols_y, with = FALSE])
  })
  res = data.table(batch_nr = data$batch_nr, best = best, cfg = job$prob.pars$cfg, level = job$prob.pars$level, repl = job$repl)
  res[, evaluation := seq_len(.N)]
  res
})

saveRDS(results_baseline, "/home/user/ofatime/results_baseline.rds")

tags = batchtools::getUsedJobTags()[-1L]
future_lapply(tags, FUN = function(tag) {
  tagged = findTagged(tag)
  tmp = map_dtr(tagged$job.id, function(id) {
    job = makeJob(id)
    archive = loadResult(id)$archive
    data = archive$data
    best = map_dbl(seq_len(NROW(data)), function(i) {
      min(data[1:i, archive$cols_y, with = FALSE])
    })
    tag_value = job$algo.pars[[tag]]
    if (tag == "surrogate_learner") tag_value = tag_value[[1L]]$id
    res = data.table(batch_nr = data$batch_nr, best = best, cfg = job$prob.pars$cfg, level = job$prob.pars$level, repl = job$repl, tag = tag_value)
    names(res)[6L] = tag
    res[, evaluation := seq_len(.N)]
  })
  saveRDS(tmp, paste0("/home/user/ofatime/results_", tag, ".rds"))
})

visualize_results = function(results, results_baseline, tag, lambda) {
  results_baseline[[tag]] = ifelse(tag == "surrogate_learner", yes = lambda[[tag]][[1L]][[1L]]$id, no = lambda[[tag]])
  results = map_dtr(unique(results_baseline$repl), function(r) {
    results_baseline_ = results_baseline[repl == r]
    results_ = results[repl == r]

    tmp = rbind(results_baseline_, results_, fill = TRUE)
    tmp_min_max = setNames(tmp[repl == r, min(best), by = .(cfg, level)], c("cfg", "level", "y_min"))
    tmp_min_max$y_max = tmp[repl == r, max(best), by = .(cfg, level)]$V1
    tmp_min_max[, y_diff := y_max - y_min]
    setkeyv(tmp_min_max, c("cfg", "level"))

    results_baseline_ = cbind(results_baseline_, tmp_min_max[results_baseline_[, c("cfg", "level")], c("y_min", "y_max", "y_diff")])
    results_baseline_[, normalized_regret := (best - y_min) / (y_diff)]
    results_baseline_mean = setNames(results_baseline_[, mean(normalized_regret), by = .(batch_nr, cfg, get(tag), evaluation)], c("batch_nr", "cfg", tag, "evaluation", "mean_normalized_regret"))
    results_ = cbind(results_, tmp_min_max[results_[, c("cfg", "level")], c("y_min", "y_max", "y_diff")])
    results_[, normalized_regret := (best - y_min) / (y_diff)]
    results_mean = setNames(results_[, mean(normalized_regret), by = .(batch_nr, cfg, get(tag), evaluation)], c("batch_nr", "cfg", tag, "evaluation", "mean_normalized_regret"))

    tmp = rbind(results_baseline_mean, results_mean)
    tmp$repl = r
    setkeyv(tmp, c("batch_nr", "cfg", tag, "evaluation"))
    tmp
  })
  results_mean = setNames(results[, mean(mean_normalized_regret), by = .(batch_nr, cfg, get(tag), evaluation)], c("batch_nr", "cfg", tag, "evaluation", "mean_mean_normalized_regret"))
  results_mean$sd_mean_normalized_regret = results[, sd(mean_normalized_regret), by = .(batch_nr, cfg, get(tag), evaluation)]$V1
  results_mean[, upper := mean_mean_normalized_regret + sd_mean_normalized_regret]
  results_mean[, lower := mean_mean_normalized_regret - sd_mean_normalized_regret]

  p1 = ggplot(aes_string(x = "evaluation", y = "log(mean_mean_normalized_regret)", colour = "as.factor(get(tag))"), data = results_mean[cfg == "lcbench"]) +
    geom_line() +
    #geom_ribbon(aes_string(x = "evaluation", ymin = "lower", ymax = "upper", colour = "as.factor(get(tag))"), alpha = 0.25) +
    labs(title = paste0("lcbench - default: ", results_baseline[[tag]][1L]), colour = tag) +
    theme_minimal()
  
  p2 = ggplot(aes_string(x = "evaluation", y = "log(mean_mean_normalized_regret)", colour = "as.factor(get(tag))"), data = results_mean[cfg == "rbv2_super"]) +
    geom_line() +
    #geom_ribbon(aes_string(x = "evaluation", ymin = "lower", ymax = "upper", colour = "as.factor(get(tag))"), alpha = 0.25) +
    labs(title = paste0("rbv2_super - default: ", results_baseline[[tag]][1L]), colour = tag) +
    theme_minimal()
  
  g = ggarrange(p1, p2, nrow = 1L, ncol = 2L, common.legend = TRUE)
  ggsave(file = paste0("/home/user/ofatime/ofa_", gsub("\\.", "_", tag), ".png"), width = 10, height = 8, plot = g)
}

library(ggplot2)
library(ggpubr)
tags = gsub("results_|.rds", "", setdiff(dir(), c("results_baseline.rds", "irace_instance.rda")))[-1L]
future_lapply(tags, FUN = function(tag) {
  results = readRDS(paste0("/home/user/ofatime/results_", tag, ".rds"))
  results_baseline = readRDS("/home/user/ofatime/results_baseline.rds")
  irace_result = readRDS("/home/user/ofatime/irace_instance.rda")
  lambda = irace_result$result_x_domain
  kknn = lambda$surrogate_learner
  ranger = irace_result$archive$data$x_domain[[1L]]$surrogate_learner
  lambda$surrogate_learner = list(list(kknn))
  visualize_results(results, results_baseline, tag, lambda)
})

