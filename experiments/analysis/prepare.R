library(data.table)
library(mlr3misc)

instances = readRDS(system.file("instances.rds", package = "mfsurrogates"))
instances = instances[cfg %in% c("lcbench", "rbv2_super") & test]

lcbench = dir("reduced_files/lcbench/sequential/")
lcbench_results = map_dtr(lcbench, function(file) {
  dat = readRDS(paste0("reduced_files/lcbench/sequential/", file))
  dat = dat[task %in% instances$level]
  for (problem_ in unique(dat$problem)) {
    for (task_ in unique(dat$task)) {
      dat[problem == problem_ & task == task_, repl := seq_len(.N)]
    }
  }
  map_dtr(seq_len(dim(dat)[1L]), function(i) {
    result = dat$result[[i]]
    result[, eval_nr := seq_len(.N)]
    tmp = cbind(dat[i, c("job.id", "repl", "problem", "task", "algorithm")], result[, c("eval_nr", "budget", "performance")])
    tmp[, job.id := paste0("a", job.id)]
    tmp
  })
})
saveRDS(lcbench_results, "lcbench_results.rds")
rm(lcbench_results)
gc()

rbv2_super = dir("reduced_files/rbv2_super/sequential/")
rbv2_super_results = map_dtr(rbv2_super, function(file) {
  dat = readRDS(paste0("reduced_files/rbv2_super/sequential/", file))
  dat = dat[task %in% instances$level]
  for (problem_ in unique(dat$problem)) {
    for (task_ in unique(dat$task)) {
      dat[problem == problem_ & task == task_, repl := seq_len(.N)]
    }
  }
  map_dtr(seq_len(dim(dat)[1L]), function(i) {
    result = dat$result[[i]]
    result[, eval_nr := seq_len(.N)]
    tmp = cbind(dat[i, c("job.id", "repl", "problem", "task", "algorithm")], result[, c("eval_nr", "budget", "performance")])
    tmp[, job.id := paste0("a", job.id)]
    tmp
  })
})
saveRDS(rbv2_super_results, "rbv2_super_results.rds")
rm(rbv2_super_results)
gc()

nb301 = dir("reduced_files/nb301/sequential/")
nb301_results = map_dtr(nb301, function(file) {
  dat = readRDS(paste0("reduced_files/nb301/sequential/", file))
  dat[, task := "0"]
  for (problem_ in unique(dat$problem)) {
    for (task_ in unique(dat$task)) {
      dat[problem == problem_ & task == task_, repl := seq_len(.N)]
    }
  }
  map_dtr(seq_len(dim(dat)[1L]), function(i) {
    result = dat$result[[i]]
    result[, eval_nr := seq_len(.N)]
    if (all(result$performance > 0)) {
      result$performance = - result$performance
    }
    tmp = cbind(dat[i, c("job.id", "repl", "problem", "task", "algorithm")], result[, c("eval_nr", "budget", "performance")])
    tmp[, job.id := paste0("a", job.id)]
    tmp
  })
})
saveRDS(nb301_results, "nb301_results.rds")
rm(nb301_results)
gc()

