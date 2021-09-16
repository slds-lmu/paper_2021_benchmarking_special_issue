library(data.table)
library(mlr3misc)

medians = readRDS("medians.rds")
multicoreperf = readRDS("multicoreperf.rds")
singularperf = readRDS("singularperf.rds")
smashy_results = readRDS("smashy_results.rds")
ymaxmins = readRDS("ymaxmins.rds")

instances = readRDS(system.file("instances.rds", package = "mfsurrogates"))
instances = instances[cfg %in% c("lcbench", "rbv2_super") & test]

names(medians) = c("problem", "task", "median")
singularperf[is.na(singularperf[["task"]]), task := "0"]
multicoreperf[is.na(multicoreperf[["task"]]), task := "0"]
ymaxmins[is.na(ymaxmins[["task"]]), task := "0"]
medians[is.na(medians[["task"]]), task := "0"]
ymaxmins = merge(ymaxmins, medians, by = "task")

singularperf = singularperf[task %in% c(instances$level, "0")]
multicoreperf = multicoreperf[task %in% c(instances$level, "0")]
ymaxmins = ymaxmins[task %in% c(instances$level, "0")]

singularperf = merge(singularperf, ymaxmins, by = "task")
singularperf[, meanp := (meanx * (y_max - y_min)) + y_min]
singularperf[, sdp := (sdx * (y_max - y_min)) + y_min]
singularperf[, meann:= (meanp - y_min) / (median - y_min)]
singularperf[, sdn:= (sdp - y_min) / (median - y_min)]
singularperf[, problem := problem.x]
singularperf = singularperf[, .(meanx = mean(meanx), sdx = mean(sdx), nx = .N), by = c("algorithm", "problem")]


multicoreperf = merge(multicoreperf, ymaxmins, by = "task")
multicoreperf[, meanp := (meanx * (y_max - y_min)) + y_min]
multicoreperf[, sdp := (sdx * (y_max - y_min)) + y_min]
multicoreperf[, meann:= (meanp - y_min) / (median - y_min)]
multicoreperf[, sdn:= (sdp - y_min) / (median - y_min)]
multicoreperf[, problem := problem.x]
multicoreperf = multicoreperf[, .(meanx = mean(meanx), sdx = mean(sdx), nx = .N), by = c("algorithm", "problem")]


smashy_single = imap(smashy_results[c("1", "4", "5a", "5b", "6")], function(x, nm) {
  tmp = x$result
  tmp[, rq := nm]
})

smashy_multi = smashy_results[["7"]]$result
smashy_multi[, rq := "7"]

smashy = rbind(rbindlist(smashy_single), smashy_multi)

