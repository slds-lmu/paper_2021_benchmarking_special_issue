source("fileloader.R")

TESTRUN <- TRUE


library("ggplot2")


data <- readRDS("results_preprocessed_lcbench_REAL_50.R")






dfb <- dfall[, .(mean(perf_cum)), by = c("algorithm", "budget_cum", "parallel", "rq", "rqn", "parallel")]
dfa <- subsampleGrid(dfall, makeGrid(dfall, 50))[, .(mean(perf_cum)), by = c("algorithm", "budget_cum", "parallel", "rq", "rqn", "parallel")]

ggplot(dfb, aes(x = log10(budget_cum / 52), y = V1, color = rq, group = algorithm, linetype = as.factor(rqn))) + geom_line()


ggplot() + geom_line(data = dfa, aes(x = log10(budget_cum / 52), y = V1, color = rq, group = algorithm, linetype = as.factor(rqn))) +
  geom_point(data = data.table(xx = log10(sort(unique(makeGrid(dfall, 50)[algorithm == "rq1.1", budget_cum])) / 52), yy = 1), aes(x = xx, y = yy)) +
  geom_point(data = data.table(xx = seq(log10(2 / 52), log10(max(dfb$budget_cum / 52)), length.out = 51), yy = 0.75), aes(x = xx, y = yy))

ggplot() + geom_line(data = dfa, aes(x = (budget_cum / 52), y = V1, color = rq, group = algorithm, linetype = as.factor(rqn))) +
  geom_point(data = data.table(xx = (sort(unique(makeGrid(dfall, 50)[algorithm == "rq1.1", budget_cum])) / 52), yy = 1), aes(x = xx, y = yy)) +
  geom_point(data = data.table(xx = seq((2 / 52), (max(dfb$budget_cum / 52)), length.out = 51), yy = 0.75), aes(x = xx, y = yy))


ggplot(dfa[parallel == TRUE], aes(x = log10(budget_cum / 52), y = V1, color = algorithm)) + geom_line()



ii <- subsampleGrid(dfall, 520)[, .(mean(perf_cum)), by = c("algorithm", "budget_cum", "parallel", "rq", "rqn")]

ii <- subsampleGrid(dfall, Inf)[, .(outcome = mean(perf_cum)), by = c("algorithm", "budget_cum", "parallel", "rq", "rqn")]

iia <- meta.input[ii, on = c("rq", "rqn")]



# TODOLIST

# - only use the 30 from smac that made it through
# - need:
#     - graph trace (mean, median, uncertainty indicators) by scenario
#     - high def data for points of interest (1, 10, 100, end)
#     - some raw data for rank plots...

# - mlrintermbo results for lcbench
# -

instances <- readRDS("~/development/R/miesmuschel/attic/data/instances.rds")

library("ggplot2")

source("../publication_themes.R")
source("../helper.R")

library("data.table")

library("ggpubr")

theme_set(theme_Publication())

scale_colour_discrete = scale_colour_Publication()
scale_fill_discrete = scale_fill_Publication()



MAXFID <- 52
PROBLEM <- "lcbench"
PLOTTITLE <- "LCbench"

MAXFID <- 98
PROBLEM <- "nb301"
PLOTTITLE <- "NASBENCH-301"

MAXFID <- 1
PROBLEM <- "rbv2_super"
PLOTTITLE <- "Randombot"

dat <- prepareData("lcbench", 500)
dat <- prepareData("nb301", 500)
dat <- prepareData("rbv2_super", 500)

# Aggregate on per task level (across all replications)
dfa = dat[, .(mean_nr = mean(perf_cum), sd_nr = sd(perf_cum), n = .N), by = c("algorithm", "task", "budget_cum")]

# Aggregation across tasks
inst = instances[cfg == PROBLEM, ]
test = inst[test == TRUE, ]$level
train = inst[test == FALSE, ]$level

# Learning curves, aggregated across tasks, test set
dfa_train = dfa[task %in% test][, .(mean_nr_across_tasks = mean(mean_nr), sde_across_tasks = sqrt(mean(sd_nr^2) / n[1] )), by = c("algorithm", "budget_cum")]
dfa_train$lower = dfa_train$mean_nr_across_tasks - 2 * dfa_train$sde_across_tasks
dfa_train$upper = dfa_train$mean_nr_across_tasks + 2 * dfa_train$sde_across_tasks


p = ggplot(data = dfa_train) + geom_line(aes(x = budget_cum / MAXFID, y = mean_nr_across_tasks, colour = algorithm))
p = p + geom_ribbon(aes(x = budget_cum / MAXFID, ymin = lower, ymax = upper, fill = algorithm), alpha = 0.2)
p = p + scale_colour_Publication() + scale_fill_Publication()
p = p + xlab("Budget (in multiples of max. budget)") + ylab("Mean normalized regret")
p = p + ggtitle(paste0(PLOTTITLE))

plog = p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))

ploglog = plog + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))
ploglog

# TODO: smac initial design size: nmax / 4



head(order(unique(dfa_train[algorithm == "SMAC"][order(budget_cum)])[, diff(mean_nr_across_tasks)]), 20)

unique(dfa_train[algorithm == "SMAC"][order(budget_cum)])[131:140]

1808 / 52

52 * 7 * 4

3484 / 52 /



allmins <- rbindlist(lapply(c("lcbench","rbv2_super", "nb301"), function(PROBLEM) {
  fps <- list.files(sprintf("../results/reduced_files/%s/sequential", PROBLEM), full.names = TRUE)
  df <- readAndConcatenateFiles(fps)
  df[, min(performance), by = c("task", "problem", "algorithm")]
}))










return(list(df = df, dfa_train = dfa_train))
}
# saveRDS(l, "plot_lcbench_32.rds")
# saveRDS(l, "plot_lcbench_single.rds")
# saveRDS(l, "plot_rbv2_single.rds")

l <- preparedataset()
dfa_train <- l$dfa_train
df <- l$df

p = ggplot(data = dfa_train) + geom_line(aes(x = budget_cum / MAXFID, y = mean_nr_across_tasks, colour = algorithm))
p = p + geom_ribbon(aes(x = budget_cum / MAXFID, ymin = lower, ymax = upper, fill = algorithm), alpha = 0.2)
p = p + scale_colour_Publication() + scale_fill_Publication()
p = p + xlab("Budget (in multiples of max. budget)") + ylab("Mean normalized regret")
p = p + ggtitle(paste0(PLOTTITLE))
p


plog = p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))
plog


ploglog = plog + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))
ploglog


# log-log?
p = ggplot(data = dfa_train) + geom_line(aes(x = budget_cum / MAXFID, y = log(mean_nr_across_tasks), colour = algorithm))
p = p + geom_ribbon(aes(x = budget_cum / MAXFID, ymin = log(lower), ymax = log(upper), fill = algorithm), alpha = 0.2)
p = p + scale_colour_Publication() + scale_fill_Publication()
p = p + xlab("Budget (in multiples of max. budget)") + ylab("Mean normalized regret")
p = p + ggtitle(paste0(PLOTTITLE))
plog = p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))
plog

# rank analysis
rank_table = lapply(c(10^seq(0, log10(30 * DIM), by = 1), 30 * DIM), function(bmax) {
    rt = df[task %in% test & budget_cum <= MAXFID * bmax]
    # rt = df[task %in% test, .SD[budget_cum <= MAXFID * bmax]]  # crashes for some reason
    rt = rt[, .SD[which.min(performance)], by = c("task", "job.id", "algorithm")]
    rt = rt[, repl := 1:.N, by = c("task", "algorithm")]
    rt[, rank := rank(performance), by = c("repl", "task")]
    rt = rt[, .(mean_rank_across_repls = mean(rank)), by = c("task", "algorithm")]
    rt = rt[, .(mean_rank = round(mean(mean_rank_across_repls), 2)), by = c("algorithm")]
    rt$bmax = MAXFID * bmax
    rt
})
rank_table = do.call(rbind, rank_table)
rank_table[, bmax := round(bmax / MAXFID)]
dcast(rank_table, algorithm ~ bmax, value.var = "mean_rank")








PROBLEM <- "lcbench"
PLOTTITLE <- "LCbench"
MAXFID <- 52
DIM <- 7
fvals <- c(2, 6, 17, 52)
hblens.bohb <- c(27, 9, 3, 1, 9, 3, 1, 6, 2, 4)
HBSCHEDULE.BOHB <- data.table(
  fid = rep(c(1, 2, 3, 4, 2, 3, 4, 3, 4, 4), hblens.bohb),
  bracket = rep(c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4), hblens.bohb)
)

hblens.hb <- c(27, 9, 3, 1, 12, 4, 1, 6, 2, 4)
HBSCHEDULE.HB <- data.table(
  fid = rep(c(1, 2, 3, 4, 2, 3, 4, 3, 4, 4), hblens.hb),
  bracket = rep(c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4), hblens.hb)
)



DO.32 <- TRUE

l <- preparedataset()
dfa_train <- l$dfa_train
df <- l$df
saveRDS(dfa_train, "plot_lcbench_32.rds")
saveRDS(df, "all_lcbench_32.rds")

DO.32 <- FALSE
l <- preparedataset()
dfa_train <- l$dfa_train
df <- l$df
saveRDS(dfa_train, "plot_lcbench_single.rds")
saveRDS(df, "all_lcbench_single.rds")


PROBLEM <- "rbv2_super"
PLOTTITLE <- "rbv2_super"
MAXFID <- 1
DIM <- 38
hblens.bohb <- c(27, 9, 3, 1, 9, 3, 1, 6, 2, 4)
HBSCHEDULE.BOHB <- data.table(
  fid = rep(c(1, 2, 3, 4, 2, 3, 4, 3, 4, 4), hblens.bohb),
  bracket = rep(c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4), hblens.bohb)
)

hblens.hb <- c(27, 9, 3, 1, 12, 4, 1, 6, 2, 4)
HBSCHEDULE.HB <- data.table(
  fid = rep(c(1, 2, 3, 4, 2, 3, 4, 3, 4, 4), hblens.hb),
  bracket = rep(c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4), hblens.hb)
)


DO.32 <- TRUE
l <- preparedataset()
dfa_train <- l$dfa_train
df <- l$df
saveRDS(dfa_train, "plot_rbv2_32.rds")
saveRDS(df, "all_rbv2_32.rds")



DO.32 <- FALSE
l <- preparedataset()
dfa_train <- l$dfa_train
df <- l$df
saveRDS(dfa_train, "plot_rbv2_single.rds")
saveRDS(df, "all_rbv2_single.rds")




PROBLEM <- "nb301"
PLOTTITLE <- "NB301"
MAXFID <- 98
DIM <- 34

hblens.bohb <- c(81, 27, 9, 3, 1, 27, 9, 3, 1, 9, 3, 1, 6, 2, 5)
HBSCHEDULE.BOHB <- data.table(
  fid = rep(c(1, 2, 3, 4, 5, 2, 3, 4, 5, 3, 4, 5, 4, 5, 5), hblens.bohb),
  bracket = rep(c(1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 5), hblens.bohb)
)

hblens.hb <- c(81, 27, 9, 3, 1, 34, 11, 3, 1, 15, 5, 1, 8, 2, 5)
HBSCHEDULE.HB <- data.table(
  fid = rep(c(1, 2, 3, 4, 5, 2, 3, 4, 5, 3, 4, 5, 4, 5, 5), hblens.hb),
  bracket = rep(c(1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 5), hblens.hb)
)


DO.32 <- TRUE
l <- preparedataset()
dfa_train <- l$dfa_train
df <- l$df
saveRDS(dfa_train, "plot_nb_32.rds")
saveRDS(df, "all_nb_32.rds")


DO.32 <- FALSE
l <- preparedataset()
dfa_train <- l$dfa_train
df <- l$df
saveRDS(dfa_train, "plot_nb_single.rds")
saveRDS(df, "all_nb_single.rds")



# ---------------------------------------------------------------------



dfa_train <- readRDS("plot_lcbench_single.rds")



# ----------------------------------------------------------------

rb1 <- readRDS("all_rbv2_single.rds")
rb32 <- readRDS("all_rbv2_32.rds")
lc1 <- readRDS("all_lcbench_single.rds")
lc32 <- readRDS("all_lcbench_32.rds")
nb1 <- readRDS("all_nb_single.rds")
nb32 <- readRDS("all_nb_32.rds")



all1 <- rbind(rb1, lc1, nb1)
all32 <- rbind(rb1, lc1, nb1)



rb1perf <- all1[budget_cum <= 1 * 30 * ifelse(problem == "rbv2_super", 38, ifelse( problem == "nb301",  34 * 98, 7 * 52))][,
  .(best = min(regret_cummin)), by = c("job.id", "algorithm", "task", "problem")]
singularperf <- rb1perf[, .(meanx = mean(best), sdx = sd(best), n = .N), by = c("algorithm", "task", "problem")]
ymaxmins <- all1[, .(y_max = y_max[[1]], y_min = y_min[[1]], diff(range(y_max)), diff(range(y_min))) , by = c("task")]


saveRDS(singularperf, "singularperf.rds")
saveRDS(ymaxmins, "ymaxmins.rds")

rb32perf <- all32[budget_cum <= 1 * 30 * ifelse(problem == "rbv2_super", 38, ifelse( problem == "nb301",  34 * 98, 7 * 52)) / 8][,
  .(best = min(regret_cummin)), by = c("job.id", "algorithm", "task", "problem")]
multicoreperf <- rb32perf[, .(meanx = mean(best), sdx = sd(best), n = .N), by = c("algorithm", "task", "problem")]

saveRDS(multicoreperf, "multicoreperf.rds")

allmedians <- all1[algorithm == "RS", median(performance), by = c("problem", "task")]

saveRDS(allmedians, "medians.rds")


nb1[, .(px = (y_max - y_min) * regret + y_min, performance)]




expoverview <- readRDS("results_new/experiments.rds")

expoverview[[1]][[3]]


dimmap <- structure(c(7, 38, 34), names = c("lcbench", "rbv2_super", "nb301"))
fidmap <- structure(c(52, 1, 98), names = c("lcbench", "rbv2_super", "nb301"))

getRelevant <- function(rq) {
  rq[, completion := cumbudget / dimmap[cfg] / fidmap[cfg] / 30 / ifelse(rq == "rq7", 8, 1)]
  perf1 <- rq[completion <= 1][, .(perf = last(best)), by = c("cfg", "task", "repl", "id", "rqn")]
  result <- perf1[, .(meanx = mean(perf), sdx = sd(perf), n = .N), by = c("cfg", "rqn", "task")][,
    .(meanx = mean(meanx), sdx = sqrt(mean(sdx^2) ), n = sum(n)), by = c("cfg", "rqn")]
  list(result = result, range = rq[, lapply(.SD, range, na.rm = TRUE), .SDcols = c("val_cross_entropy", "logloss", "val_accuracy"), by = "task"])
}

relevants <- sapply(c("1", "4", "5a", "5b", "5a_cond", "5b_cond", "6", "7"), function(x) {
  print(x)
  getRelevant(readRDS(sprintf("results_new/results_rq%s.rds", x)))
}, simplify = FALSE)

re3 <- lapply(re2, getRelevant)

saveRDS(relevants, "smashy_results.rds")





rq1 <- readRDS("results_new/results_rq1.rds")
rq4 <- readRDS("results_new/results_rq4.rds")
rq5a <- readRDS("results_new/results_rq5a.rds")
rq5b <- readRDS("results_new/results_rq5b.rds")
rq6 <- readRDS("results_new/results_rq6.rds")
rq7 <- readRDS("results_new/results_rq7.rds")


rbind(rq1, rq4, rq5a, rq5b, rq6, rq7, fill = TRUE)






rq1[, completion := cumbudget / dimmap[cfg] / fidmap[cfg] / 30]

perf1 <- rq1[completion <= 1][, .(perf = last(best)), by = c("cfg", "task", "repl", "id", "rqn")]

rq1res <- perf1[, .(meanx = mean(perf), sdx = sd(perf), n = .N), by = c("cfg", "rqn", "task")][,
  .(meanx = mean(meanx), sdx = sqrt(mean(sdx^2) ), n = sum(n)), by = c("cfg", "rqn")]



ggplot(rq1res, aes(y = rqn, x = meanx, color = cfg)) + geom_point() + geom_errorbar(aes(xmin = meanx - sdx, xmax = meanx + sdx))


perf1[cfg == "nb301"][, .(meanx = mean(perf), sdx = sd(perf), n = .N), by = c("cfg", "rqn", "task")]

rq1[J()]
