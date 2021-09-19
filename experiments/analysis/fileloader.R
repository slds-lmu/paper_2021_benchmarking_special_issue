

data.table::setDTthreads(parallel::detectCores())
options(warn=1)

library("data.table")
library("checkmate")

instances.location <- "~/development/R/miesmuschel/attic/data/instances.rds"

readAndConcatenateFiles = function(filepaths) {
  reslist = lapply(filepaths, function(file) {
    x = readRDS(file)
    out = lapply(seq_len(nrow(x)), function(i) {
      cbind(job.id = x$job.id[i], x$result[[i]])
    })
    out = do.call(rbind, out)
    batchtools::ijoin(x[, c("job.id", "problem", "task", "objectives", "algorithm")], out[, c("job.id", "budget", "performance")], by = "job.id")
  })
  do.call(rbind, reslist)
}

###### ---------------- inferring HB schedule info

hblens.bohb.3 <- c(27, 9, 3, 1, 9, 3, 1, 6, 2, 4)
hblens.bohb.4 <- c(81, 27, 9, 3, 1, 27, 9, 3, 1, 9, 3, 1, 6, 2, 5)
hblens.hb.3 <- c(27, 9, 3, 1, 12, 4, 1, 6, 2, 4)
hblens.hb.4 <- c(81, 27, 9, 3, 1, 34, 11, 3, 1, 15, 5, 1, 8, 2, 5)

makeSched  <- function(numfid, hblens) {
  data.table(
      fid = rep(unlist(sapply(1:numfid, function(x) x:numfid)), hblens),
      bracket = rep(rep(1:numfid, numfid:1), hblens)
  )
}

getCostSchedule <- function(hbsched, cores = 32) {
  curbracket <- 0
  curfid <- 0
  result <- numeric(0)
  for (i in seq_len(nrow(hbsched))) {
    if (hbsched[i, fid] != curfid || hbsched[i, bracket] != curbracket || occupied > cores) {
      result[[i]] = 1
      curfid = hbsched[i, fid]
      curbracket = hbsched[i, bracket]
      occupied = 1
    } else {
      result[[i]] = 0
      occupied <- occupied + 1
    }
  }
  result
}

###### ---------------- information about problem sets

metainfo <- data.table(
  name =             c("lcbench",                          "rbv2_super",                             "nb301"),
  prettyname =       c("LCbench",                          "rbv2_super",                             "NAS-Bench-301"),
  minfidelity =      c(round(52 / 27),                     1 / 27,                                   round(98 / 81)),
  maxfidelity =      c(52,                                 1,                                        98),
  dimension =        c(7,                                  38,                                       34),
  schedule.hb =   list(makeSched(4, hblens.hb.3),          makeSched(4, hblens.hb.3),                makeSched(5, hblens.hb.4)),
  schedule.bohb = list(makeSched(4, hblens.bohb.3),        makeSched(4, hblens.bohb.3),              makeSched(5, hblens.bohb.4))
)

metainfo[, budgetlimit.1 := dimension * maxfidelity * 30]
metainfo[, budgetlimit.32 := dimension * maxfidelity * 30 / 8]

setkeyv(metainfo, "name")


###### ---------------- load raw data (non-smashy)

prepareDemoFiles <- function(problem, rq = NULL) {
  instances <- readRDS(instances.location)
  testlevels = instances[cfg == problem & test == TRUE, level]

  if (is.null(rq)) {
    rq <- "external"
    fps <- list.files(sprintf("../results/reduced_files/%s/sequential", problem), full.names = TRUE)
    df <- readAndConcatenateFiles(fps)
  } else {
    df <- readRDS(sprintf("../results_new/results_%s.rds", rq))[cfg == problem]
  }
  if (any(!is.na(testlevels))) {
    tasks <- head(intersect(sort(unique(df$task)), testlevels), 4)
  } else {
    tasks <- NA
  }
  df <- df[task %in% tasks]
  dir.create("../results/reduced_files/demo", showWarnings = FALSE, recursive = TRUE)
  saveRDS(df, sprintf("../results/reduced_files/demo/demo_%s_%s.rds", rq, problem))
}

###### ---------------- dataset helper functions

# df: data.table with columns
# performance, perf_cum, budget, budget_cum, job.id, task, algorithm
# for each repetition within a task (uniquely identified by job.id), and for each budget_cum grid position
# there should be a point. however, some runs stop slightly too early so we impute the last points.
#
# grid points can be different between different algorithms.
imputeDF <- function(df) {

  # how many reps should there be at each budget_cum position for a given algorithm x task? I.e. how many were started?
  wantedN <- df[, length(unique(job.id)), keyby = c("algorithm", "task")]

  # all job.ids and its related algorithm x task. Note job.ids can be duplicated between algorithm x tasks
  jobids <- unique(df[, .(algorithm, task, job.id)])
  setkeyv(jobids, c("algorithm", "task"))

  # for each algorithm, where are the budget grid points? Note that these may differ between *problems*, df only
  # contains a single algorithm here.
  budgetpoints <- unique(df[, .(algorithm, budget_cum)])

  crosstask <- df[, CJ(algorithm, task, unique = TRUE)]

  # for each algorithm x task the budget_cum grid points that should be present
  allgridpoints <- budgetpoints[crosstask, on = "algorithm", allow.cartesian = TRUE]

  # the actual number of reps for a given algorithm x task and budget_cum grid point
  repcount <- df[allgridpoints, .(job.id, .N), on = c("algorithm", "task", "budget_cum"), by = .EACHI]

  # table algorithm, task, budget_cum, job.id (all job.ids that were run for the given algorithm x task & budget_cum point; NA if none),
  # N (number of reps for which the point is given), and V1 (number of reps that are needed)
  neededpoints <- wantedN[repcount, on = c("algorithm", "task")][N != V1]

  if (nrow(neededpoints)) {
    # find out what job.id needs to have algorithm x task x budget_cum points filled in:
    # take the neededpoints table, and take the complement of the job.id column: take all
    # job.ids for which the point has *not* been evaluated.
    missingpoints <- neededpoints[,
      .(job.id = setdiff(jobids[.BY, on = c("algorithm", "task"), job.id], job.id)),
      by = c("algorithm", "task", "budget_cum")]

    # carry the perf_cum
    newpoints <- df[missingpoints, on = c("algorithm", "task", "job.id", "budget_cum"), roll = Inf]
    newpoints[, perf_cum_unimputed := NA_real_]  # indicate that the point was imputed

    df <- rbind(df, newpoints)

    repcount <- df[allgridpoints, .(job.id, .N), on = c("algorithm", "task", "budget_cum"), by = .EACHI]

    repcount <- df[, .(job.id, .N), by = c("task", "algorithm", "budget_cum")]
    if (nrow(wantedN[repcount, on = c("algorithm", "task")][N != V1])) stop("missingpoints failed in some way: did not fill all points.")
  }
  df
}

scaleDist <- function(budget, minbudget, maxbudget, resolution) {
  scaleDist.linear <- (maxbudget - minbudget) / resolution
  scaleDist.log <- budget * (maxbudget / minbudget)^(1 / resolution) - budget
  min(scaleDist.linear, scaleDist.log)
}

makeGrid1D <- function(budget_cum, minbudget, maxbudget, resolution) {
  prev <- budget_cum[[1]]
  result <- numeric(0)
  nextpoint <- prev + scaleDist(prev, minbudget, maxbudget, resolution)
  for (nxt in budget_cum) {
    if (nxt < nextpoint) next
    nextpoint <- prev + scaleDist(prev, minbudget, maxbudget, resolution)
    result[[length(result) + 1]] <- prev
    prev <- nxt
  }
  c(result, unique(c(prev, nxt)))
}


###### ---------------- exports

prepareData <- function(problem, demorun = TRUE) {
  if (demorun) {
    fname <- sprintf("../results/reduced_files/demo/demo_external_%s.rds", problem)
    if (!file.exists(fname)) prepareDemoFiles(problem)
    df <- readRDS(fname)
  } else {
    fps <- list.files(sprintf("../results/reduced_files/%s/sequential", problem), full.names = TRUE)
    df <- readAndConcatenateFiles(fps)
    instances <- readRDS(instances.location)
    testlevels = instances[cfg == problem & test == TRUE, level]
    df <- df[task %in% testlevels]
  }

  budgetlimit.1 <- metainfo[problem, budgetlimit.1]
  budgetlimit.32 <- metainfo[problem, budgetlimit.32]

  if (problem == "nb301") {
    df$performance = -abs(df$performance)
  }

  stopifnot(length(unique(df$problem)) == 1)
  df[, `:=`(problem = NULL, objectives = NULL)]

  yrangetbl <- df[,
    .(y_min = min(performance),
      y_max = max(performance),
      y_median = .SD["randomsearch_full_budget", median(performance), on = "algorithm"],
      problem),
    by = "task"]

  budgetfactor <- min(df$budget)

  df[, `:=`(perf_cum = cummin(performance), budget_cum = cumsum(round(budget / budgetfactor)) * budgetfactor), by = c("job.id", "algorithm", "task")]
  df[, `:=`(perf_cum_unimputed = perf_cum, performance = NULL)]

  # multicore BOHB simulation
  counting <- getCostSchedule(metainfo[problem, schedule.bohb][[1]])
  dfbohb <- df[algorithm == "hpbster_bohb", cbind(.SD, counting)[counting == 1], by = c("job.id", "algorithm", "task")]
  dfbohb[, budget_cum := cumsum(round(budget / budgetfactor)) * budgetfactor, by = c("job.id", "algorithm", "task")]
  badtasks <- dfbohb[, any(budget_cum > budgetlimit.32), by = c("task", "job.id")][, mean(V1), by = "task"][V1 < .9, task]
  if (length(badtasks)) warning(sprintf("Not enough budget for tasks %s in problem %s for bohb.", paste(head(badtasks, 3), collapse = ", "), problem))
  dfbohb[, counting := NULL]

  # multicore HB simulation
  counting <- getCostSchedule(metainfo[problem, schedule.hb][[1]])
  dfhb <- df[algorithm == "mlr3hyperband", cbind(.SD, counting)[counting == 1], by = c("job.id", "algorithm", "task")]
  dfhb[, budget_cum := cumsum(round(budget / budgetfactor)) * budgetfactor, by = c("job.id", "algorithm", "task")]
  badtasks <- dfhb[, any(budget_cum > budgetlimit.32), by = c("task", "job.id")][, mean(V1), by = "task"][V1 < .9, task]
  if (length(badtasks)) warning(sprintf("Not enough budget for tasks %s in problem %s for HB.", paste(head(badtasks, 3), collapse = ", "), problem))
  dfhb[, counting := NULL]

  # multicore RS simulation
  dfrs <- df[algorithm == "randomsearch_full_budget"]
  dfrs[, budget_cum := ceiling(budget_cum / metainfo[problem, maxfidelity] / 32) * metainfo[problem, maxfidelity], by = "task"]
  dfrs <- dfrs[, .(budget = max(budget), perf_cum_unimputed = min(perf_cum_unimputed), perf_cum = min(perf_cum)),
    by = c("job.id", "task", "algorithm", "budget_cum")]
  badtasks <- dfrs[, any(budget_cum > budgetlimit.32), by = c("task", "job.id")][, mean(V1), by = "task"][V1 < .9, task]
  if (length(badtasks)) warning(sprintf("Not enough budget for tasks %s in problem %s for RS.", paste(head(badtasks, 3), collapse = ", "), problem))

  if (FALSE) {  # smac multicore analysis doesn't work
    dfsm <- df[algorithm == "smac_full_budget"]
    set.seed(1)
    dfsmbeginning <- dfsm[budget_cum == min(budget_cum)][, .(job.id, identifier = sample(head(rep(seq_len(.N / 32 + 1), each = 32), .N))), by = "task"]
    dfsm <- dfsmbeginning[dfsm, on = "job.id"]
    dfsm <- dfsm[, .(budget = max(budget), perf_cum_unimputed = min(perf_cum_unimputed), perf_cum = min(perf_cum), job.id = first(job.id)),
      by = c("identifier", "task", "algorithm", "budget_cum")]
    badtasks <- dfsm[, any(budget_cum > budgetlimit.32), by = c("task")][V1 == FALSE, task]
    if (length(badtasks)) warning(sprintf("Not enough budget for tasks %s in problem %s for SMAC.", paste(head(badtasks, 3), collapse = ", "), problem))
    dfsm[, `:=`(
      job.id = identifier,
      identifier = NULL,
      budget_cum = budget_cum - 0 * (smacinitbudget - ceiling(smacinitbudget / 32)) * metainfo[problem, maxfidelity]
    )]
  }

  df32 <- rbind(dfhb, dfbohb, dfrs)

  df <- df[budget_cum <= budgetlimit.1]
  df32 <- df32[budget_cum <= budgetlimit.32]

  df <- imputeDF(df)
  df32 <- imputeDF(df32)

  df[, parallel := FALSE]
  df32[, parallel := TRUE]

  df <- rbind(df, df32)

  algonamemap <- c(hpbster_bohb = "BOHB", mlr3hyperband = "HB", randomsearch_full_budget = "RS", smac_full_budget = "SMAC")
  df[, algorithm := {
    algorithm <- as.factor(algorithm)
    levels(algorithm) <- algonamemap[levels(algorithm)] # Beautification
    as.character(algorithm)
  }]

  list(df = df[], yrangetbl = yrangetbl)
}

# make a grid so that on linear *and* on log scale the distances between points are less than <range> / resolution
makeGrid <- function(df, resolution) {
  if (is.infinite(resolution)) return(df)

  budgetmin <- metainfo[problem, minfidelity]
  budgetmax.1 <- metainfo[problem, dimension] * metainfo[problem, maxfidelity] * 30
  budgetmax.32 <- metainfo[problem, dimension] * metainfo[problem, maxfidelity] * 30 / 8

  unique(df[, .(parallel, algorithm, budget_cum)])[, .(budget_cum = makeGrid1D(budget_cum, budgetmin, budgetmax.1, resolution)), by = c("parallel", "algorithm")]
}

# 'grid' can be a vector of budget_cum points, or
subsampleGrid <- function(df, grid) {
  if (is.numeric(grid)) {
    grid <- df[, CJ(parallel, algorithm, budget_cum = grid, unique = TRUE)]
  }
  assertDataTable(grid)
  assertNames(colnames(grid), must.include = c("parallel", "algorithm", "budget_cum"))

  fullgrid <- unique(df[, .(job.id, task, algorithm, parallel)])[grid, on = c("parallel", "algorithm"), allow.cartesian = TRUE, nomatch = NULL]

  # grid[, .(parallel, algorithm, budget_cum)]

  result1 <- df[fullgrid, .SD,
    on = c("parallel", "algorithm", "task", "job.id", "budget_cum"),
    by = .EACHI,
    roll = Inf, nomatch = NULL
  ]
  result2 <- df[fullgrid, .(perf_cum_unimputed, inflength = i.budget_cum - x.budget_cum),
    on = c("parallel", "algorithm", "task", "job.id", "budget_cum"),
    by = .EACHI,
    roll = Inf, nomatch = NULL
  ]



  cbind(result1, perf_cum_grid_unimputed =   result2[, ifelse(inflength > metainfo[problem, maxfidelity], NA_real_, perf_cum_unimputed)])
}

###### ---------------- load smashy data


meta.input <- readRDS("../results_new/metainfo.rds")
meta.input[, rqn := seq_len(.N), by = "rq"]
meta.input[rq == "rq6_fix", rqn := 7:8]
setkeyv(meta.input, c("rq", "rqn"))



loadSmashyData <- function(rq, problem, demorun = TRUE) {
  if (demorun) {
    fname <- sprintf("../results/reduced_files/demo/demo_%s_%s.rds", rq, problem)
    if (!file.exists(fname)) prepareDemoFiles(problem, rq)
    df <- readRDS(fname)
  } else {
    df <- readRDS(sprintf("../results_new/results_%s.rds", rq))[cfg == problem]
  }

  budgetlimit.1 <- metainfo[problem, dimension] * metainfo[problem, maxfidelity] * 30
  budgetlimit.32 <- metainfo[problem, dimension] * metainfo[problem, maxfidelity] * 30 / 8

  df <- df[, .(
    job.id = id,
    task,
    algorithm = paste(rq, rqn, sep = "."),
    rq = rq,
    rqn = rqn,
    budget = budget,
    perf_cum = best,
    budget_cum = cumbudget,
    perf_cum_unimputed = best,
    parallel = meta.input[.SD, parallel, on = c("rq", "rqn")]
  )]

  if (rq == "rq7") {
    df[, batch := ceiling(seq_len(.N) / 32), by = c("job.id", "task", "algorithm")]
    df <- df[, .(
        budget = max(budget),
        perf_cum = min(perf_cum),
        budget_cum = ceiling(max(budget_cum) / 32),
        perf_cum_unimputed = min(perf_cum_unimputed, na.rm = TRUE),
        parallel = assertTRUE(all(parallel))),
      by = c("job.id", "task", "algorithm", "rq", "rqn", "batch")][, batch := NULL]
  }
  df[budget_cum <= metainfo[problem, ifelse(parallel, budgetlimit.32 * 32, budgetlimit.1)]]
}
