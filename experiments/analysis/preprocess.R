source("fileloader.R")


problem <- Sys.getenv("PROBLEM")

TESTRUN <- Sys.getenv("NOTEST") == ""

RESOLUTIONS <- assertNumeric(as.numeric(strsplit(Sys.getenv("RESOLUTION"), " ")[[1]]), any.missing = FALSE)

assertSubset(problem, c("lcbench", "rbv2_super", "nb301"))


# check things...
# ------------------
meta.experiments <- readRDS("../results_new/experiments.rds")
ty <- split(meta.input, by = "rq")
tx <- meta.experiments$tbl
names(tx) <- meta.experiments$rqx
stopifnot(all(names(tx) == names(ty)))

stopifnot(all(mapply(function(x, y) all(x$batch_method == y$batchmethod), tx, ty)))

print(tx[[7]]$surrogate_learner == ty[[7]]$surrogate_learner)
# ------------------

# create cached demo files explicitly; should not be necessary.
if (FALSE) {
  lapply(px, prepareDemoFiles)
}

l <- prepareData(problem, demorun = TESTRUN)
fulldata <- l$df

yrangetbl <- l$yrangetbl
rm(l)

df <- rbindlist(lapply(unique(meta.input$rq), loadSmashyData, problem = problem, demorun = TESTRUN))

dfall <- rbind(fulldata, df, fill = TRUE)

saveRDS(yrangetbl, sprintf("results_yrangetbl_%s_%s.rds", problem, if (TESTRUN) "TEST" else "REAL"))
for (res in RESOLUTIONS) {
  grid <- if (res) {
    makeGrid(dfall, res)
  } else {
    metainfo[problem, maxfidelity] * c(1, 10, 100, Inf)
  }

  saveRDS(subsampleGrid(dfall, grid), sprintf("results_preprocessed_%s_%s_%s.rds", problem, if (TESTRUN) "TEST" else "REAL", res))
}
