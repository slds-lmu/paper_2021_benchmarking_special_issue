# RQ 1, 2, 3
library(data.table)

collected = readRDS("collected_2021-09-15_2.rds")
canonical <- collected[mu == "muvary" & curseed == -1]
lambdas.1 <- canonical[siman & infillsearch == "all", ]$lambda

rq.1.tbl <- cbind(canonical[siman & infillsearch == "all", c("objective", "batchmethod")], rbindlist(lambdas.1))
rq.1.tbl[, rq := "rq1"]
rq.1.tbl[, rqn := seq_len(.N)]
rq.1.tbl[, algorithm := paste0(rq, "_", rqn)]



# RQ 4
rq.4.tbl <-  cbind(canonical[siman & infillsearch == "all", c("objective", "batchmethod")], rbindlist(lambdas.1)[, budget_log_step := 100])
rq.4.tbl[, batch_method := canonical[siman & infillsearch == "all", ]$batchmethod]

# RQ 5

# | conditional opt
lambdas.2 <- canonical[!siman & infillsearch == "all", ]$lambda
rq.5a.tbl <- rbindlist(lambdas.2)
rq.5a.tbl[, batch_method := canonical[!siman & infillsearch == "all", ]$batchmethod]

# | setting values directly
rq.5a.tbl.cond <- rbindlist(lambdas.1)[,
  c("filter_factor_last", "filter_factor_last.end") := sqrt(filter_factor_last.end * filter_factor_last)][,
  c("filter_factor_first", "filter_factor_first.end") := sqrt(filter_factor_first.end * filter_factor_first)][,
  c("filter_select_per_tournament", "filter_select_per_tournament.end") := round(sqrt(filter_select_per_tournament.end * filter_select_per_tournament))][,
  c("random_interleave_fraction", "random_interleave_fraction.end") := (random_interleave_fraction + random_interleave_fraction.end) / 2][]
rq.5a.tbl.cond[, batch_method := canonical[siman & infillsearch == "all", ]$batchmethod]

# RQ 5b
# | conditional opt
lambdas.3 <- canonical[!siman & infillsearch == "rs", ]$lambda
rq.5b.tbl <- rbindlist(lambdas.3)
rq.5b.tbl[, batch_method := canonical[!siman & infillsearch == "rs", ]$batchmethod]

rq.5b.tbl.cond <- copy(rq.5a.tbl.cond)[,
  c("filter_factor_first", "filter_factor_last", "filter_factor_first.end", "filter_factor_last.end") :=
    sqrt(filter_factor_first * filter_factor_last)][filter_algorithm != "tournament", filter_select_per_tournament := 1][,
  filter_algorithm := "tournament"][]
rq.5b.tbl.cond[, batch_method := canonical[siman & infillsearch == "all", ]$batchmethod]

# RQ 6
t1 <- rbindlist(canonical[siman & infillsearch == "all" & batchmethod == "smashy"]$lambda)[,
  original_surrogate_learner := surrogate_learner][,
  surrogate_learner := NULL][, id := 1:2]
t1[, batch_method := canonical[siman & infillsearch == "all" & batchmethod == "smashy"]$batchmethod]
t2 <- CJ(id = 1:2, surrogate_learner = c("knn1", "knn7", "bohblrn", "ranger"))

surgrid <- t1[t2, on = "id"][, id := NULL][surrogate_learner != original_surrogate_learner][, original_surrogate_learner := NULL][]
rigrid <- rbind(
  rbindlist(canonical[siman & infillsearch == "all" & batchmethod == "smashy"]$lambda)[,
    c("random_interleave_fraction", "random_interleave_fraction.end") := 1],
  rbindlist(canonical[siman & infillsearch == "all" & batchmethod == "smashy"]$lambda)[,
    c("random_interleave_fraction", "random_interleave_fraction.end") := 0]
)
rigrid[, batch_method := c(canonical[siman & infillsearch == "all" & batchmethod == "smashy"]$batchmethod, canonical[siman & infillsearch == "all" & batchmethod == "smashy"]$batchmethod)]

rq.6.tbl <- rbind(surgrid, rigrid, use.names = TRUE)
rq.6.tbl[, rq := "rq6"]
rq.6.tbl[, rqn := seq_len(.N)]
rq.6.tbl[, algorithm := paste0(rq, "_", rqn)]



# RQ 7
# special:
# budgetfactor: 8 * 30 == 240
rq.7.tbl.BUDGETFACTOR <- rbindlist(canonical[siman & infillsearch == "all" & batchmethod == "smashy"]$lambda)[, mu := 32]
rq.7.tbl.BUDGETFACTOR[, batch_method := canonical[siman & infillsearch == "all" & batchmethod == "smashy"]$batchmethod]

# RQ 6 fix
t1 <- rbindlist(canonical[siman & infillsearch == "all" & batchmethod == "smashy"]$lambda)[,
  original_surrogate_learner := surrogate_learner][,
  surrogate_learner := NULL][, id := 1:2]
t1[, batch_method := canonical[siman & infillsearch == "all" & batchmethod == "smashy"]$batchmethod]
t2 <- CJ(id = 1:2, surrogate_learner = c("knn1", "knn7", "bohblrn", "ranger"))

surgrid <- t1[t2, on = "id"][, id := NULL][surrogate_learner != original_surrogate_learner][, original_surrogate_learner := NULL][]
rigrid <- rbind(
  rbindlist(canonical[siman & infillsearch == "all" & batchmethod == "smashy"]$lambda)[,
    c("random_interleave_fraction", "random_interleave_fraction.end") := 1][,
      sample := "random"],
  rbindlist(canonical[siman & infillsearch == "all" & batchmethod == "smashy"]$lambda)[,
    c("random_interleave_fraction", "random_interleave_fraction.end") := 0]
)
rigrid[, batch_method := c(canonical[siman & infillsearch == "all" & batchmethod == "smashy"]$batchmethod, canonical[siman & infillsearch == "all" & batchmethod == "smashy"]$batchmethod)]

rq.6.tbl_fix <- rbind(surgrid, rigrid, use.names = TRUE)
rq.6.tbl_fix[, rq := "rq6_fix"]
rq.6.tbl_fix[, rqn := seq_len(.N)]
rq.6.tbl_fix[, algorithm := paste0(rq, "_", rqn)]



#rq.1.tbl
#rq.4.tbl
#rq.5a.tbl
#rq.5a.tbl.cond
#rq.5b.tbl
#rq.5b.tbl.cond
#rq.6.tbl
#rq.7.tbl.BUDGETFACTOR
#rq.6.tbl_fix

# FIXME: rq.6.tbl_fix to rq.6.tbl

