safeSetupRegistry = function(registry_name, overwrite, packages, def) {

  if (!dir.exists(dirname(registry_name)))
    dir.create(dirname(registry_name), recursive = TRUE)

  if (file.exists(registry_name)) {
    if (overwrite) {
      unlink(registry_name, recursive = TRUE)
      reg = makeExperimentRegistry(file.dir = registry_name, seed = 123L,
        packages = packages, source = def)
    } else {
      reg = loadRegistry(registry_name, writeable = TRUE)
    }
  } else {
      reg = makeExperimentRegistry(file.dir = registry_name, seed = 123L,
        packages = packages, source = def)
  }
  return(reg)
}


readProblem = function(data, job, task, objectives, ...) {

  seconds = runif(1, 0, 45)
  Sys.sleep(seconds)

  nobjectives = length(objectives)

  dom = data$param_set

  # Get the upper budget limit
  param_ids = dom$ids()
  budget_param = data$budget_param

  # For the randombot data we drop repls as budget parameter
  if (data$id == "RBv2_super") {
    budget_param_all = budget_param
    budget_param = "trainsize"
  }

  budget_lower = dom$params[[budget_param]]$lower
  budget_upper = dom$params[[budget_param]]$upper

  # We give a total budget of lbmax * 30 * d
  BUDGET_MAX = B_MULTIPLIER * length(param_ids) * budget_upper

  if (data$model_name == "branin") {
    obj = data$get_objective()
    task = NA
  } else {
    if (is.na(task)) {
      obj = data$get_objective(target_variables = objectives)
      task = NA
    } else {
      obj = data$get_objective(task = task, target_variables = objectives)
    }
  }

  if (nobjectives == 1) {
    ins = OptimInstanceSingleCrit$new(
      objective = obj,
      terminator = trm("budget", budget = BUDGET_MAX, aggregate = sum)
    )

    if (data$id == "RBv2_super") {
      ts = list(as.integer(ins$search_space$params[[setdiff(budget_param_all, budget_param)]]$upper))
      names(ts) = setdiff(budget_param_all, budget_param)
      ins$search_space$values = c(ins$search_space$values, ts)
      ins$search_space$params[[budget_param]]$lower = 3^(-3)
    }
  }

  if (nobjectives > 1) {
    ins = OptimInstanceMultiCrit$new(
      objective = obj,
      terminator = trm("budget", budget = BUDGET_MAX, aggregate = sum)
    )
  }


  return(list(name = data$model_name, ins = ins, task = task))
}

compute_brackets = function(bupper, blower, eta) {
  smax = floor(log(bupper / blower, eta))
  B = (smax + 1) * bupper
  brackets = seq(0, smax)

  out = lapply(brackets, function(s) {
    n = ceiling(B / bupper * eta^s / (s + 1))
    r = bupper * eta^(-s)
    out = lapply(seq(0, s), function(i) {
      ni = floor(n * eta^(-i))
      ri = r * eta^i
      data.frame(ni = ni, ri = ri)
    })
    do.call(rbind, out)
  })
}


compute_total_budget = function(bupper, blower, eta) {
  smax = floor(log(bupper / blower, eta))
  B = (smax + 1) * bupper
  brackets = seq(0, smax)

  out = lapply(brackets, function(s) {
    n = ceiling(B / bupper * eta^s / (s + 1))
    r = round(bupper * eta^(-s))
    out = lapply(seq(0, s), function(i) {
      ni = floor(n * eta^(-i))
      ri = r * eta^i
      ni * ri
    })
    sum(unlist(out))
  })
  sum(unlist(out))
}

compute_total_evals = function(bupper, blower, eta) {
  smax = floor(log(bupper / blower, eta))
  B = (smax + 1) * bupper
  brackets = seq(0, smax)

  out = lapply(brackets, function(s) {
    n = ceiling(B / bupper * eta^s / (s + 1))
    r = bupper * eta^(-s)
    out = lapply(seq(0, s), function(i) {
      ni = floor(n * eta^(-i))
      ri = r * eta^i
      ni
    })
    sum(unlist(out))
  })
  sum(unlist(out))
}




plotAggregatedLearningCurves = function(df, var = "perf_mean", x = "budget", y_name = NULL, se = FALSE) {

  if (is.null(y_name)) {
    y_name = var
  }

  p = ggplot(data = df, aes_string(x = x, y = var, colour = "algorithm", fill = "algorithm"))
  if (se)
    p = p + geom_ribbon(aes_string(x = x, ymin = "lower", ymax = "upper", fill = "algorithm"), alpha = 0.25, colour = NA)
  p = p + geom_line()
  # p = p + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, colour = NA)
  p = p + theme_bw()
  # p = p + scale_x_continuous(breaks = seq(0, 4, by = 1),
  #         labels= 10^seq(0, 4))
  p = p + ylab(y_name)
  p
}




getResultsTable = function(tab, algo, prob, filedir, budget_max, objective_multiplier = 1) {

  print(paste("Reducing: ", algo))

  tored = tab[problem == prob & algorithm == algo, ]
  tored = ijoin(tored, findDone())

  tored = tored[, .SD, by = c("task")]

  jids = tored$job.id

  # For the python scripts it does not give us the result
  res = reduceResultsDataTable(jids[!is.na(jids)], function(x) {
    out = x$archive[, c("budget", "performance")]
    if (!is.null(out)) {
      out$budget_cum = cumsum(out$budget)
      out = out[budget_cum <= budget_max, ]
    }
    return(out)
  })

  if (algo %in% c("smac_full_budget", "hpbster_bohb", "hpbster_hb")) {
    updates = parallel::mclapply(res$job.id, function(jid) reduceJIDpythonSimple(jid, algo, prob, filedir, budget_max),
      mc.preschedule = TRUE, mc.cores = 28)

    res$result = updates
  }

  return(res)
}

reduceJIDpythonSimple = function(jid, algo, prob, filedir, budget_max = NULL) {

    print(jid)

    df = NULL
    path = file.path(filedir, "external", jid)

    # Manually read in the logged files
    if (algo %in% c("smac", "smac_full_budget", "smac_bohb", "smac_hb")) {

      library(reticulate)
      library(dplyr)
      pd = import("pandas")

      if (file.exists(file.path(path, "results.pkl"))) {

        df = as.data.table(pd$read_pickle(file.path(path, "results.pkl")))
        df$budget = round(df$budget) # Needs to be done as correction for correct representation
        df$budget_cum = cumsum(df$budget)
        if (!is.null(budget_max))
          df = df[budget_cum <= budget_max, ]
        return(df)

      } else {
        warning(paste0("Results file does not exist for ", jid))
      }
    }

    if (algo %in% c("hpbster_hb", "hpbster_bohb")) {

      if (file.exists(file.path(path, "results.json"))) {

        library(dplyr)

        df = readLines(file.path(path, "results.json")) %>% lapply(function(x) {
#          tryCatch({

            input = rjson::fromJSON(sub("Infinity", "2147483647", x))
            out = cbind(matrix(input[[1]], nrow = 1), input[[2]], input[[4]]$loss, input[[3]]$submitted)
            out
#          }, error = function(cond) return(NULL))})
        })
        df = do.call(rbind, df)
        df = as.data.table(df)
        colnames(df) = c("cid1", "cid2", "cid3", "budget", "performance", "submitted")
        rownames(df) = NULL

        df = df[order(df$submitted, df$cid1, df$cid3), ]
        if (prob != "rbv2_super") {
          df$budget = round(df$budget) # Correction to match the actual budget
        }

        df$budget_cum = cumsum(df$budget)
        if (!is.null(budget_max))
          df = df[budget_cum <= budget_max, ]

    }
  }
  return(df)
}


getResultsTableParallel = function(tab, algo, prob, filedir, budget_max, objective_multiplier = 1) {

  print(paste("Reducing: ", algo))

  tored = tab[problem == prob & algorithm == algo & task == 3945, ]
  tored = ijoin(tored, findDone())

  # For the python scripts it does not give us the result
  res = reduceResultsDataTable(tored$job.id, function(x) {
    out = x$archive[, c("budget", "performance")]

    if (!is.null(out)) {
      if (algo == "randomsearch_full_budget") {
        # We simulate the randomsearch was parallelized to 32 cores
        # As runs are independent, we just put them into equally sized chunks
        # Vector 1 1 ... 1 2 .... 2 indicating the "core"
        out$core = rep(1:32, each = nrow(out) / 32)
        out = out[, iteration := 1:.N, by = c("core")]
        # Get the best outcome across all cores;
        out = out[, .(performance = min(performance)), by = c("iteration", "budget")]
        # At every iteration, we spend a total budget of 32 * max_budget resources
        out$budget = out$budget * 32
      }

      if (algo == "mlr3hyperband") {
        # We simulate that every parallel worker runs a separate mlr3hyperband session
        # We have to assign "complete" hyperband runs to individual cores, we cannot random chunk the sessions
        # Thus: Identify the individual hyperband runs by looking at the differences in budget
        # When a new hyperband run is started, we observe a difference from max-budget to min-budget;
        diffs = c(out$budget[2:nrow(out)] - out$budget[seq_len(nrow(out) - 1)])

        bracket_diff = switch(prob,
          "lcbench" = 2 - 52,
          "rbv2_super" = 3^(-3) - 1,
          "nb301" = 1 - 98
        )

        # Length of a single hyperband run (where to we observe this "jump" for the first time)
        hblen = which(diffs == bracket_diff)[1]
        nhbruns = length(which(diffs == -50)) + 1 # + 1 bc. we have to count the first one as well
        # We assign a hyperband run id
        out$hb_run_id = rep(seq(1, nhbruns), each = hblen)

        hb_runs_per_core = floor(max(out$hb_run_id) / 32L)

        # We assign complete hyperband runs to individual cores
        hb_id_chunk = data.table(hb_run_id = seq(1, max(out$hb_run_id)))[1:(32L * hb_runs_per_core), ]
        hb_id_chunk$core = rep(1:32L, each = hb_runs_per_core)

        out = batchtools::ijoin(out, hb_id_chunk, by = c("hb_run_id"))
        out = out[, iteration := 1:.N, by = c("core")]

        out = out[, .(budget = sum(budget), performance = min(performance)), by = c("iteration")]
      }
    }
    return(out)
  })

  # Read in result for all python runs manually
  if (algo %in% c("smac_full_budget", "hpbster_bohb")) {
    updates = lapply(res$job.id, function(jid) {

      # read in the file via our script
      out = reduceJIDpythonSimple(jid, algo, filedir)

      # Now do some weird computations to have a "parallelized" version of BOHB
      # We simulate that batches are parallelized;
      # However, if batchsize is lower than the number of parallel cores, the resources are "wasted"

      if (algo == "hpbster_bohb") {
        # Goal: Identify the batches
        # FIXME: Problem: via differences, we won't detect the difference between last and second last batch
        diffs = c(0, out$budget[2:nrow(out)] - out$budget[seq_len(nrow(out) - 1)])
        bracket_diff = switch(prob,
          "lcbench" = 2 - 52,
          "rbv2_super" = 3^(-3) - 1,
          "nb301" = 1 - 98
        )
        hblen = which(diffs == bracket_diff)[1]
        nhbruns = which(diffs == bracket_diff)
        out$bracket_change = FALSE
        out[1:length(diffs), ]$bracket_change = diffs != 0
        out$stage_id = cumsum(out$bracket_change)[1:nrow(out)]

        # When we ignore that some cores are idel, this is the budget
        out$budget_spent_ignore_cores = out$budget

        out[, n_configs_stage := .N, by = c("stage_id")]
        # We scale the budget, such that the budget for every "used" core is a little bit higher, to also represent the "unused" cores
        out[, budget := 32 / n_configs_stage * budget_spent_ignore_cores, by = c("stage_id")]

        # Compute the best value across all core
        # Sum up the (scaled) budget that accounts for the idle cores
        out = out[, .(budget = sum(budget), performance = min(performance)), by = c("stage_id")]
      }

      if (algo == "smac_full_budget") {
        out = cbind(job.id = jid, out)
      }

      # For smac we have to average outside this loop bc. we span 32 independent runs, which are separate jobs

      return(out)
    })

    if (algo == "smac_full_budget") {
      out = do.call(rbind, updates)

      # Assign the 32 * 30 different jobs to different nodes
      # core_table = data.table(tored[, c("job.id", "task")])
      # core_table = lapply(unique(core_table$task), function(tt) {
      #   ct = core_table[task == tt, ]
      #   ct$replication = rep(1:30, each = 32)
      #   return(ct)
      # })
      # core_table = do.call(rbind, core_table)

      # out = ijoin(core_table, out)
      # out = out[, iteration := 1:.N, by = c("job.id")]

      # # Average out the cores
      # out = out[, .(performance = min(performance)), by = c("replication", "iteration", "budget", "task")]

      # # Somre workaround to put it again into a data.table with 30 entries per task
      # core_table = core_table[, .SD[1:30], by = c("replication", "task")]

      # out = ijoin(core_table, out, by = c("replication", "task"), all.y = TRUE)
    }

    res$result = updates


  }

  return(res)
}


readAndConcatenateFiles = function(filepaths) {
    reslist = lapply(filepaths, function(file) {
        x = readRDS(file)
        out = lapply(1:nrow(x), function(i) {
            cbind(job.id = x$job.id[i], x$result[[i]])
        })
        out = do.call(rbind, out)
        batchtools::ijoin(x[, c("job.id", "problem", "task", "objectives", "algorithm")], out[, c("job.id", "budget", "performance")], by = "job.id")
    })
    res = do.call(rbind, reslist)
    return(res)
}


# Compute normalized regret
computeNormalizedRegret = function(df, objective_multiplier = 1) {
    # for lcbench, set it to -1
    df$performance = df$performance * objective_multiplier

    # Overall best result achieved by randomsearch per task
    if (df$problem[1] == "branin") {
        df$y_min = 0.3978874
        df$y_max = 485.3732
    } else {
        # Compute the overall minimum and maximum per problem
        minmax = df[algorithm == "randomsearch_full_budget", ][, .(y_min = min(performance), y_max = max(performance)), by = c("task")]
        df = merge(df, minmax, all.x = TRUE, by = c("task"))
    }

    (df$performance - df$y_min) / (df$y_max - df$y_min)
}

