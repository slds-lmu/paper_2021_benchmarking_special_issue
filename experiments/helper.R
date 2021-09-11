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

  tored = tored[, .SD[1:30], by = c("task")]

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
    updates = lapply(res$job.id, function(jid) reduceJIDpythonSimple(jid, algo, filedir, budget_max))

    res$result = updates  
  }

  return(res)
}

reduceJIDpythonSimple = function(jid, algo, filedir, budget_max = NULL) {

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
          tryCatch({
            input = rjson::fromJSON(x)
            out = cbind(matrix(input[[1]], nrow = 1), input[[2]], input[[4]]$loss, input[[3]]$submitted)
            out
          }, error = function(cond) return(NULL))})
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

  tored = tab[problem == prob & algorithm == algo, ]
  tored = ijoin(tored, findDone())

  # For the python scripts it does not give us the result 
  res = reduceResultsDataTable(tored$job.id, function(x) {
    out = x$archive[, c("budget", "performance")]   

    if (!is.null(out)) {
      if (algo == "randomsearch_full_budget") {
        out$core = rep(1:32, each = nrow(out) / 32)
        out = out[, iteration := 1:.N, by = c("core")]
        out = out[, .(budget = sum(budget), performance = min(performance)), by = c("iteration")]
      } 

      if (algo == "mlr3hyperband") {
        # ACHTUN HARD-GEDODED (50)
        diffs = c(out$budget[2:nrow(out)] - out$budget[seq_len(nrow(out) - 1)])
        hblen = which(diffs == -50)[1]
        nhbruns = length(which(diffs == -50))
        out$hb_run_id = rep(seq(1, nhbruns + 1), each = hblen)

        hb_runs_per_core = floor(max(out$hb_run_id) / 32L)

        # chunk runs
        hb_id_chunk = data.table(hb_run_id = seq(1, max(out$hb_run_id)))[1:(32L * hb_runs_per_core), ]
        hb_id_chunk$core = rep(1:32L, each = hb_runs_per_core)

        out = batchtools::ijoin(out, hb_id_chunk, by = c("hb_run_id"))
        out = out[, iteration := 1:.N, by = c("core")]

        out = out[, .(budget = sum(budget), performance = min(performance)), by = c("iteration")]
      }
    }
    return(out)
  })

  if (algo %in% c("smac_full_budget", "hpbster_bohb", "hpbster_hb")) {
    updates = lapply(res$job.id, function(jid) reduceJIDpythonSimple(jid, algo, filedir))

    res$result = updates  
  }

  # FOR SMAC, AGGREGATE OVER PARALLEL RUNS 
  if (algo == "smac") {


  } 

  # FOR BOHB


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
















































computeDatasetForAnalysis = function(dirs, type = "sequential", min_max = "min") {

  MAX_BUDGET_SEQUENTIAL = list(
      lcbench = 7 * 52 * 30,
      rbv2_super = 38 * 1 * 30,
      nb301 = 34 * 98 * 30
    )

  BUDGET_OF_HB_RUN = list(
    lcbench = compute_total_budget(52, 1, 3)
  )

  if (min_max == "max") {
    objective_multiplier = (-1)
  } else {
    objective_multiplier = 1    
  }

  out = lapply(dirs, function(d) {
    print(d)
    df = readRDS(d)

    if (type == "sequential") {
      df = df[, .SD[1:30], by = c("task")]
    }

    results = df$result
    jids = df$job.id

    prob = df$problem[1]
    algo = df$algorithm[1]

    # if (prob == "rbv2_super") {
    #   subset_idx = round(exp(seq(log(1), log(nrow(results[[1]])), length.out = 1000)))
    # }

    out = lapply(1:length(results), function(i) {

      print(i)

      if (!is.null(results[[i]])) {
        outd = cbind(job.id = jids[i], algorithm = df[i, ]$algorithm, task = df[i, ]$task, problem = df[i, ]$problem, results[[i]][, c("budget_boundary", "performance")])
        outd = as.data.table(outd)

        names(outd)[5] = "budget_cum"

        outd$performance = outd$performance * objective_multiplier

        if (type == "sequential") {
          # outd = outd[, budget_cum := cumsum(budget), by = c("job.id")]
          # outd = outd[budget_cum <= MAX_BUDGET_SEQUENTIAL[[prob]], ]
          # outd = outd[, perfmin := cummin(performance), by = c("job.id")]
          # if (prob == "rbv2_super") {
          #   outd = outd[subset_idx, ]
          #   outd = outd[, .SD[!is.na(outd$performance)], ]
          # }
        } else {

          ## TODO: smashy parallel analysis 

          if (algo == "smashy_config_lcbench") {
            outd = NULL
          }

          if (algo == "randomsearch_full_budget") {
            outd$core = rep(1:32, each = nrow(outd) / 32)
            outd = outd[, iteration := 1:.N, by = c("core")]
            outd = outd[, .(perfmin_across_cores = min(performance)), by = c("job.id", "algorithm", "task", "budget", "iteration")]
            outd$budget = outd$budget * 32
          }

          if (algo == "smac_full_budget") {
            outd$core = i %% 32L
            outd = outd[, iteration := 1:.N, by = c("core")]
          }

          if (algo == "mlr3hyperband") {
            diffs = c(outd$budget[2:nrow(outd)] - outd$budget[seq_len(nrow(outd) - 1)])
            hblen = which(diffs == -50)[1]
            nhbruns = length(which(diffs == -50))
            outd$hb_run_id = rep(seq(1, nhbruns + 1), each = hblen)

            hb_runs_per_core = floor(max(outd$hb_run_id) / 32L)

            # chunk runs
            hb_id_chunk = data.table(hb_run_id = seq(1, max(outd$hb_run_id)))[1:(32L * hb_runs_per_core), ]
            hb_id_chunk$core = rep(1:32L, each = hb_runs_per_core)

            outd = batchtools::ijoin(outd, hb_id_chunk, by = c("hb_run_id"))
            outd = outd[, iteration := 1:.N, by = c("core")]

            # Get the number of full hyperband runs 
            outd$cumsum = cumsum(outd$budget)
            total_budget_hb = BUDGET_OF_HB_RUN[[prob]] 
            outd$hb_run = outd$cumsum / total_budget_hb

            outd = outd[, .(perfmin_across_cores = min(performance), budget = sum(budget)), by = c("job.id", "algorithm", "task", "iteration")]

            outd$algorithm = "mlr3hyperband"
          }

          if (algo == "hpbster_bohb") {
            diffs = c(0, outd$budget[2:nrow(outd)] - outd$budget[seq_len(nrow(outd) - 1)])
            hblen = which(diffs == -50)[1]
            nhbruns = which(diffs == -50)
            outd$bracket_change = FALSE
            outd[1:length(diffs), ]$bracket_change = diffs != 0
            outd$stage_id = cumsum(outd$bracket_change)[1:nrow(outd)]

            outd$budget_spent_ignore_cores = outd$budget

            outd[, n_configs_stage := .N, by = c("stage_id")]
            outd[, budget := 32 / n_configs_stage * budget_spent_ignore_cores, by = c("stage_id")]

            outd = outd[, .(perfmin_across_cores = min(performance), budget = sum(budget)), by = c("job.id", "algorithm", "task", "stage_id")]
          }
        }
        return(outd)
      }
    })
    out = lapply(out, as.data.table)
    out = do.call(rbind, out)

    if (nrow(out) > 0) {
      out$problem = prob

      if (algo == "smac_full_budget" & type == "parallel") {
        # Compute the minimum across cores   
        out = out[, .(perfmin_across_cores = min(performance)), by = c("job.id", "problem", "algorithm", "task", "budget", "iteration")]
        out$budget = out$budget * 32

        out = out[, c("job.id", "problem", "algorithm", "task", "perfmin_across_cores", "budget")]
      }
    }

    return(out)
  })

  out = do.call(rbind, out)

  # Comparison with randomsearch 

  # Overall best result achieved by randomsearch per task 
  if (out$problem[1] == "branin") {
    out$y_min = 0.3978874
    out$y_max = 485.3732
  } else {
    # Compute the overall minimum and maximum per problem 
    if (type == "sequential") {
      minmax = out[algorithm == "randomsearch_full_budget", ][, .(y_min = min(performance), y_max = max(performance)), by = c("task")]
    }
    if (type == "parallel") {
      minmax = out[, .(y_min = min(perfmin_across_cores), y_max = max(perfmin_across_cores)), by = c("task")]      
    }
    out = merge(out, minmax, all.x = TRUE, by = c("task"))
  }

  if (type == "sequential") {
    out$normalized_regret = (out$performance - out$y_min) / (out$y_max - out$y_min)
  }
  if (type == "parallel") {
    out$normalized_regret = (out$perfmin_across_cores - out$y_min) / (out$y_max - out$y_min)
  }

  out$performance = out$performance * objective_multiplier
  out$perfmin = out$perfmin * objective_multiplier

  if (type == "parallel") {
    out$perfmin_across_cores = out$perfmin_across_cores * objective_multiplier
  }

  return(out)
}




