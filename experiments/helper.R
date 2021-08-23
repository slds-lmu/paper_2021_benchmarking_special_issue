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


computeDatasetForAnalysis = function(dirs, type = "sequential") {

  MAX_BUDGET_SEQUENTIAL = list(
      lcbench = 7 * 52 * 30,
      rbv2_super = 38 * 1 * 30,
      nb301 = 34 * 98 * 30
    )

  BUDGET_OF_HB_RUN = list(
    lcbench = compute_total_budget(52, 1, 3)
  )

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

    out = lapply(1:length(results), function(i) {

      outd = cbind(job.id = jids[i], algorithm = df[i, ]$algorithm, task = df[i, ]$task, problem = df[i, ]$problem, results[[i]][, c("budget", "performance")])
      outd = as.data.table(outd)

      if (type == "sequential") {
        outd = outd[, budget_cum := cumsum(budget), by = c("job.id")]
        outd = outd[budget_cum <= MAX_BUDGET_SEQUENTIAL[[prob]], ]
        outd = outd[, perfmin := cummin(performance), by = c("job.id")]
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
    })
    out = setDT(do.call(rbind, out))

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
      minmax = out[, .(y_min = min(performance), y_max = max(performance)), by = c("task")]
    }
    if (type == "parallel") {
      minmax = out[, .(y_min = min(perfmin_across_cores), y_max = max(perfmin_across_cores)), by = c("task")]      
    }
    out = merge(out, minmax, all.x = TRUE, by = c("task"))
  }

  if (type == "sequential") {
    out$normalized_regret = (out$perfmin - out$y_min) / (out$y_max - out$y_min)
  }
  if (type == "parallel") {
    out$normalized_regret = (out$perfmin_across_cores - out$y_min) / (out$y_max - out$y_min)
  }

  return(out)
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

