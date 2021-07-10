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

    if (data$id == "Branin") {
      ins$search_space$params[[budget_param]]$lower = 3^(-4)
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


compute_total_budget = function(bupper, blower, eta) {
  smax = floor(log(bupper / blower, eta))
  B = (smax + 1) * bupper
  brackets = seq(0, smax)

  out = lapply(brackets, function(s) {
    n = ceiling(B / bupper * eta^s / (s + 1))
    r = bupper * eta^(-s)
    out = lapply(seq(0, s), function(i) {
      ni = floor(n * eta^(-i))
      ri = r * eta^i
      ni * ri
    })
    sum(unlist(out))
  })
  sum(unlist(out))
}



computeDatasetForAnalysis = function(dirs, algorithm, quantiles) {

  out = lapply(dirs, function(d) {

    print(d)

    paths = paste0(d, "/", algorithms, ".rds")
    
    dfs = lapply(paths, function(p) {
        if (file.exists(p)) {
          df = readRDS(p)

          jids = df[, .(job.id = head(job.id, 3)), by = c("algorithm", "algorithm_type", "full_budget")]
          df = df[job.id %in% jids$job.id, ]

          ss = !unlist(lapply(df$result, is.null))
          df = df[ss, ]

          df_out = lapply(seq(1, nrow(df)), function(i) {
            dh = df[i, ]
            dh$result = NULL
            # dh$multi.point = NULL
            cbind(dh, df[i, ]$result[[1]])
          })
          do.call(rbind, df_out)      
        }
      })

      dfs = do.call(rbind, dfs)
      dfs = as.data.table(dfs)

      dfs[, budget_cum := cumsum(budget), by = c("job.id")]

      dfs[, nexps := length(unique(job.id)), by = c("problem", "task", "algorithm", "algorithm_type", "full_budget", "multi.point")]

      dfs[algorithm_type == "bohb", ]$algorithm = "hpbster_bohb"
      dfs[algorithm_type == "hb", ]$algorithm = "hpbster_hb"
      dfs[full_budget == TRUE, ]$algorithm = paste0(dfs[full_budget == TRUE, ]$algorithm, "_full_budget")

      dfs$algorithm_nexps = paste0(dfs$algorithm, " (", dfs$nexps, ")")
      dfs = dfs[, perf_min := cummin(performance), by = c("job.id")]

      bla = unique(dfs[, c("job.id", "algorithm_nexps")])
      bla = bla[, repl := 1:.N, by = c("algorithm_nexps")]

      if (!(any(bla$repl) < 3)) {
        df_sub = NULL
      } else {

        dfs = merge(dfs, bla, all.x = TRUE, by = c("job.id", "algorithm_nexps"))

        # Bracket location
        dfs$new_bracket = c(TRUE, dfs$budget[2:length(dfs$budget)] - dfs$budget[seq_len(length(dfs$budget) - 1)] == -50)
        dfs$new_bracket[!(dfs$algorithm %in% c("randomsearch", "mlrintermbo", "smac"))] = FALSE

        dfs$end_init_des = FALSE
        dfs = dfs[, iter := 1:.N, by = c("job.id")]
        dfs[algorithm == "smac" & iter == 10 * 8, ]$end_init_des = TRUE
        dfs[algorithm == "mlrintermbo" & iter == 4 * 8, ]$end_init_des = TRUE
        df = dfs
        df$budget_cum_log = log(df$budget_cum / 52, 10)
        dfin = df[, .SD[which(end_init_des)], by = c("job.id")]
        dfin$end_init_des_budget_cum_log = dfin$budget_cum_log
        df = merge(df, dfin[, c("job.id", "end_init_des_budget_cum_log") ], all.x = TRUE, by = c("job.id"))

        # Compare performance at fixed points in time 
        df_sub = lapply(quantiles, function(q) {
          cbind(df[budget_cum_log <= q, .SD[which.min(performance)], by = c("job.id", "end_init_des")], q = q)
        })
        df_sub = do.call(rbind, df_sub)

        # get the random-search ones
        df_comp = df_sub[algorithm == "randomsearch_full_budget", ]
        df_comp = df_comp[, c("q", "repl", "perf_min")]
        names(df_comp) = c("q", "repl", "randomsearch_perf")
        df_sub = merge(df_sub, df_comp, by = c("q", "repl"), all.x = TRUE)
        df_sub$perf_min_rel_rs = (df_sub$perf_min - df_sub$randomsearch_perf) / df_sub$randomsearch_perf

        df_sub[, rank := rank(perf_min), by = c("q", "task", "repl")]

        df_sub = df_sub[, .(perf_mean = mean(performance), perf_mean_rel_rs = mean(perf_min_rel_rs), mean_rank = mean(rank), q_upper = quantile(performance, 0.9), q_lower = quantile(performance, 0.1), end_init_des = mean(end_init_des_budget_cum_log)), by = c("q", "algorithm_nexps", "task")]

      }

      saveRDS(df_sub, file.path(d, "learning_curves.rds"))
  })
}

plotAggregatedLearningCurves = function(dir, init_des = FALSE, with_brackets = FALSE, var = "perf_mean") {

  dfq = readRDS(file.path(dir, "learning_curves.rds"))

  p = ggplot(data = dfq, aes_string(x = "q", y = var, colour = "algorithm_nexps", fill = "algorithm_nexps")) 
  p = p + geom_line()
  p = p + geom_ribbon(aes(ymin = q_lower, ymax = q_upper), alpha = 0.1, colour = NA)
  p = p + theme_bw()
  p = p + scale_x_continuous(breaks = seq(0, 4, by = 1),
          labels= 10^seq(0, 4))
  if (init_des) {
    p = p + geom_vline(aes(xintercept = end_init_des, colour = algorithm_nexps), lty = 2)
  }

  p = p + xlab("Budget spent (in multiples of full budget)")
  p = p + ylab("Mean validation cross entropy")
  p = p + ggtitle(paste0("LCBench, task_id = ", strsplit(dir, "/")[[1]][4]))
  p
}

