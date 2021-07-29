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


computeDatasetForAnalysis = function(dirs, quantiles, parallel = FALSE) {

  if (parallel) {
    library(future.apply)
    plan(multisession)
    fun = future_lapply
  } else {
    fun = lapply
  }

  out = fun(dirs, function(d) {
    print(d)
    df = readRDS(d)

    results = df$result
    jids = df$job.id
    out = lapply(1:length(results), function(i) {
      outd = cbind(job.id = jids[i], algorithm = df[i, ]$algorithm, task = df[i, ]$task, problem = df[i, ]$problem, results[[i]][, c("budget", "performance")])
      outd = as.data.table(outd)
      outd = outd[, budget_cum := cumsum(budget), by = c("job.id")]
      outd = outd[budget_cum <= 30 * 9 * 52, ]
      outd = outd[, perfmin := cummin(performance), by = c("job.id")]
    })
    out = do.call(rbind, out)

    df = NULL

    # Budget in multiples of the maximum budget and on a logarithmic scale 
    out$budget_cum_log = log(out$budget_cum / max(out$budget), 10)
    # df_sub = lapply(quantiles, function(q) {
    #   cbind(dh[budget_cum_log <= q, .SD[which.min(performance)], by = c("job.id")], q = q)
    # })
    # df_sub = do.call(rbind, df_sub)

    # df_sub2 = lapply(quantiles, function(q) {
    #   dhs = dh[budget == max(dh$budget), ]
    #   cbind(dhs[budget_cum_log <= q, .SD[which.min(performance)], by = c("job.id")], q = q)
    # })
    # df_sub2 = do.call(rbind, df_sub2)
    # names(df_sub2)[which(names(df_sub2) == "performance")] = "perf_min_on_full_budget"

    # df_to_add = merge(df_sub, df_sub2[, c("q", "perf_min_on_full_budget", "job.id")], by = c("q", "job.id"), all.x = TRUE)

    # Add metadata 
    # dm = df
    # dm$result = NULL
    # dm = merge(dm, df_to_add, by = c("job.id"), all.y = TRUE)

    return(out)
  })

  out = do.call(rbind, out)
  
  # if (any(is.na(out2$problem)))
  #   out2 = out2[- which(is.na(out2$problem)), ]

  # Comparison with randomsearch 

  # Overall best result achieved by randomsearch per task 
  if (out$problem[1] == "branin") {
    out$y_min = 0.3978874
    out$y_max = 485.3732
  } else {
    # Compute the overall minimum and maximum per problem 
    minmax = out[, .(y_min = min(performance), y_max = max(performance)), by = c("task")]
    out = merge(out, minmax, all.x = TRUE, by = c("task"))
  }

  out$normalized_regret = (out$perfmin - out$y_min) / (out$y_max - out$y_min)

  return(out)
}




plotAggregatedLearningCurves = function(df, var = "perf_mean", x = "budget", y_name = NULL, se = FALSE) {

  if (is.null(y_name)) {
    y_name = var
  }

  p = ggplot(data = df, aes_string(x = x, y = var, colour = "algorithm", fill = "algorithm")) 
  if (se)
    p = p + geom_ribbon(aes(x = q, ymin = lower, ymax = upper, fill = algorithm), alpha = 0.25, colour = NA)
  p = p + geom_line()
  # p = p + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, colour = NA)
  p = p + theme_bw()
  # p = p + scale_x_continuous(breaks = seq(0, 4, by = 1),
  #         labels= 10^seq(0, 4))
  p = p + ylab(y_name)
  p
}

