focussearch = function(data, job, instance, full_budget, log_scale = TRUE) {

  ins = instance$ins$clone()

  # get the budget parameter
  bid = ins$search_space$ids(tags = c("budget"))
  cids = ins$objective$codomain$ids()

  if (full_budget) {
    ts = list(as.integer(ins$search_space$params[[bid]]$upper))
    names(ts) = bid
    ins$search_space$values = c(ins$search_space$values, ts)
  } else {
    if (log_scale) {
      domain = ins$objective$domain
      param_ids = domain$ids()
      params_to_keep = param_ids[- which(param_ids == bid)]
      search_space = ParamSet$new(domain$params[params_to_keep])

      budget_lower = domain$params[[bid]]$lower
      budget_upper = domain$params[[bid]]$upper

      search_space = ParamSet$new(domain$params[params_to_keep])
      search_space$add(ParamDbl$new(id = bid, lower = log(budget_lower), upper = log(budget_upper), tags = "budget"))
      domain_tafo = domain$trafo
      search_space$trafo = function(x, param_set) {
        if (!is.null(domain_tafo)) x = domain_tafo(x, param_set)
        x[bid] = if (domain$params[[bid]]$class == "ParamInt") as.integer(exp(x[[bid]])) else exp(x[[bid]])
        x
      }
      search_space$deps = domain$deps

      ins = OptimInstanceSingleCrit$new(objective = ins$objective, search_space = search_space, terminator = trm("budget", budget = ins$terminator$param_set$values$budget, aggregate = function(x) sum(exp(as.numeric(x)))))
    }
  }

  budget = ins$terminator$param_set$values$budget * PARALLELIZATION
  ins$terminator$param_set$values$budget = budget

  # some heuristics in the case of using full budget
  n_restarts = 3L
  maxit = 20L
  n_points = ceiling(budget / (ins$search_space$params[[bid]]$upper * n_restarts * maxit))

  start_t = Sys.time()
  opt("focus_search", n_points = n_points, maxit = maxit)$optimize(ins)
  end_t = Sys.time()

  # To make it uniform in the end we rename the budget
  archive = ins$archive$data
  names(archive)[which(names(archive) == bid)] = "budget"
  names(archive)[which(names(archive) %in% cids)] = ifelse(length(cids) == 1, "performance", paste0("loss_", seq_len(length(length(cids)))))

  return(list(archive = archive, runtime = end_t - start_t))
}
