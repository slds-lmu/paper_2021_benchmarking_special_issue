library(data.table)
library(batchtools)
library(paradox)
library(mlr3misc)


# inputs
searchspace = ps(
  mtry = p_int(1, 10),
  foo = p_fct(levels = letters[1:3]),
  replace = p_lgl()
)
lambda = list(mtry = 3, replace = FALSE, foo = "a")
algo.name = "foo"


# source julias stuff to get problem and algo defs
# ...

ngrid = 5L # How many points on a grid for real values?
reg = makeExperimentRegistry(file.dir = NA)

for (i in seq_along(lambda)) {
  id = names(lambda)[i]
  value = lambda[[i]]
  param = searchspace$params[[id]]
  algo.design = as.data.table(lambda)

  lambdas = switch(param$class,
    ParamLgl = ,
    ParamFct = {
      lapply(param$levels, function(val) {
        insert_named(lambda, set_names(list(val), id))
      })
    },

    ParamInt = {
      lapply(unique(round(seq(from = param$lower, to = param$upper, length.out = ngrid))), function(val) {
        insert_named(lambda, set_names(list(as.integer(val)), id))
      })
    },

    ParamReal = {
      lapply(seq(from = param$lower, to = param$upper, length.out = ngrid), function(val) {
        insert_named(lambda, set_names(list(val), id))
      })
    }
  )

  ids = addExperiments(
    prob.designs = NULL,  # FIXME
    algo.designs = set_names(list(rbindlist(lambdas, use.names = TRUE)), algo.name),
    repls = 1L,
    reg = reg
  )

  setJobNames(ids, names = sprintf("%s_%03i", id, seq_along(lambdas)))
}
