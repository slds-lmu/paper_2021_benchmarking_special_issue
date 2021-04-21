# - BOHB

# Taken from 

bohb = function(data, job, instance, lambda) {

	library(reticulate)

	cfg = instance$cfg
	cfg$setup()

	hpbandster = import("hpbandster")
	hpns = hpbandster$core$nameserver
	Worker = hpbandster$core$worker$Worker
	
	BOHB = hpbandster$optimizers$BOHB

	CS = import("ConfigSpace")

	np = import("numpy")

	# Create a config space for the tuning instance 

	# Create a function that takes a config as well as a budget
	# Must return a loss and a result

	parser = argparse$ArgumentParser(description='Example 1 - sequential and local execution.')
	parser$add_argument('--min_budget',   type=`float`, help='Minimum budget used during the optimization.',    default=9)
	parser$add_argument('--max_budget',   type=`float`, help='Maximum budget used during the optimization.',    default=243)
	parser.add_argument('--n_iterations', type=`int`,   help='Number of iterations performed by the optimizer', default=4)
	args=parser.parse_args()


	source_python("experiments/algorithms/call_bohb.py")


	bla = main()


	# Requires a Worker Class 

	MyWorker <- PyClass("MyWorker", list(
		  name = NULL,
		  `__init__` = function(self, ...) {
		    super()$`__init__`(...)

		    NULL
		  },
		  compute = function(self, config, budget, ...) {
		  	res = runif(1)
		    return(list(loss = res))
		  }, 
		  get_configspace = function(...) {
		  	config_space = CS$ConfigurationSpace()
			config_space = config_space$add_hyperparameter(CS$UniformFloatHyperparameter("x", lower = 0, upper = 1))

			return(config_space)
		  }
		), inherit = Worker
	)

	NS = hpns$NameServer(run_id = 'example1', host = '127.0.0.1')
	NS$start()

	w = MyWorker(run_id = 'example1', nameserver = '127.0.0.1')
	
	w$run(background = TRUE)

	bohb = BOHB(configspace = w$get_configspace(),
	            run_id = 'example1', nameserver='127.0.0.1',
	            min_budget = 3, max_budget=27 # TODO
	           )


	bohb = BOHB(configspace = w.get_configspace(),
	              run_id = 'example1', nameserver='127.0.0.1',
	              min_budget=args.min_budget, max_budget=args.max_budget
	           )


res = bohb.run(n_iterations=args.n_iterations)

	res = bohb.run(n_iterations=args.n_iterations)	


	source("benchmarks/helper_evaluation.R")

	surrogate_data = readRDS(file.path(instance, "surrogate.rds"))
	surr_val = surrogate_data$result[[1]]$model_val_balanced_acc[[1]]
	surr_test = surrogate_data$result[[1]]$model_test_balanced_acc[[1]]

	# I also samples the variables on a log scale that were originally sampled on a log scale
	ps = makeParamSet(
			# do early stopping instead for the bigger datasets
		  	makeNumericParam("batch_size", lower = log(16, 2), upper = log(512, 2), trafo = function(x) round(2^x)), 
		  	makeNumericParam("max_dropout", lower = 0, upper = 1), 
		  	makeNumericParam("max_units", lower = log(64, 2), upper = log(1024, 2), trafo = function(x) round(2^x)), 
		  	makeIntegerParam("num_layers", lower = 1, upper = 5),
		  	makeNumericParam("learning_rate", lower = 0, upper = 0.01),
		  	makeNumericParam("momentum", lower = 0.1, upper = 1),
		  	makeNumericParam("weight_decay", lower = 0, upper = 0.1)
		)

	obj = makeSingleObjectiveFunction(name = "mlp.surr.tuning",
	  fn = function(x) {
	  	x = as.data.frame(x)
		y = predict(surr_val, newdata = x)$data$response

		attr(y, "extras") = list(test_performance = predict(surr_test, newdata = x)$data$response)
		return(1 - y / 100)
	  },
	  par.set = ps,
	  noisy = TRUE,
	  has.simple.signature = FALSE,
	  minimize = TRUE
	)

	ctrl = makeMBOControl(store.model.at = 1:200)
	ctrl = setMBOControlTermination(ctrl, max.evals = max.evals, time.budget = RUNTIME_MAX)
	ctrl = setMBOControlInfill(ctrl, makeMBOInfillCritCB(cb.lambda = lambda))

	set.seed(1234)
	des = generateDesign(n = 2 * length(ps$pars), par.set = ps, fun = lhs::randomLHS)

    start_t = Sys.time()
	res = mbo(obj, design = des, control = ctrl, show.info = TRUE)
    end_t = Sys.time()

    opdf = as.data.frame(res$opt.path)
    dob.best = opdf[res$best.ind, ]$dob

    models = res$models
    
    models = models[c(dob.best, length(models))]
    res$final.opt.state = NULL

    # Compute sampling bias
    mmd2 = compute_sampling_bias(res)

    return(list(
    	opt.path = opdf, 
    	models = models, 
    	objective = obj, 
    	mmd2 = mmd2, 
    	runtime = as.integer(end_t) - as.integer(start_t)
    	)
    )
}
