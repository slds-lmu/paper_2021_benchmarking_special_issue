# - BOHB

# Taken from 

bohb = function(data, job, instance) {

	print(job$external.dir)

	start_t = Sys.time()
    system(paste0("python3 experiments/algorithms/bohb.py --problem ", instance$name, " --tempdir ", job$external.dir, " --task ", instance$task))
	end_t = Sys.time()

    # get result
    # res = read.csv(paste0(job$external.dir, "/res.csv", sep = ","))

    return(list(runtime = end_t - start_t))
}
