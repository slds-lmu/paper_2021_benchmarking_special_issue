# - BOHB

# Taken from 

bohb = function(data, job, instance) {

    system(paste0("python3 experiments/test.py '", instance$name, "' '", instance$task, "' '", instance$ins$objective$codomain$ids(), "'"))

    # get result
    res = read.csv("experiments/test.csv")
}
