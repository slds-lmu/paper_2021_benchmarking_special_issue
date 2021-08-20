import time
import numpy as np
import pandas as pd
import argparse
import sys
import os
from ConfigSpace.read_and_write import json
import onnxruntime
from rpy2.robjects.packages import importr
import rpy2.rlike.container as rlc
import rpy2.robjects as ro
from rpy2.robjects import pandas2ri
from hpbandster.core.worker import Worker
import hpbandster.core.nameserver as hpns
import hpbandster.core.result as hpres
from hpbandster.optimizers import BOHB as BOHB
from hpbandster.optimizers import HyperBand as HB
import logging
import pickle
import random
import math
import shutil

class nb301(Worker):
    def __init__(self, objective, objective_multiplier, *args, sleep_interval=0, **kwargs):
        super().__init__(*args, **kwargs)
        self.sleep_interval = sleep_interval
        base = importr("base")
        self.objective = objective
        self.multiplier = objective_multiplier
        self.mfsurrogates = importr("mfsurrogates")
        self.session = onnxruntime.InferenceSession("experiments/problems/nb301/model.onnx")
        self.param_set = base.readRDS("experiments/problems/nb301/param_set.rds")
        self.codomain = base.readRDS("experiments/problems/nb301/codomain.rds")  # FIXME: download manually from lrz or create from cfg
        self.data_order = base.readRDS("experiments/problems/nb301/data_order.rds")
        self.trafo_dict = base.readRDS("experiments/problems/nb301/dicts.rds")
        self.multiplier = objective_multiplier
        self.target_names = ["val_accuracy", "runtime"]
        pandas2ri.activate()
    def compute(self, config, budget, **kwargs):
        """
        Args:
            config: dictionary containing the sampled configurations by the optimizer
            budget: (float) amount of time/epochs/etc. the model can use to train
        Returns:
            dictionary with mandatory fields:
                'loss' (scalar)
                'info' (dict)
        """
        config.update({"epoch": int(budget)})  # FIXME: budget trafo to match epoch range and int
        xdt = pd.DataFrame.from_dict([config])
        xdt = pandas2ri.py2rpy(xdt)
        li_ = self.mfsurrogates.convert_for_onnx(xdt, data_order = self.data_order, param_set = self.param_set, trafo_dict = self.trafo_dict)  
        li = { key : li_.rx2(key) for key in li_.names }
        li["continuous"] = np.atleast_2d(li["continuous"]).astype("float32")
        res_ = self.session.run(None, li)[0]
        res_ = self.mfsurrogates.retrafo_predictions(res_, target_names = self.target_names, codomain = self.codomain, trafo_dict = self.trafo_dict)        
        res =  { key : res_[key] for key in res_.keys().to_list() } # convert to dict
        time.sleep(self.sleep_interval)
        return({
                    'loss': self.multiplier * float(res[self.objective]),  # this is the a mandatory field to run hyperband
                    'info': "empty"  # can be used for any user-defined information - also mandatory
                })
    @staticmethod
    def get_configspace():
        with open('../paper_2021_multi_fidelity_surrogates/src/configspaces/configspace_nb301_drop_epoch.json', 'r') as f:
            json_string = f.read()
            cs = json.read(json_string)
        return(cs)


class lcbench(Worker):
    def __init__(self, task, objective, objective_multiplier, *args, sleep_interval=0, **kwargs):
        super().__init__(*args, **kwargs)
        self.sleep_interval = sleep_interval
        base = importr("base")
        self.task = task
        self.objective = objective
        self.multiplier = objective_multiplier
        self.mfsurrogates = importr("mfsurrogates")
        self.session = onnxruntime.InferenceSession("experiments/problems/lcbench/model.onnx")
        self.param_set = base.readRDS("experiments/problems/lcbench/param_set.rds")
        self.codomain = base.readRDS("experiments/problems/lcbench/codomain.rds")  # FIXME: download manually from lrz or create from cfg
        self.data_order = base.readRDS("experiments/problems/lcbench/data_order.rds")        
        self.trafo_dict = base.readRDS("experiments/problems/lcbench/dicts.rds")
        self.target_names = ["val_accuracy", "val_cross_entropy", "val_balanced_accuracy", "test_cross_entropy", "test_balanced_accuracy", "time"]
        pandas2ri.activate()
    def compute(self, config, budget, **kwargs):
        """
        Args:
            config: dictionary containing the sampled configurations by the optimizer
            budget: (float) amount of time/epochs/etc. the model can use to train
        Returns:
            dictionary with mandatory fields:
                'loss' (scalar)
                'info' (dict)
        """
        config.update({"OpenML_task_id": self.task})
        config.update({"epoch": int(np.round(budget))}) # Do rounding: "int" causes strange behavior   # FIXME: budget trafo to match epoch range and int
        xdt = pd.DataFrame.from_dict([config])
        xdt = pandas2ri.py2rpy(xdt)
        li_ = self.mfsurrogates.convert_for_onnx(xdt, data_order = self.data_order, param_set = self.param_set, trafo_dict = self.trafo_dict)       
        li = { key : li_.rx2(key) for key in li_.names }
        li["continuous"] = np.atleast_2d(li["continuous"]).astype("float32")
        res_ = self.session.run(None, li)[0]
        res_ = self.mfsurrogates.retrafo_predictions(res_, target_names = self.target_names, codomain = self.codomain, trafo_dict = self.trafo_dict)        
        res =  { key : res_[key] for key in res_.keys().to_list() } #
        time.sleep(self.sleep_interval)
        return({
                    'loss': self.multiplier * float(res[self.objective]),  # this is the a mandatory field to run hyperband
                    'info': "empty"  # can be used for any user-defined information - also mandatory
                })
    @staticmethod
    def get_configspace():
        with open('../paper_2021_multi_fidelity_surrogates/src/configspaces/configspace_lcbench_drop_OpenML_task_id_epoch.json', 'r') as f:
            json_string = f.read()
            cs = json.read(json_string)
        return(cs)

class rbv2_super(Worker):
    def __init__(self, task, objective, objective_multiplier, *args, sleep_interval=0, **kwargs):
        super().__init__(*args, **kwargs)
        self.sleep_interval = sleep_interval
        base = importr("base")
        self.task = task
        self.objective = objective
        self.multiplier = objective_multiplier
        self.mfsurrogates = importr("mfsurrogates")
        self.session = onnxruntime.InferenceSession("experiments/problems/rbv2_super/model.onnx")
        self.param_set = base.readRDS("experiments/problems/rbv2_super/param_set.rds")
        self.codomain = base.readRDS("experiments/problems/rbv2_super/codomain.rds")  # FIXME: download manually from lrz or create from cfg
        self.data_order = base.readRDS("experiments/problems/rbv2_super/data_order.rds")        
        self.trafo_dict = base.readRDS("experiments/problems/rbv2_super/dicts.rds")
        self.target_names = ["mmce", "f1", "auc", "logloss", "timetrain", "timepredict"]
        pandas2ri.activate()
    def compute(self, config, budget, **kwargs):
        """
        Args:
            config: dictionary containing the sampled configurations by the optimizer
            budget: (float) amount of time/epochs/etc. the model can use to train
        Returns:
            dictionary with mandatory fields:
                'loss' (scalar)
                'info' (dict)
        """
        config.update({"task_id": self.task})
        config.update({"trainsize": budget})  # FIXME: budget trafo to match epoch range and int
        config.update({"repl": 10})  # Note: we have to use a fixed repl; 10 is the default
        xdt = pd.DataFrame.from_dict([config])
        xdt = pandas2ri.py2rpy(xdt)
        li_ = self.mfsurrogates.convert_for_onnx(xdt, data_order = self.data_order, param_set = self.param_set, trafo_dict = self.trafo_dict)       
        li = { key : li_.rx2(key) for key in li_.names }
        li["continuous"] = np.atleast_2d(li["continuous"]).astype("float32")
        res_ = self.session.run(None, li)[0]
        res_ = self.mfsurrogates.retrafo_predictions(res_, target_names = self.target_names, codomain = self.codomain, trafo_dict = self.trafo_dict)        
        res =  { key : res_[key] for key in res_.keys().to_list() } #
        time.sleep(self.sleep_interval)
        return({
                    'loss': self.multiplier * float(res[self.objective]),  # this is the a mandatory field to run hyperband
                    'info': "empty"  # can be used for any user-defined information - also mandatory
                })
    @staticmethod
    def get_configspace():
        with open('../paper_2021_multi_fidelity_surrogates/src/configspaces/configspace_rbv2_super_drop_trainsize_repl_task_id.json', 'r') as f:
            json_string = f.read()
            cs = json.read(json_string)
        return(cs)


class branin(Worker):
    def __init__(self, objective, objective_multiplier, *args, sleep_interval=0, **kwargs):
        super().__init__(*args, **kwargs)
        self.sleep_interval = sleep_interval
        base = importr("base")
        self.objective = objective
        self.multiplier = objective_multiplier
        self.target_names = ["y"]
        pandas2ri.activate()
    def compute(self, config, budget, **kwargs):
        """
        Args:
            config: dictionary containing the sampled configurations by the optimizer
            budget: (float) amount of time/epochs/etc. the model can use to train
        Returns:
            dictionary with mandatory fields:
                'loss' (scalar)
                'info' (dict)
        """
        y = (config["x2"] - ((5.1 / (4 * math.pi**2)) - 0.1 * (1 - budget)) * config["x1"]**2 + (5 / math.pi) * config["x1"] - 6)**2 + 10 * (1 - (1 / (8 * math.pi))) * math.cos(config["x1"]) + 10
        time.sleep(self.sleep_interval)
        return({
                    'loss': self.multiplier * float(y),  # this is the a mandatory field to run hyperband
                    'info': "empty"  # can be used for any user-defined information - also mandatory
                })
    @staticmethod
    def get_configspace():
        with open('../paper_2021_multi_fidelity_surrogates/src/configspaces/configspace_branin_drop_fidelity.json', 'r') as f:
            json_string = f.read()
            cs = json.read(json_string)
        return(cs)


def main(args): 
    # logging.basicConfig(level=logging.DEBUG)
    parser = argparse.ArgumentParser(description = "Do Something")
    parser.add_argument("--alg", type=str, required=True)
    parser.add_argument("--problem", type=str, required=True)
    parser.add_argument("--tempdir", type=str, required=True)
    parser.add_argument("--task", type=str, required=True)
    parser.add_argument("--minbudget", type=float, required=True)
    parser.add_argument("--maxbudget", type=float, required=True)
    parser.add_argument("--eta", type=int, required=True)
    parser.add_argument("--fullbudget", type=int, required=True)
    parser.add_argument("--objective", type=str, required=True)
    parser.add_argument("--objective_multiplier", type=int, required=True)
    args = parser.parse_args(args)
    # args.fullbudget = 20000
    # Compute the total number of SH iterations
    max_SH_iter = -int(np.log(args.minbudget/args.maxbudget)/np.log(args.eta)) + 1
    print(max_SH_iter)
    # args = parser.parse_args(['--problem', 'branin', '--tempdir', 'reg_temp/external/', '--task', 'NA', '--minbudget', '0.01', '--maxbudget', '1', '--eta', '3', '--fullbudget', '10', '--alg', 'hb', '--objective', 'y', '--objective_multiplier', '1'])
    # args = parser.parse_args(['--problem', 'nb301', '--tempdir', 'reg_temp/external/', '--task', 'NA', '--minbudget', '1', '--maxbudget', '52', '--eta', '3', '--fullbudget', '5000', '--alg', 'hb', '--objective', 'val_accuracy', '--objective_multiplier', '-1'])
    result_logger = hpres.json_result_logger(directory=args.tempdir, overwrite=True)

    print(args.fullbudget)

    res = None
    # Bei ports randomisieren 
    randport = random.randrange(49152, 65535 + 1)

    total_configs_evaluated_parallel = 0
    total_configs_evaluated_sequential = 0
    total_budget_hb_parallel = 0
    total_budget_hb_sequential = 0
    budgets = args.maxbudget * np.power(args.eta, -np.linspace(max_SH_iter-1, 0, max_SH_iter))
    # compute the total number of evaluations
    for s in range(max_SH_iter):
        n0 = int(np.floor((max_SH_iter)/(s+1)) * args.eta**s)
        ns_parallel_execution = [32 for i in range(s+1)]
        ns_sequential_execution = [max(int(n0*(args.eta**(-i))), 1) for i in range(s+1)]
        total_configs_evaluated_parallel = total_configs_evaluated_parallel + sum(ns_parallel_execution)
        total_configs_evaluated_sequential = total_configs_evaluated_sequential + sum(ns_sequential_execution)
        total_budget_per_iteration_parallel = [ns_parallel_execution * budgets[(-s-1):]] 
        total_budget_per_iteration_sequential = [ns_sequential_execution * budgets[(-s-1):]] 
        total_budget_hb_parallel = total_budget_hb_parallel + np.sum(total_budget_per_iteration_parallel)
        total_budget_hb_sequential = total_budget_hb_sequential + np.sum(total_budget_per_iteration_sequential)

    # Parallel: Buckets are not completely filled, number of configurations is uprounded to 32
    # if alg == "hb": # We execute the hb runs in parallel with 32x the budget
    #     # Spanning up 32 independent HB runs (overoptimistc budget estimation)
    #     iterations_needed = math.ceil(fullbudget / total_budget_hb_sequential) * (max_SH_iter + 1)
    # if alg == "bohb": # We only fill each bracket as far as we can 
    # Do the maximum iterations: (1) A sequential run with fullbudget / 32 (2) Parallel run with not completely filled nodes on fullbudget
    iterations_needed = max(math.ceil(args.fullbudget / 32 / total_budget_hb_sequential) * (max_SH_iter + 1), math.ceil(args.fullbudget / total_budget_hb_parallel) * (max_SH_iter + 1))
    print('Iterations needed: ' + str(iterations_needed))

    # LCBENCH: 
    # * total_budget_hb_parallel:            8751
    # * total_budget_hb_sequential:          780
    # * iterations if all 32 nodes are used: 2304 (576 full runs)
    # * iterations if 32 nodes are not fully used: 260 (52 full runs)
    # RBV2_SUPER: 
    # * total_budget_hb_parallel:            168
    # * total_budget_hb_sequential:          15
    # * iterations if all 32 nodes are used: 11200 (2240 full runs)
    # * iterations if 32 nodes are not fully used: 1000 (200 full runs)
    # --> We only run the version that is not computed on the full budget, as we still have mlr3hyperband (which is comparable and computed on the full budget!)

    NS = hpns.NameServer(run_id='example1', host='127.0.0.1', port=randport, working_directory=args.tempdir)
    NS.start()

    if args.problem == "nb301":
        w = nb301(sleep_interval=0, objective = args.objective, objective_multiplier = args.objective_multiplier, nameserver='127.0.0.1', nameserver_port = randport, run_id='example1')
    if args.problem == "lcbench":
        w = lcbench(task = args.task, objective = args.objective, objective_multiplier = args.objective_multiplier, sleep_interval=0, nameserver='127.0.0.1', nameserver_port = randport, run_id='example1')
    if args.problem == "rbv2_super":
        w = rbv2_super(task = args.task, objective = args.objective, objective_multiplier = args.objective_multiplier, sleep_interval=0, nameserver='127.0.0.1', nameserver_port = randport, run_id='example1')
    if args.problem == "branin":
        w = branin(objective = args.objective, objective_multiplier = args.objective_multiplier, sleep_interval=0, nameserver='127.0.0.1', nameserver_port = randport, run_id='example1')

    w.run(background=True)

    if args.alg == "hb":
        alg = HB(configspace=w.get_configspace(), run_id='example1', nameserver='127.0.0.1', nameserver_port = randport, min_budget=args.minbudget, max_budget=args.maxbudget, eta = args.eta, previous_result = res, result_logger=result_logger)
    if args.alg == "bohb":
        alg = BOHB(configspace=w.get_configspace(), run_id='example1', nameserver='127.0.0.1', nameserver_port = randport, min_budget=args.minbudget, max_budget=args.maxbudget, eta = args.eta, previous_result = res, result_logger=result_logger)
    res = alg.run(n_iterations = iterations_needed) # hand over number of brackets here
    alg.shutdown(shutdown_workers = False)

    NS.shutdown()    

if __name__ == "__main__":
    main(sys.argv[1:])



eta = 3
minbudget = 3**(-3)
maxbudget = 1

max_SH_iter = -int(np.log(minbudget/maxbudget)/np.log(eta)) + 1

total_configs_evaluated_parallel = 0
total_configs_evaluated_sequential = 0
total_budget_hb_parallel = 0
total_budget_hb_sequential = 0
budgets = maxbudget * np.power(eta, -np.linspace(max_SH_iter-1, 0, max_SH_iter))
# compute the total number of evaluations
for s in range(max_SH_iter):
    n0 = int(np.floor((max_SH_iter)/(s+1)) * eta**s)
    ns_parallel_execution = [32 for i in range(s+1)] # In parallel execution, we always spend all 32 resources
    print(ns_parallel_execution)
    ns_sequential_execution = [max(int(n0*(eta**(-i))), 1) for i in range(s+1)] # in sequential, we only waste what we actually need
    print(ns_sequential_execution)
    total_configs_evaluated_parallel = total_configs_evaluated_parallel + sum(ns_parallel_execution)
    total_configs_evaluated_sequential = total_configs_evaluated_sequential + sum(ns_sequential_execution)
    total_budget_per_iteration_parallel = [ns_parallel_execution * budgets[(-s-1):]] 
    print(total_budget_per_iteration_parallel)
    total_budget_per_iteration_sequential = [ns_sequential_execution * budgets[(-s-1):]] 
    total_budget_hb_parallel = total_budget_hb_parallel + np.sum(total_budget_per_iteration_parallel)
    total_budget_hb_sequential = total_budget_hb_sequential + np.sum(total_budget_per_iteration_sequential)

