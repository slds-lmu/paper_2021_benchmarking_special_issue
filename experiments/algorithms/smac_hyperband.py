import argparse
import numpy as np
import random
import logging
import pandas as pd

from rpy2.robjects.packages import importr
import rpy2.rlike.container as rlc
import rpy2.robjects as ro
from rpy2.robjects import pandas2ri

from ConfigSpace.read_and_write import json
import onnxruntime

import smac
from smac.configspace import ConfigurationSpace
from smac.facade.smac_bohb_facade import BOHB4HPO
from smac.facade.hyperband_facade import HB4AC
# Import SMAC-utilities
from smac.scenario.scenario import Scenario
from ConfigSpace.hyperparameters import UniformFloatHyperparameter, UniformIntegerHyperparameter

from smac.runhistory.runhistory import RunHistory
from smac.scenario.scenario import Scenario
from smac.stats.stats import Stats
from smac.utils.io.traj_logging import TrajLogger

import math
import os
import pickle
import sys


class worker():
    def __init__(self, problem, task, budget_param, objective, objective_multiplier, minbudget, maxbudget, eta, total_budget):
        base = importr("base")
        self.problem = problem 
        self.task = task
        self.budget_param = budget_param
        self.objective = objective
        self.multiplier = objective_multiplier
        self.minbudget = minbudget
        self.maxbudget = maxbudget
        self.eta = eta
        self.total_budget = total_budget
        self.mfsurrogates = importr("mfsurrogates")
        if problem in ["lcbench", "rbv2_super", "nb301"]:
            self.session = onnxruntime.InferenceSession("experiments/problems/" + problem + "/model.onnx")
            self.param_set = base.readRDS("experiments/problems/" + problem + "/param_set.rds")
            self.codomain = base.readRDS("experiments/problems/" + problem + "/codomain.rds")  
            self.data_order = base.readRDS("experiments/problems/" + problem + "/data_order.rds")        
            self.trafo_dict = base.readRDS("experiments/problems/" + problem + "/dicts.rds")
        if problem == "lcbench":
            self.target_names = ["val_accuracy", "val_cross_entropy", "val_balanced_accuracy", "test_cross_entropy", "test_balanced_accuracy", "time"]
            self.config_space_path = '../paper_2021_multi_fidelity_surrogates/src/configspaces/configspace_lcbench_drop_OpenML_task_id_epoch.json'
        if problem == "nb301":
            self.target_names = ["val_accuracy", "runtime"]
            self.config_space_path = '../paper_2021_multi_fidelity_surrogates/src/configspaces/configspace_nb301_drop_epoch.json'
        if problem == "rbv2_super":
            self.target_names = ["mmce", "f1", "auc", "logloss", "timetrain", "timepredict"]
            self.config_space_path = '../paper_2021_multi_fidelity_surrogates/src/configspaces/configspace_rbv2_super_drop_trainsize_repl_task_id.json'
        if problem == "branin":
            self.target_names = ["y"]
            self.config_space_path = '../paper_2021_multi_fidelity_surrogates/src/configspaces/configspace_branin_drop_fidelity.json'
        pandas2ri.activate()
    def get_configspace(self):
        with open(self.config_space_path, 'r') as f:
            json_string = f.read()
            cs = json.read(json_string)
            return(cs)
    def compute(self, cfg, seed, budget, **kwargs):
        config = cfg.get_dictionary()
        if self.problem in ["lcbench", "rbv2_super", "nb301"]:
            if self.problem in ['lcbench']: 
                config.update({"OpenML_task_id": self.task}) 
                config.update({self.budget_param: int(budget)}) 
            if self.problem in ['rbv2_super']:
                config.update({"task_id": self.task})
                config.update({"repl": 10})  # Note: we have to use a fixed repl; 10 is the default
                config.update({self.budget_param: budget}) 
            xdt = pd.DataFrame.from_dict([config])
            xdt = pandas2ri.py2rpy(xdt)
            li_ = self.mfsurrogates.convert_for_onnx(xdt, data_order = self.data_order, param_set = self.param_set, trafo_dict = self.trafo_dict)       
            li = { key : li_.rx2(key) for key in li_.names }
            li["continuous"] = np.atleast_2d(li["continuous"]).astype("float32")
            res_ = self.session.run(None, li)[0]
            res_ = self.mfsurrogates.retrafo_predictions(res_, target_names = self.target_names, codomain = self.codomain, trafo_dict = self.trafo_dict)        
            res =  { key : res_[key] for key in res_.keys().to_list() } 
            out = self.multiplier * float(res[self.objective])
        else:
            out = (config["x2"] - ((5.1 / (4 * math.pi**2)) - 0.1 * (1 - budget)) * config["x1"]**2 + (5 / math.pi) * config["x1"] - 6)**2 + 10 * (1 - (1 / (8 * math.pi))) * math.cos(config["x1"]) + 10
        return(out)  # SMAC always minimizes! 

def create_new_scenario(old_scen_dict, out_dir, new_budget):
    new_scenario = Scenario(old_scen_dict, cmd_options={'runcount_limit': new_budget, 
                                     'output_dir': old_scen_dict})
    rh_path = os.path.join(out_dir, "runhistory.json") # We load the runhistory, ...
    runhistory = RunHistory()
    runhistory.load_json(rh_path, new_scenario.cs)     # ... stats, ...
    stats_path = os.path.join(out_dir, "stats.json")
    stats = Stats(new_scenario)
    stats.load(stats_path)
    traj_path = os.path.join(out_dir, "traj_aclib2.json") # ... and trajectory.
    trajectory = TrajLogger.read_traj_aclib_format(
        fn=traj_path, cs=new_scenario.cs)
    incumbent = trajectory[-1]["incumbent"]
    return({"scenario": new_scenario, "stats": stats, "trajectory": trajectory, "runhistory": runhistory, "incumbent": incumbent})

def main(args): 
    # args = parser.parse_args(['--problem', 'lcbench', '--tempdir', 'reg_temp/external/', '--task', '3945', '--budget_param', 'epoch', '--minbudget', '1', '--maxbudget', '52', '--eta', '3', '--total_budget', '1000', '--objective', 'val_cross_entropy', '--objective_multiplier', '1', '--seed', '1'])
    # args = parser.parse_args(['--problem', 'rbv2_super', '--tempdir', 'reg_temp/external/', '--task', '1040', '--budget_param', 'trainsize', '--minbudget', '0.05', '--maxbudget', '1', '--full_budget', 'TRUE', '--total_budget', '10', '--objective', 'logloss', '--objective_multiplier', '1', '--seed', '1', "--budget_on_log", "TRUE"])
    parser = argparse.ArgumentParser(description = "Parsing Arguments")
    parser.add_argument("--alg", type=str, required=True)
    parser.add_argument("--problem", type=str, required=True)
    parser.add_argument("--tempdir", type=str, required=True)
    parser.add_argument("--task", type=str, required=True)
    parser.add_argument("--eta", type=int, required=True) 
    parser.add_argument("--budget_param", type=str, required = True)
    parser.add_argument("--minbudget", type=float, required=True)
    parser.add_argument("--maxbudget", type=float, required=True) 
    parser.add_argument("--total_budget", type=int, required=True) 
    parser.add_argument("--objective", type=str, required=True)
    parser.add_argument("--objective_multiplier", type=int, required=True)
    parser.add_argument("--seed", type=int, required=True)

    # parser.add_argument("--multi.point", type=int, required=True)
    args = parser.parse_args(args)

    logging.basicConfig(level=logging.WARNING)
    w = worker(problem=args.problem, task=args.task, budget_param=args.budget_param, objective=args.objective, objective_multiplier=args.objective_multiplier, minbudget = args.minbudget, maxbudget=args.maxbudget, eta = args.eta, total_budget=args.total_budget)

    cs = w.get_configspace()
    
    fun = w.compute

    scen = {"run_obj": "quality",  # we optimize quality (alternatively runtime)
                         "runcount-limit": args.total_budget,  
                         "cs": cs,  # configuration space
                         "deterministic": "true", 
                         # "memory_limit": 2048, 
                         # "wallclock-limit": 2600 * 48,
                         "output_dir": args.tempdir
                         }

    scenario = Scenario(scen)

    # intensifier parameters
    intensifier_kwargs = {'initial_budget': args.minbudget, 'max_budget': args.maxbudget, 'eta': args.eta}
    # To optimize, we pass the function to the SMAC-object
    if args.alg == "bohb":
        smac = BOHB4HPO(scenario=scenario,
                     tae_runner=fun, rng = np.random.RandomState(args.seed), 
                     intensifier_kwargs=intensifier_kwargs)  # all arguments related to intensifier can be passed like this
    else: 
        smac = HB4AC(scenario=scenario,
             tae_runner=fun, rng = np.random.RandomState(args.seed),
             intensifier_kwargs=intensifier_kwargs)  # all arguments related to intensifier can be passed like this

    smac.optimize()

    history = smac.get_runhistory()
    values = pd.DataFrame.from_records([[k.budget, v.cost] for k, v in history.data.items()])
    values[1] = values[1] * args.objective_multiplier
    values = values.rename(columns = {0: 'budget', 1: 'performance'})

    with open(os.path.join(args.tempdir, 'results.pkl'), 'wb') as fh:
        pickle.dump(values, fh)  


if __name__ == "__main__":
    main(sys.argv[1:])

