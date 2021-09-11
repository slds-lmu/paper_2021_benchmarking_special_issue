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
from smac.facade.smac_hpo_facade import SMAC4HPO
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
    def __init__(self, problem, task, budget_param, objective, objective_multiplier, minbudget, maxbudget, full_budget, budget_on_log, total_budget):
        print(smac.__version__)
        base = importr("base")
        self.problem = problem 
        self.task = task
        self.budget_param = budget_param
        self.objective = objective
        self.multiplier = objective_multiplier
        self.minbudget = minbudget
        self.maxbudget = maxbudget
        self.full_budget = full_budget
        self.total_budget = total_budget
        self.budget_on_log = budget_on_log
        self.mfsurrogates = importr("mfsurrogates")
        if problem in ["lcbench", "rbv2_super", "nb301"]:
            opts = onnxruntime.SessionOptions()
            opts.inter_op_num_threads = 1
            opts.intra_op_num_threads = 1
            self.session = onnxruntime.InferenceSession("experiments/problems/" + problem + "/model.onnx", sess_options = opts)
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
        if not self.full_budget: # Tune over the budget parameter instead of setting it to the full budget
            if self.problem in ["lcbench", "nb301"]:
                budget = UniformIntegerHyperparameter(self.budget_param, self.minbudget, self.maxbudget, default_value=self.maxbudget, log = self.budget_on_log)
            if self.problem in ["rbv2_super", "branin"]:
                budget = UniformFloatHyperparameter(self.budget_param, self.minbudget, self.maxbudget, default_value = self.maxbudget, log = self.budget_on_log)
            cs.add_hyperparameters([budget])
        return(cs)
    def compute(self, config):
        config = config.get_dictionary()
        if self.full_budget: # Set the budget parameter to the full budget 
            config.update({self.budget_param: self.maxbudget}) 
        if self.problem in ["lcbench", "rbv2_super", "nb301"]:
            if self.problem in ['lcbench']: 
                config.update({"OpenML_task_id": self.task}) 
            if self.problem in ['rbv2_super']:
                config.update({"task_id": self.task})
                config.update({"repl": 10})  # Note: we have to use a fixed repl; 10 is the default
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
            out = (config["x2"] - ((5.1 / (4 * math.pi**2)) - 0.1 * (1 - config["fidelity"])) * config["x1"]**2 + (5 / math.pi) * config["x1"] - 6)**2 + 10 * (1 - (1 / (8 * math.pi))) * math.cos(config["x1"]) + 10
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
    # args = parser.parse_args(['--problem', 'lcbench', '--tempdir', 'reg_temp/external/', '--task', '3945', '--budget_param', 'epoch', '--minbudget', '1', '--maxbudget', '52', '--full_budget', 'TRUE', '--total_budget', '1000', '--objective', 'val_cross_entropy', '--objective_multiplier', '1', '--seed', '1', "--budget_on_log", "TRUE"])
    # args = parser.parse_args(['--problem', 'rbv2_super', '--tempdir', 'reg_temp/external/', '--task', '1040', '--budget_param', 'trainsize', '--minbudget', '0.05', '--maxbudget', '1', '--full_budget', 'TRUE', '--total_budget', '10', '--objective', 'logloss', '--objective_multiplier', '1', '--seed', '1', "--budget_on_log", "TRUE"])
    parser = argparse.ArgumentParser(description = "Parsing Arguments")
    parser.add_argument("--problem", type=str, required=True)
    parser.add_argument("--tempdir", type=str, required=True)
    parser.add_argument("--task", type=str, required=True)
    parser.add_argument("--budget_param", type=str, required = True)
    parser.add_argument("--minbudget", type=float, required=True)
    parser.add_argument("--maxbudget", type=float, required=True) 
    parser.add_argument("--full_budget", type=str, required=True)
    parser.add_argument("--budget_on_log", type=str, required=True)
    parser.add_argument("--total_budget", type=int, required=True) 
    parser.add_argument("--objective", type=str, required=True)
    parser.add_argument("--objective_multiplier", type=int, required=True)
    parser.add_argument("--seed", type=int, required=True)

    # parser.add_argument("--multi.point", type=int, required=True)
    args = parser.parse_args(args)

    if args.full_budget == 'TRUE':
        args.full_budget = True
    else: 
        args.full_budget = False

    if args.budget_on_log == 'TRUE':
        args.budget_on_log = True
    else: 
        args.budget_on_log = False


    logging.basicConfig(level=logging.INFO) 

    w = worker(problem=args.problem, task=args.task, budget_param=args.budget_param, objective=args.objective, objective_multiplier=args.objective_multiplier, minbudget = args.minbudget, maxbudget=args.maxbudget, full_budget=args.full_budget, budget_on_log=args.budget_on_log, total_budget=args.total_budget)

    cs = w.get_configspace()

    fun = w.compute

    # Number of function evaluations (need to restart in case budget parameter is not set to a fixed value)
    total_budget_in_evals = math.ceil(args.total_budget / args.maxbudget)

    print(args.total_budget)
    print(total_budget_in_evals)

    scen = {"run_obj": "quality",  # we optimize quality (alternatively runtime)
                         "runcount-limit": total_budget_in_evals,  
                         "cs": cs,  # configuration space
                         "deterministic": "true", 
                         "memory_limit": 2048, 
                         "wallclock-limit": 45 * 60, # Each run should take at max. half an hour (usually much faster!)
                         "output_dir": args.tempdir
                         }

    scenario = Scenario(scen)

    np.random.seed(args.seed)

    print("Optimizing!")
    smac = SMAC4HPO(scenario = scenario,
                   rng = np.random.RandomState(args.seed), 
                   tae_runner = fun,
                   run_id = 1
                   )

    smac.optimize()

    out_dir = scenario.output_dir_for_this_run

    history = smac.get_runhistory()
    budget = [el[args.budget_param] for el in history.get_all_configs()]
    values = [history.get_cost(conf) * args.objective_multiplier for conf in history.get_all_configs()]

    # Iterate in case we are not 
    if args.full_budget:
        budget = [args.maxbudget for el in history.get_all_configs()]
    else:
        total_budget_spent = sum(budget)
        i = 2
        while total_budget_spent < args.total_budget: # Compute the total budget spent, and if not increase budget by a heuristic
            print('Budget not fully spent: ' + str(total_budget_spent) + ' < ' + str(args.total_budget))
            evals_per_budget = len(budget) / total_budget_spent
            new_budget_in_evals = math.ceil((args.total_budget - total_budget_spent) * evals_per_budget)
            print("Total budget in evals: " + str(new_budget_in_evals))
            new_data = create_new_scenario(scen, out_dir, total_budget_in_evals + new_budget_in_evals)         # specify a new scenario
            smac = SMAC4HPO(scenario=new_data['scenario'],         # Now we can initialize SMAC with the recovered objects 
                        runhistory=new_data['runhistory'],
                        stats=new_data['stats'],
                        restore_incumbent=new_data['incumbent'],
                        rng = np.random.RandomState(args.seed), 
                        run_id=i)
            smac.optimize()
            history = smac.get_runhistory()
            budget = [el[args.budget_param] for el in history.get_all_configs()]
            values = [history.get_cost(conf) for conf in history.get_all_configs()]
            total_budget_spent = sum(budget)
            i = i + 1

    smac.runhistory.save_json(out_dir + '/runhistory.json')        

    # values = values * args.objective_multiplier
    df = pd.DataFrame({"budget": budget, "performance": values})

    with open(os.path.join(args.tempdir, 'results.pkl'), 'wb') as fh:
        pickle.dump(df, fh)  


if __name__ == "__main__":
    main(sys.argv[1:])


# cs = ConfigurationSpace()
# hyperparameters = [UniformFloatHyperparameter('x%d' % (i + 1), 0, 1) for i in range(21)]
# cs.add_hyperparameters(hyperparameters)

# sobol_kwargs = dict(
#     rng=np.random.RandomState(1),
#     traj_logger=unittest.mock.Mock(),
#     ta_run_limit=1000,
#     configs=None,
#     n_configs_x_params=None,
#     max_config_fracs=0.25,
#     init_budget=1,
# )
# SobolDesign(cs=cs, **sobol_kwargs).select_configurations()

# with open(config_space_path, 'r') as f:
#     json_string = f.read()
#     cs = json.read(json_string)
