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
        config.update({"epoch": int(budget)})  # FIXME: budget trafo to match epoch range and int
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
        with open('../paper_2021_multi_fidelity_surrogates/src/configspaces/configspace_lcbench_drop_OpenML_task_id_epoch.json'            , 'r') as f:
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
        config.update({"budget": float(budget)}) 
        xdt = pd.DataFrame.from_dict([budget])
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
        with open('../paper_2021_multi_fidelity_surrogates/src/configspaces/configspace_rbv2_super_drop_trainsize_repl_task_id.json'            , 'r') as f:
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
        with open('../paper_2021_multi_fidelity_surrogates/src/configspaces/configspace_branin_drop_fidelity.json'            , 'r') as f:
            json_string = f.read()
            cs = json.read(json_string)
        return(cs)



def compute_total_budget(res):
    df = res.get_pandas_dataframe()[0]
    return(sum(df['budget']))

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

    # args = parser.parse_args(['--problem', 'branin', '--tempdir', 'reg_temp/external/', '--task', 'NA', '--minbudget', '0.01', '--maxbudget', '1', '--eta', '3', '--fullbudget', '10', '--alg', 'hb', '--objective', 'y', '--objective_multiplier', '1'])
    # args = parser.parse_args(['--problem', 'nb301', '--tempdir', 'reg_temp/external/', '--task', 'NA', '--minbudget', '1', '--maxbudget', '52', '--eta', '3', '--fullbudget', '5000', '--alg', 'hb', '--objective', 'val_accuracy', '--objective_multiplier', '-1'])
    # result_logger = hpres.json_result_logger(directory=args.tempdir, overwrite=True)

    total_budget_spent = 0
    res = None
    # Bei ports randomisieren 
    randport = random.randrange(49152, 65535 + 1)

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
        alg = HB(configspace=w.get_configspace(), run_id='example1', nameserver='127.0.0.1', nameserver_port = randport, min_budget=args.minbudget, max_budget=args.maxbudget, eta = args.eta, previous_result = res)# , result_logger=result_logger)
    if args.alg == "bohb":
        alg = BOHB(configspace=w.get_configspace(), run_id='example1', nameserver='127.0.0.1', nameserver_port = randport, min_budget=args.minbudget, max_budget=args.maxbudget, eta = args.eta, previous_result = res)# , result_logger=result_logger)

    while total_budget_spent < args.fullbudget:
        res = alg.run(n_iterations=max_SH_iter) # hand over number of brackets here
        total_budget_spent = compute_total_budget(res)

    print(total_budget_spent)

    alg.shutdown(shutdown_workers = True)
    NS.shutdown()    

    with open(os.path.join(args.tempdir, 'results.pkl'), 'wb') as fh:
        pickle.dump(res, fh)  

if __name__ == "__main__":
    main(sys.argv[1:])


