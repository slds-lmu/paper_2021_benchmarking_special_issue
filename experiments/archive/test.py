import os
import numpy
import time

import argparse

import ConfigSpace as CS
import sys
from hpbandster.core.worker import Worker
from hpbandster.optimizers import BOHB as BOHB
import hpbandster.core.nameserver as hpns
from rpy2.robjects.packages import importr
import rpy2.robjects as robjects
import rpy2.rlike.container as rlc
from ConfigSpace.read_and_write import json
import onnxruntime as rt

class MyWorker(Worker):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        sess = rt.InferenceSession("experiments/problems/nb301/model.onnx")
    def compute(self, config, budget, **kwargs):
        """
        Simple example for a compute function
        The loss is just a the config + some noise (that decreases with the budget)

        For dramatization, the function can sleep for a given interval to emphasizes
        the speed ups achievable with parallel workers.

        Args:
            config: dictionary containing the sampled configurations by the optimizer
            budget: (float) amount of time/epochs/etc. the model can use to train

        Returns:
            dictionary with mandatory fields:
                'loss' (scalar)
                'info' (dict)
        """
        tl = rlc.TaggedList(config.values(), tags=config.keys())
        res = numpy.array(self.fun(tl))[0][0]
        return({
                    'loss': float(res),  # this is the a mandatory field to run hyperband
                    'info': res  # can be used for any user-defined information - also mandatory
                })
    @staticmethod
    def get_configspace():
        config_space = CS.ConfigurationSpace()
        with open('experiments/problems/nb301/domain.json', 'r') as f:
            jason_string = f.read()
            config = json.read(jason_string)        
        return(config)


def main(args):
    parser = argparse.ArgumentParser(description="Do something.")
parser.add_argument("-x", "--xcenter", type=float, default= 2, required=False)
    problem = argv[0]
    task = argv[1]
    objectives = argv[2:]
    # NS = hpns.NameServer(run_id='example1', host='127.0.0.1', port=None)
    # NS.start()
    w = MyWorker(problem = problem, task = task, objectives = objectives, nameserver='127.0.0.1',run_id= problem + task)
    # w.run(background=True)
    # bohb = BOHB(configspace=w.get_configspace(),
    #             run_id='example1', nameserver='127.0.0.1',
    #             min_budget=0.01, max_budget=1)
    # res = bohb.run(n_iterations=1)
    # bohb.shutdown(shutdown_workers=True)
    # NS.shutdown()
    # df = res.get_pandas_dataframe()[0]
    # df.to_csv('experiments/test.csv')

if __name__ == '__main__':
    main(sys.argv[1:])

w = MyWorker(nameserver='127.0.0.1', run_id = '1')
    