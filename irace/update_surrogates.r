library(mfsurrogates)
workdir = "./irace/data/surrogates"

cfg = cfgs("lcbench", workdir = workdir)
cfg$setup(force_download = TRUE)

cfg = cfgs("rbv2_super", workdir = workdir)
cfg$setup(force_download = TRUE)
