library(data.table)
library(mlr3misc)
library(ggplot2)
library(scales)
library(ggpubr)
source("../publication_themes.R")
source("ablation_prepare.R")  # for design tables

theme_set(theme_Publication())

scale_colour_discrete = scale_colour_Publication()
scale_fill_discrete = scale_fill_Publication()

lcbench_agg = readRDS("lcbench_agg_results.rds")
lcbench_agg = lcbench_agg[cumbudget <= 1 * 52 * 30 * 7]
lcbench_agg[, cumbudget := cumbudget / 52]

rbv2_super_agg = readRDS("rbv2_super_agg_results.rds")
rbv2_super_agg = rbv2_super_agg[cumbudget <= 1 * 1 * 30 * 37]

nb301_agg = readRDS("nb301_agg_results.rds")
nb301_agg = nb301_agg[cumbudget <= 1 * 98 * 30 * 30]
nb301_agg[, cumbudget := cumbudget / 98]

plot_rq1 = function(dat, logscale = TRUE, benchmark) {
  rq1 = dat[algorithm %in% c("hpbster_bohb", "mlr3hyperband", "randomsearch_full_budget", "smac_full_budget", "rq1_1", "rq1_2", "rq1_3", "rq1_4")]
  rq1$algorithm = factor(rq1$algorithm, labels = c("BOHB", "HB", "RS", "SMASHY (lcbench BM HB)", "SMASHY (lcbench BM Equal)", "SMASHY (rbv2_super BM HB)", "SMASHY (rbv2_super BM Equal)", "SMAC"))
  p = ggplot(aes(y = mean_best, x = cumbudget, colour = algorithm, fill = algorithm), data = rq1) +
    geom_line() +
    geom_ribbon(aes(ymin = mean_best - sd_best / sqrt(n), ymax = mean_best + sd_best / sqrt(n)), colour = NA, alpha = 0.1) +
    xlab("Budget in Multiples of Max Budget") +
    ylab("Mean Normalized Regret over Replications and Tasks") +
    labs(title = paste0(benchmark, " Test Instances"), colour = "Algorithm", fill = "Algorithm")
  if (logscale) {
    p = p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))
  }
  p = p + scale_colour_Publication() + scale_fill_Publication()
  p
}

g_rq1 = list(plot_rq1(lcbench_agg, benchmark = "lcbench"), plot_rq1(rbv2_super_agg, benchmark = "rbv2_super"), plot_rq1(nb301_agg, benchmark = "nb301"))
g = ggarrange(plotlist = g_rq1, nrow = 1, ncol = 3, common.legend = TRUE)
ggsave("rq1.png", plot = g, device = "png", width = 20, height = 6)



plot_rq4 = function(dat, logscale = TRUE, benchmark) {
  rq4 = dat[algorithm %in% c("rq1_1", "rq1_2", "rq1_3", "rq1_4", "rq4_1", "rq4_2", "rq4_3", "rq4_4")]
  labs = c("SMASHY (lcbench BM HB)", "SMASHY (lcbench BM Equal)", "SMASHY (rbv2_super BM HB)", "SMASHY (rbv2_super BM Equal)")
  labs_ = c(labs, paste0(labs, " No MF"))
  rq4$algorithm_ = factor(rq4$algorithm, labels = labs_)
  rq4[, mf := !grepl("No MF", algorithm_)]
  rq4$algorithm = factor(rq4$algorithm, labels = rep(labs, 2))
  p = ggplot(aes(y = mean_best, x = cumbudget, colour = algorithm, fill = algorithm, linetype = mf), data = rq4) +
    geom_line() +
    geom_ribbon(aes(ymin = mean_best - sd_best / sqrt(n), ymax = mean_best + sd_best / sqrt(n)), colour = NA, alpha = 0.1) +
    xlab("Budget in Multiples of Max Budget") +
    ylab("Mean Normalized Regret over Replications and Tasks") +
    labs(title = paste0(benchmark, " Test Instances"), colour = "Algorithm", fill = "Algorithm", linetype = "Multifidelity")
  if (logscale) {
    p = p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))
  }
  p = p + scale_colour_Publication() + scale_fill_Publication()
  p
}

g_rq4 = list(plot_rq4(lcbench_agg, benchmark = "lcbench"), plot_rq4(rbv2_super_agg, benchmark = "rbv2_super"), plot_rq4(nb301_agg, benchmark = "nb301"))
g = ggarrange(plotlist = g_rq4, nrow = 1, ncol = 3, common.legend = TRUE)
ggsave("rq4.png", plot = g, device = "png", width = 20, height = 6)



plot_rq6 = function(dat,logscale = TRUE, benchmark) {
  rq6 = dat[algorithm %in% c("rq1_2", "rq1_4", paste0("rq6_", 1:6))]
  rq6 = rq6[cumbudget >= 10^2]
  sl = rbind(rq.1.tbl[c(2, 4), c("algorithm", "surrogate_learner")], rq.6.tbl[1:6, c("algorithm", "surrogate_learner")])
  labs = c("SMASHY (lcbench BM Equal)", "SMASHY (rbv2_super BM Equal)")
  sl$labs = c(labs, rep(labs, each = 3))
  rq6 = merge(rq6, sl)
  p = ggplot(aes(y = mean_best, x = cumbudget, colour = surrogate_learner, fill = surrogate_learner, linetype = labs), data = rq6) +
    geom_line() +
    geom_ribbon(aes(ymin = mean_best - sd_best / sqrt(n), ymax = mean_best + sd_best / sqrt(n)), colour = NA, alpha = 0.1) +
    xlab("Budget in Multiples of Max Budget") +
    ylab("Mean Normalized Regret over Replications and Tasks") +
    labs(title = paste0(benchmark, " Test Instances"), colour = "Surrogate Model", fill = "Surrogate Model", linetype = "Algorithm")
  if (logscale) {
    p = p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))
  }
  p = p + scale_colour_Publication() + scale_fill_Publication()
  p
}

g_rq6 = list(plot_rq6(lcbench_agg, benchmark = "lcbench"), plot_rq6(rbv2_super_agg, benchmark = "rbv2_super"), plot_rq6(nb301_agg, benchmark = "nb301"))
g = ggarrange(plotlist = g_rq6, nrow = 1, ncol = 3, common.legend = TRUE)
ggsave("rq6.png", plot = g, device = "png", width = 20, height = 6)



plot_rq6_ri = function(dat,logscale = TRUE, benchmark) {
  rq6 = dat[algorithm %in% c("rq1_2", "rq1_4", paste0("rq6_", 9:10), paste0("rq6_fix_", 7:8))]
  rq6 = rq6[cumbudget >= 10^2]
  sl = rbind(rq.1.tbl[c(2, 4), c("algorithm", "surrogate_learner")], rq.6.tbl[9:10, c("algorithm", "surrogate_learner")], rq.6.tbl_fix[7:8, c("algorithm", "surrogate_learner")])
  sl[, random_interleaving := c("default", "default", "0", "0", "1", "1")]
  labs = c("SMASHY (lcbench BM Equal)", "SMASHY (rbv2_super BM Equal)")
  sl$labs = rep(labs, 3)
  rq6 = merge(rq6, sl)
  p = ggplot(aes(y = mean_best, x = cumbudget, colour = random_interleaving, fill = random_interleaving, linetype = labs), data = rq6) +
    geom_line() +
    geom_ribbon(aes(ymin = mean_best - sd_best / sqrt(n), ymax = mean_best + sd_best / sqrt(n)), colour = NA, alpha = 0.1) +
    xlab("Budget in Multiples of Max Budget") +
    ylab("Mean Normalized Regret over Replications and Tasks") +
    labs(title = paste0(benchmark, " Test Instances"), colour = "Random Interleaving", fill = "Random Interleaving", linetype = "Algorithm")
  if (logscale) {
    p = p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))
  }
  p = p + scale_colour_Publication() + scale_fill_Publication()
  p
}

g_rq6_ri = list(plot_rq6_ri(lcbench_agg, benchmark = "lcbench"), plot_rq6_ri(rbv2_super_agg, benchmark = "rbv2_super"), plot_rq6_ri(nb301_agg, benchmark = "nb301"))
g = ggarrange(plotlist = g_rq6_ri, nrow = 1, ncol = 3, common.legend = TRUE)
ggsave("rq6_ri.png", plot = g, device = "png", width = 20, height = 6)

# ToDO
# rq5 & rq5cond stuff missing
# ranks
# unify and prettify plots
