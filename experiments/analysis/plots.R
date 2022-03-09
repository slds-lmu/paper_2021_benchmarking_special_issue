library(data.table)
library(mlr3misc)
library(ggplot2)
library(scales)
library(ggpubr)
library(ggstance)
source("../publication_themes.R")
source("ablation_prepare.R")  # for design tables

theme_set(theme_Publication())

scale_colour_discrete = scale_colour_Publication()
scale_fill_discrete = scale_fill_Publication()

lcbench_agg = readRDS("../results/lcbench_agg_results_new.rds")  # new is based on Martin's toplot.rds additional BO & 32 runs
#lcbench_agg = lcbench_agg[algorithm %in% c("hpbster_bohb", "mlr3hyperband", "randomsearch_full_budget", "smac_full_budget", "BO", "rq1_1", "rq1_2", "rq1_3", "rq1_4")]
lcbench_agg = lcbench_agg[cumbudget <= 1 * 52 * 30 * 7]
lcbench_agg[, cumbudget := cumbudget / 52]
lcbench_agg[, mean_best := mean_mean_best]
lcbench_agg[, se_best := se_mean_best]


lcbench_aggp = readRDS("../results/lcbench_agg_results_new.rds")  # new is based on Martin's toplot.rds additional BO & 32 runs
lcbench_aggp = lcbench_aggp[algorithm %in% c("BOHB_parallel", "HB_parallel", "RS_parallel", "BO_parallel", "rq7_1_parallel", "rq7_2_parallel")]
lcbench_aggp[, cumbudget := cumbudget / 52]
lcbench_aggp[, mean_best := mean_mean_best]
lcbench_aggp[, se_best := se_mean_best]


rbv2_super_agg = readRDS("../results/rbv2_super_agg_results.rds")
rbv2_super_agg = rbv2_super_agg[cumbudget <= 1 * 1 * 30 * 37]
rbv2_super_agg[, mean_best := mean_mean_best]
rbv2_super_agg[, se_best := se_mean_best]


nb301_agg = readRDS("../results/nb301_agg_results.rds")
nb301_agg = nb301_agg[cumbudget <= 1 * 98 * 30 * 30]
nb301_agg[, cumbudget := cumbudget / 98]

asexp = function(label) {
  sapply(label, function(l) {
    switch(l,
           "BO" = "GPBO",
           "BOHB" = "BOHB",
           "HB" = "HB",
           "RS" = "RS",
           "sm1" = expression(gamma^"*"~(lcbench~bm~HB)),
           "sm2" = expression(gamma^"*"~(lcbench~bm~equal)),
           "sm3" = expression(gamma^"*"~(rbv2_super~bm~HB)),
           "sm4" = expression(gamma^"*"~(rbv2_super~bm~equal)),
           "SMAC" = "SMAC"
    )
  })
}

plot_rq1 = function(dat, logscale = TRUE, benchmark) {
  datx = dat[mean_best <= 2 & cumbudget >= 1]
  inter = dat[cumbudget <= 1, .(mcb = max(cumbudget)), by = .(algorithm)]
  dat = merge(dat, inter, by = "algorithm")
  inc = dat[cumbudget == mcb, .(mean_best, se_best), by = .(algorithm)]
  dat = rbind(dat, inc, fill = TRUE)
  if (benchmark == "lcbench") {
    rq1 = dat[algorithm %in% c("hpbster_bohb", "mlr3hyperband", "randomsearch_full_budget", "smac_full_budget", "BO", "rq1_1", "rq1_2", "rq1_3", "rq1_4")]
    rq1$algorithm = factor(rq1$algorithm,
                           levels = c("hpbster_bohb", "mlr3hyperband", "randomsearch_full_budget", "smac_full_budget", "rq1_1", "rq1_2", "rq1_3", "rq1_4", "BO"),
                           labels = c("BOHB", "HB", "RS", "SMAC", "sm1", "sm2", "sm3", "sm4", "BO"))
  } else {
    rq1 = dat[algorithm %in% c("hpbster_bohb", "mlr3hyperband", "randomsearch_full_budget", "smac_full_budget", "rq1_1", "rq1_2", "rq1_3", "rq1_4")]
    rq1$algorithm = factor(rq1$algorithm,
                           levels = c("hpbster_bohb", "mlr3hyperband", "randomsearch_full_budget", "smac_full_budget", "rq1_1", "rq1_2", "rq1_3", "rq1_4", "BO"),
                           labels = c("BOHB", "HB", "RS", "SMAC", "sm1", "sm2", "sm3", "sm4", "BO"))
  }
  p = ggplot(aes(y = mean_best, x = cumbudget, colour = algorithm, fill = algorithm), data = rq1) +
    geom_line() +
    geom_ribbon(aes(ymin = mean_best - se_best, ymax = mean_best + se_best), colour = NA, alpha = 0.1) +
    xlab("Budget in Multiples of Max Budget") +
    ylab("Mean Normalized Regret") +
    labs(title = paste0(benchmark, " Test Instances"), colour = "Algorithm", fill = "Algorithm")
  if (logscale) {
    p = p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))
  }
  p = p + scale_colour_Publication(labels = asexp, drop = FALSE) + scale_fill_Publication(labels = asexp, drop = FALSE)
  if (benchmark == "lcbench") {
    p = p + coord_cartesian(ylim = c(0, 2), xlim = c(1, max(rq1$cumbudget)))
  } else if (benchmark == "rbv2_super") {
    p = p + coord_cartesian(ylim = c(0, 2), xlim = c(1, max(rq1$cumbudget)))
  } else if (benchmark == "nb301") {
    p = p + coord_cartesian(ylim = c(0, 2), xlim = c(1, max(rq1$cumbudget)))
  }
  p
}

rq1_parallel = {
  dat = lcbench_aggp
  rq1 = dat
  mb = min(rq1[, .(mcb = max(cumbudget)), by = .(algorithm)]$mcb)
  rq1 = rq1[cumbudget <= mb]
  rq1$algorithm = factor(rq1$algorithm,
                           levels = c("BOHB_parallel", "HB_parallel", "RS_parallel", "SMAC_parallel", "rq1_1", "rq7_1_parallel", "rq1_3", "rq7_2_parallel", "BO_parallel"),
                           labels = c("BOHB", "HB", "RS", "SMAC", "sm1", "sm2", "sm3", "sm4", "BO"))
  p = ggplot(aes(y = mean_best, x = cumbudget, colour = algorithm, fill = algorithm), data = rq1) +
    geom_line() +
    geom_ribbon(aes(ymin = mean_best - se_best, ymax = mean_best + se_best), colour = NA, alpha = 0.1) +
    xlab("Budget in Multiples of Max Budget x 32") +
    ylab("Mean Normalized Regret") +
    labs(title = "lcbench Test Instances Parallel Runs", colour = "Algorithm", fill = "Algorithm")
  p = p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))
  p = p + scale_colour_Publication(labels = asexp, drop = FALSE) + scale_fill_Publication(labels = asexp, drop = FALSE) + coord_cartesian(ylim = c(0, 0.6), xlim = c(1, max(rq1$cumbudget)))
  p
}

#lcbench_agg = lcbench_agg[algorithm %in% c("hpbster_bohb", "mlr3hyperband", "randomsearch_full_budget", "smac_full_budget", "BO")]
#rbv2_super_agg = rbv2_super_agg[algorithm %in% c("hpbster_bohb", "mlr3hyperband", "randomsearch_full_budget", "smac_full_budget")]
#nb301_agg = nb301_agg[algorithm %in% c("hpbster_bohb", "mlr3hyperband", "randomsearch_full_budget", "smac_full_budget")]

g_rq1 = list(plot_rq1(lcbench_agg, benchmark = "lcbench"), plot_rq1(rbv2_super_agg, benchmark = "rbv2_super"), plot_rq1(nb301_agg, benchmark = "nb301"), rq1_parallel)
g = ggarrange(plotlist = g_rq1, nrow = 1, ncol = 4, legend = "top", common.legend = TRUE)
ggsave("../plots/rq1.png", plot = g, device = "png", width = 20, height = 6)



plot_rq4 = function(dat, logscale = TRUE, benchmark) {
  rq4 = dat[algorithm %in% c("rq1_1", "rq1_2", "rq1_3", "rq1_4", "rq4_1", "rq4_2", "rq4_3", "rq4_4")]
  labs = c("SMASHY (lcbench BM HB)", "SMASHY (lcbench BM Equal)", "SMASHY (rbv2_super BM HB)", "SMASHY (rbv2_super BM Equal)")
  labs_ = c(labs, paste0(labs, " No MF"))
  rq4$algorithm_ = factor(rq4$algorithm, labels = labs_)
  rq4[, mf := !grepl("No MF", algorithm_)]
  rq4$algorithm = factor(rq4$algorithm, labels = rep(labs, 2))
  p = ggplot(aes(y = mean_best, x = cumbudget, colour = algorithm, fill = algorithm, linetype = mf), data = rq4) +
    geom_line() +
    geom_ribbon(aes(ymin = mean_best - se_best, ymax = mean_best + se_best), colour = NA, alpha = 0.1) +
    xlab("Budget in Multiples of Max Budget") +
    ylab("Mean Normalized Regret") +
    labs(title = paste0(benchmark, " Test Instances"), colour = "Algorithm", fill = "Algorithm", linetype = "Multifidelity")
  if (logscale) {
    p = p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))
  }
  p = p + scale_colour_Publication() + scale_fill_Publication()
  p
}

g_rq4 = list(plot_rq4(lcbench_agg, benchmark = "lcbench"), plot_rq4(rbv2_super_agg, benchmark = "rbv2_super"), plot_rq4(nb301_agg, benchmark = "nb301"))
g = ggarrange(plotlist = g_rq4, nrow = 1, ncol = 3, common.legend = TRUE)
ggsave("../plots/rq4.png", plot = g, device = "png", width = 20, height = 6)



plot_rq5a = function(dat, logscale = TRUE, benchmark) {
  rq5a = dat[algorithm %in% c("rq1_1", "rq1_2", "rq1_3", "rq1_4", "rq5a_1", "rq5a_2", "rq5a_3", "rq5a_4", "rq5a_cond_1", "rq5a_cond_2", "rq5a_cond_3", "rq5a_cond_4")]
  labs = c("SMASHY (lcbench BM HB)", "SMASHY (lcbench BM Equal)", "SMASHY (rbv2_super BM HB)", "SMASHY (rbv2_super BM Equal)")
  labs_ = c(labs, paste0(labs, "t Pars Constant"), paste0(labs, " t Pars Mean"))
  rq5a$algorithm_ = factor(rq5a$algorithm, labels = labs_)
  rq5a[, var := ifelse(grepl("Constant", algorithm_), yes = "t Pars Constant", no = ifelse(grepl("Mean", algorithm_), yes = "t Pars Mean", no = "Default"))]
  rq5a$algorithm = factor(rq5a$algorithm, labels = rep(labs, 3))
  p = ggplot(aes(y = mean_best, x = cumbudget, colour = algorithm, fill = algorithm, linetype = var), data = rq5a) +
    geom_line() +
    geom_ribbon(aes(ymin = mean_best - se_best, ymax = mean_best + se_best), colour = NA, alpha = 0.1) +
    xlab("Budget in Multiples of Max Budget") +
    ylab("Mean Normalized Regret") +
    labs(title = paste0(benchmark, " Test Instances"), colour = "Algorithm", fill = "Algorithm", linetype = "Variation")
  if (logscale) {
    p = p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))
  }
  p = p + scale_colour_Publication() + scale_fill_Publication()
  p
}

g_rq5a = list(plot_rq5a(lcbench_agg, benchmark = "lcbench"), plot_rq5a(rbv2_super_agg, benchmark = "rbv2_super"), plot_rq5a(nb301_agg, benchmark = "nb301"))
g = ggarrange(plotlist = g_rq5a, nrow = 1, ncol = 3, common.legend = TRUE)
ggsave("../plots/rq5a.png", plot = g, device = "png", width = 20, height = 6)



plot_rq5b = function(dat, logscale = TRUE, benchmark) {
  rq5b = dat[algorithm %in% c("rq1_1", "rq1_2", "rq1_3", "rq1_4", "rq5b_1", "rq5b_2", "rq5b_3", "rq5b_4", "rq5b_cond_1", "rq5b_cond_2", "rq5b_cond_3", "rq5b_cond_4")]
  labs = c("SMASHY (lcbench BM HB)", "SMASHY (lcbench BM Equal)", "SMASHY (rbv2_super BM HB)", "SMASHY (rbv2_super BM Equal)")
  labs_ = c(labs, paste0(labs, "Sample Random"), paste0(labs, "Filtering Mean"))
  rq5b$algorithm_ = factor(rq5b$algorithm, labels = labs_)
  rq5b[, var := ifelse(grepl("Random", algorithm_), yes = "Sample Random", no = ifelse(grepl("Mean", algorithm_), yes = "Filtering Mean", no = "Default"))]
  rq5b$algorithm = factor(rq5b$algorithm, labels = rep(labs, 3))
  p = ggplot(aes(y = mean_best, x = cumbudget, colour = algorithm, fill = algorithm, linetype = var), data = rq5b) +
    geom_line() +
    geom_ribbon(aes(ymin = mean_best - se_best, ymax = mean_best + se_best), colour = NA, alpha = 0.1) +
    xlab("Budget in Multiples of Max Budget") +
    ylab("Mean Normalized Regret") +
    labs(title = paste0(benchmark, " Test Instances"), colour = "Algorithm", fill = "Algorithm", linetype = "Variation")
  if (logscale) {
    p = p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))
  }
  p = p + scale_colour_Publication() + scale_fill_Publication()
  p
}

g_rq5b = list(plot_rq5b(lcbench_agg, benchmark = "lcbench"), plot_rq5b(rbv2_super_agg, benchmark = "rbv2_super"), plot_rq5b(nb301_agg, benchmark = "nb301"))
g = ggarrange(plotlist = g_rq5b, nrow = 1, ncol = 3, common.legend = TRUE)
ggsave("../plots/rq5b.png", plot = g, device = "png", width = 20, height = 6)



plot_rq6 = function(dat,logscale = TRUE, benchmark) {
  rq6 = dat[algorithm %in% c("rq1_2", "rq1_4", paste0("rq6_", 1:6))]
  rq6 = rq6[cumbudget >= 10^2]
  sl = rbind(rq.1.tbl[c(2, 4), c("algorithm", "surrogate_learner")], rq.6.tbl[1:6, c("algorithm", "surrogate_learner")])
  labs = c("SMASHY (lcbench BM Equal)", "SMASHY (rbv2_super BM Equal)")
  sl$labs = c(labs, rep(labs, each = 3))
  rq6 = merge(rq6, sl)
  p = ggplot(aes(y = mean_best, x = cumbudget, colour = surrogate_learner, fill = surrogate_learner, linetype = labs), data = rq6) +
    geom_line() +
    geom_ribbon(aes(ymin = mean_best - se_best, ymax = mean_best + se_best), colour = NA, alpha = 0.1) +
    xlab("Budget in Multiples of Max Budget") +
    ylab("Mean Normalized Regret") +
    labs(title = paste0(benchmark, " Test Instances"), colour = "Surrogate Model", fill = "Surrogate Model", linetype = "Algorithm")
  if (logscale) {
    p = p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))
  }
  p = p + scale_colour_Publication() + scale_fill_Publication()
  p
}

g_rq6 = list(plot_rq6(lcbench_agg, benchmark = "lcbench"), plot_rq6(rbv2_super_agg, benchmark = "rbv2_super"), plot_rq6(nb301_agg, benchmark = "nb301"))
g = ggarrange(plotlist = g_rq6, nrow = 1, ncol = 3, common.legend = TRUE)
ggsave("../plots/rq6.png", plot = g, device = "png", width = 20, height = 6)



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
    geom_ribbon(aes(ymin = mean_best - se_best, ymax = mean_best + se_best), colour = NA, alpha = 0.1) +
    xlab("Budget in Multiples of Max Budget") +
    ylab("Mean Normalized Regret") +
    labs(title = paste0(benchmark, " Test Instances"), colour = "Random Interleaving", fill = "Random Interleaving", linetype = "Algorithm")
  if (logscale) {
    p = p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))
  }
  p = p + scale_colour_Publication() + scale_fill_Publication()
  p
}

g_rq6_ri = list(plot_rq6_ri(lcbench_agg, benchmark = "lcbench"), plot_rq6_ri(rbv2_super_agg, benchmark = "rbv2_super"), plot_rq6_ri(nb301_agg, benchmark = "nb301"))
g = ggarrange(plotlist = g_rq6_ri, nrow = 1, ncol = 3, common.legend = TRUE)
ggsave("../plots/rq6_ri.png", plot = g, device = "png", width = 20, height = 6)

lcbenchx = lcbench_agg[algorithm %in% c(#"hpbster_bohb", "mlr3hyperband", "randomsearch_full_budget", "smac_full_budget",
                                        paste0("rq1_", 1:4), paste0("rq4_", 1:4), paste0("rq5a_", 1:4), paste0("rq5b_", 1:4), paste0("rq6_", 1:8), paste0("rq6_fix_", 7:8))]
lcbenchx[, cfg := "lcbench"]
lcbenchx[, mcb := max(cumbudget), by = .(algorithm)]
lcbenchx = lcbenchx[cumbudget == mcb]
lcbenchx[, x := mean_mean_best]
lcbenchx[, se := se_mean_best]

rbv2_superx = rbv2_super_agg[algorithm %in% c(#"hpbster_bohb", "mlr3hyperband", "randomsearch_full_budget", "smac_full_budget",
                                              paste0("rq1_", 1:4), paste0("rq4_", 1:4), paste0("rq5a_", 1:4), paste0("rq5b_", 1:4), paste0("rq6_", 1:8), paste0("rq6_fix_", 7:8))]
rbv2_superx[, cfg := "rbv2_super"]
rbv2_superx[, mcb := max(cumbudget), by = .(algorithm)]
rbv2_superx = rbv2_superx[cumbudget == mcb]
rbv2_superx[, x := mean_mean_best]
rbv2_superx[, se := se_mean_best]

nb301x = nb301_agg[algorithm %in% c(#"hpbster_bohb", "mlr3hyperband", "randomsearch_full_budget", "smac_full_budget",
                                    paste0("rq1_", 1:4), paste0("rq4_", 1:4), paste0("rq5a_", 1:4), paste0("rq5b_", 1:4), paste0("rq6_", 1:8), paste0("rq6_fix_", 7:8))]
nb301x[, cfg := "nb301"]
nb301x[, mcb := max(cumbudget), by = .(algorithm)]
nb301x = nb301x[cumbudget == mcb]
nb301x[, x := mean_best]
nb301x[, se := se_best]

rqx = rbind(lcbenchx, rbv2_superx, nb301x, fill = TRUE)

rqx[algorithm %in% c("rq1_1", "rq1_2", "rq4_1", "rq4_2", "rq5a_1", "rq5a_2", "rq5b_1", "rq5b_2", "rq6_1", "rq6_2", "rq6_3", "rq6_7", "rq6_fix_7"), objective := "lcbench"]
rqx[algorithm %in% c("rq1_3", "rq1_4", "rq4_3", "rq4_4", "rq5a_3", "rq5a_4", "rq5b_3", "rq5b_4", "rq6_4", "rq6_5", "rq6_6", "rq6_8", "rq6_fix_8"), objective := "rbv2_super"]

rqx[algorithm %in% c("rq1_1", "rq1_3", "rq4_1", "rq4_3", "rq5a_1", "rq5a_3", "rq5b_1", "rq5b_3"), batch_method := "HB"]
rqx[algorithm %in% c("rq1_2", "rq1_4", "rq4_2", "rq4_4", "rq5a_2", "rq5a_4", "rq5b_2", "rq5b_4", paste0("rq6", "_", 1:8), paste0("rq6_fix", "_", 7:8)), batch_method := "equal"]

rqx[grepl("_1|_2", algorithm), surrogate_learner := "KKNN7"]
rqx[grepl("_3|_4", algorithm), surrogate_learner := "KNN1"]
rqx[algorithm %in% c("rq6_1", "rq6_4"), surrogate_learner := "TPE"]
rqx[algorithm %in% c("rq6_3", "rq6_6"), surrogate_learner := "RF"]
rqx[algorithm %in% c("rq6_2"), surrogate_learner := "KNN1"]
rqx[algorithm %in% c("rq6_5"), surrogate_learner := "KKNN7"]
rqx[algorithm %in% c("rq6_7"), surrogate_learner := "Sample KDE"]
rqx[algorithm %in% c("rq6_8"), surrogate_learner := "Sample KDE"]
rqx[algorithm %in% c("rq6_fix_7"), surrogate_learner := "none"]
rqx[algorithm %in% c("rq6_fix_8"), surrogate_learner := "none"]

gamma_names = list(
  "hpbster_bohb" = "BOHB",
  "mlr3hyperband" = "HB",
  "randomsearch_full_budget" = "RS",
  "smac_full_budget" = "SMAC",
  "rq1_1" = "gamma", "rq1_2" = "gamma", "rq1_3" = "gamma", "rq1_4" = "gamma",
  "rq4_1" = "gamma1", "rq4_2" = "gamma1", "rq4_3" = "gamma1", "rq4_4" = "gamma1",
  "rq5a_1" = "gamma2", "rq5a_2" = "gamma2", "rq5a_3" = "gamma2", "rq5a_4" = "gamma2",
  "rq5b_1" = "gamma3", "rq5b_2" = "gamma3", "rq5b_3" = "gamma3", "rq5b_4" = "gamma3",
  "rq6_1" = "gamma4", "rq6_2" = "gamma4", "rq6_3" = "gamma4", "rq6_4" = "gamma4", "rq6_5" = "gamma4", "rq6_6" = "gamma4",
  "rq6_7" = "gamma5", "rq6_8" = "gamma5",
  "rq6_fix_7" = "gamma6", "rq6_fix_8" = "gamma6"
) 

gamma_labeller = function(algorithm) {
  return(sapply(algorithm, function(i) gamma_names[[i]]))
}

rqx$algorithm = factor(rqx$algorithm,
  levels = rev(c("hpbster_bohb", "mlr3hyperband", "randomsearch_full_budget", "smac_full_budget",
                 paste0("rq1_", 1:4), paste0("rq4_", 1:4), paste0("rq5a_", 1:4), paste0("rq5b_", 1:4), paste0("rq6_", 1:8), paste0("rq6_fix_", 7:8))),
  labels = gamma_labeller(rev(c("hpbster_bohb", "mlr3hyperband", "randomsearch_full_budget", "smac_full_budget",
                                paste0("rq1_", 1:4), paste0("rq4_", 1:4), paste0("rq5a_", 1:4), paste0("rq5b_", 1:4), paste0("rq6_", 1:8), paste0("rq6_fix_", 7:8))))
)
rqx$cfg = factor(rqx$cfg, levels = c("lcbench", "rbv2_super", "nb301"))

asexp = function(gamma) {
  sapply(gamma, function(g) {
    switch(g,
           "BOHB" = "BOHB",
           "HB" = "HB",
           "RS" = "RS",
           "SMAC" = "SMAC",
           "gamma" = expression(gamma^"*"),
           "gamma1" = expression(gamma[1]), "gamma2" = expression(gamma[2]), "gamma3" = expression(gamma[3]),
           "gamma4" = expression(gamma[4]), "gamma5" = expression(gamma[5]), "gamma6" = expression(gamma[6])
    )
  })
}

p = ggplot(aes(x = x, y = algorithm, colour = objective, shape = surrogate_learner, linetype = batch_method), data = rqx) +
  geom_point(size = 2, position = position_dodgev(height = 1)) +
  geom_errorbar(aes(xmin = x - se, xmax = x + se), width = 0, position = position_dodgev(height = 1)) +
  facet_grid(~ cfg, scales = "free_x") +
  xlab("Mean Normalized Regret") +
  ylab("") +
  labs(colour = "Scenario", shape = expression(I[f[surr]]), linteype = "batch_method") +
  scale_y_discrete(labels = asexp) +
  scale_colour_Publication() + scale_fill_Publication()

ggsave("../plots/ablation.png", plot = p, device = "png", width = 10, height = 6)

