



library("data.table")
library("mlr3pipelines")
library("mlr3")
library("mlr3learners")
library("ggthemes")
library("ggplot2")
source("~/projects/paper_2021_benchmarking_special_issue/experiments/publication_themes.R")

theme_set(theme_Publication())

scale_colour_discrete = scale_colour_Publication()
scale_fill_discrete = scale_fill_Publication()



scale_01 = function(x) (x - min(x)) / (max(x) - min(x))


colnames(combined)[which(colnames(combined) == "curseed")[[1]]] <- "evalseed"

combined.X <- combined[, gsub("^x\\.", "", grep("^x\\.", colnames(combined), value = TRUE)), with = FALSE]
combined.X.trafod <- combined[, grep("^x\\.", colnames(combined), value = TRUE), with = FALSE]
combined.Y <- combined[, .(yval)]
combined.X.trafod = combined.X.trafod[, map_if(.SD, is.character, as.factor)]
combined.X.trafod = combined.X.trafod[, map_if(.SD, is.logical, as.numeric)]


obj = combined$objective


combined.X.trafod

X



canonical




lambdawins <- cbind(rbindlist(canonical$lambda, fill = TRUE), batch_method = canonical$batchmethod)
setnames(lambdawins, colnames(lambdawins), paste0("x.", colnames(lambdawins)))

aa2 <- lambdawins[
    is.na(x.filter_factor_last), x.filter_factor_last := x.filter_factor_first][
    is.na(x.filter_algorithm), x.filter_algorithm := "tournament"][
    is.na(x.filter_select_per_tournament), x.filter_select_per_tournament := 1][
    is.na(x.filter_factor_first.end), x.filter_factor_first.end := x.filter_factor_first][
    is.na(x.random_interleave_fraction.end), x.random_interleave_fraction.end := x.random_interleave_fraction][
    is.na(x.filter_factor_last.end), x.filter_factor_last.end := x.filter_factor_last][
    is.na(x.filter_select_per_tournament.end), x.filter_select_per_tournament.end := x.filter_select_per_tournament][
    is.na(x.mu), x.mu := 32][]
aa2 <- cbind(aa2, canonical[, .(objective, siman, infillsearch, name = "")])

aa2 = aa2[, map_if(.SD, is.character, as.factor)]
aa2 = aa2[, map_if(.SD, is.logical, as.numeric)]

aa2[siman & infillsearch == "all", name := "gamma*"]
aa2[!siman & infillsearch == "all", name := "gamma2"]
aa2[!siman & infillsearch == "rs", name := "gamma3"]
aa2 <- aa2[name != ""]
aa2[, name := sprintf("%s[%s]", name, x.batch_method)]


aa2


curobj <- "rbv2_super"


getsmooth <- function(curobj) {
  dt <- cbind(combined.X.trafod, combined.Y)[obj == curobj, ]


  tsk <- TaskRegr$new(id = "performances", backend = dt, target = "yval")
  l <- lrn("regr.ranger")
  l$train(tsk)
  prd <- l$predict(tsk)
  copy(dt)[, yval := prd$data$response]
}

dtsmooth <- getsmooth(curobj)


dtsmooth


# colorder: batch_method next to mu


ss.transform <- function(dt) {
  copy(dt)[, `:=`(
    x.filter_with_max_budget = NULL,
    x.filter_select_per_tournament = NULL,
    x.filter_select_per_tournament.end = NULL,
    x.random_interleave_random = NULL,
    x.eta_surv = log(pmax(pmin(1 / x.survival_fraction, x.mu - 1), 1)),
    x.mu = log(x.mu),
    x.survival_fraction = NULL,
    x.filter_factor = sqrt(x.filter_factor_first * x.filter_factor_last),
    x.filter_factor.end = sqrt(x.filter_factor_first.end * x.filter_factor_last.end),
    x.filter_factor_last = NULL, x.filter_factor_first = NULL,
    x.filter_factor_last.end = NULL, x.filter_factor_first.end = NULL)][, `:=`(
      x.filter_factor = log(x.filter_factor),
      x.filter_factor.end = log(x.filter_factor.end),
      x.random_interleave_fraction = (x.random_interleave_fraction),
      x.random_interleave_fraction.end = (x.random_interleave_fraction.end))][]
  # [, `:=`(
#    x.filter_factor_avg = log(sqrt(x.filter_factor * x.filter_factor.end)),
#    x.filter_factor_change = log(x.filter_factor.end / x.filter_factor),
#    x.random_interleave_fraction_avg = (x.random_interleave_fraction + x.random_interleave_fraction.end) / 2,
#    x.random_interleave_fraction_change = x.random_interleave_fraction.end - x.random_interleave_fraction#,
#    x.filter_factor = NULL, x.filter_factor.end = NULL,
#    x.random_interleave_fraction = NULL, x.random_interleave_fraction.end = NULL)][]
#)]
}



dt.origin.rbv2_super <- ss.transform(getsmooth("rbv2_super")[combined[obj == "rbv2_super", infillsearch == "all" & siman]])
dt.origin.lcbench <- ss.transform(getsmooth("lcbench")[combined[obj == "lcbench", infillsearch == "all" & siman]])
# dt.origin <- ss.transform(dtsmooth)

dt.origin <- rbind(dt.origin.rbv2_super[, scenario := "rbv2_super"], dt.origin.lcbench[, scenario := "lcbench"])[, scenario := as.factor(scenario)]

dtt = copy(dt.origin)

dtt[, top_q := (yval >= quantile(yval, .9)), by = "scenario"]


dtt[, .N, by = "scenario"]

dtt[, yrank := (rank(-yval)), by = "scenario"]
dtt[, top_q := (yrank <= 80), by = "scenario"]

scaleData <- function(pdf, alldtts) {
  alldtts <- rbindlist(alldtts, fill = TRUE)
  scale_minmax = function(x, colname) {
    curcol <- alldtts[[as.character(colname)]]
    if (is.numeric(curcol)) {
      curcol <- if (any(curcol > 1)) c(log(1), log(1000)) else c(0, 1)
    }
    (x - min(as.numeric(curcol))) / (max(as.numeric(curcol)) - min(as.numeric(curcol)))
  }

  pdf[, vals := scale_minmax(vals, params), by = "params"]
}


# pdf <- melt(dtt, c("id", "yval", "top_q", "x.batch_method"), variable.name = "params", value.name = "vals")
# pdf <- melt(dtt, c("id", "yval", "top_q"), variable.name = "params", value.name = "vals")
transposeData <- function(dtt, idvars, alldtts) {

  dtt <- copy(dtt)
  # dtt[, top_q := (yval >= quantile(yval, .9))]
  #dtt[, top_q := (yval >= quantile(yval, .975))]
  # dtt = dtt[x.mu < 25 & x.sample == "bohb" & x.surrogate_learner %in% c("knn1", "knn7"), ]
  # dtt = dtt[x.filter_with_max_budget == TRUE & x.filter_select_per_tournament<=6 , ]
  # dtt = dtt[, -c(grep("random_interleave", colnames(dtt), value = TRUE)), with = FALSE]
  # dtt = dtt[, -c(grep("select_per_tournament", colnames(dtt), value = TRUE)), with = FALSE]
  # dtt = dtt[, -c(grep("filter_with_max_budget", colnames(dtt), value = TRUE)), with = FALSE]
  dtt[, id := seq_len(nrow(dtt))]
  # Drop constants:
  # dtt = dtt[, names(dtt)[dtt[, map_lgl(.SD, function(x) length(unique(x)) > 1)]], with = FALSE]
  dtt = dtt[, sapply(colnames(.SD), function(x) if (is.factor(.SD[[x]])) factor(.SD[[x]], levels = levels(dt.origin[[x]])) else .SD[[x]], simplify = FALSE)]
  dtt = dtt[, map_if(.SD, is.factor, as.numeric)]

  pdf <- melt(dtt, idvars, variable.name = "params", value.name = "vals")
  pdf <- scaleData(pdf, alldtts)
  pdf[, params := gsub("^x\\.", "", params)]
  param_levels = c("batch_method", "mu", "budget_log_step", "eta_surv", "filter_algorithm", # "filter_factor_avg", "filter_factor_change",
#    "random_interleave_fraction_avg", "random_interleave_fraction_change",
    "surrogate_learner",
    "filter_factor", "filter_factor.end", "random_interleave_fraction", "random_interleave_fraction.end", "sample")

  pdf[, params := factor(params, levels = param_levels)]
}

bests <- ss.transform(copy(aa2)[, `:=`(infillsearch = NULL, siman = NULL, scenario = objective, objective = NULL)])

pdf <- transposeData(dtt, c("id", "yval", "top_q", "yrank", "scenario"), list(dtt, bests))
bests.t <- transposeData(bests,  c("id", "name", "scenario"), list(dtt, bests))



annot = imap_dtr(dt.origin, function(x, nm) {
  nm <- gsub("^x\\.", "", nm)
  leveltranslation <- function(x) {
    trafo <- c(knn1 = "KNN1", knn7 = "KKNN7", ranger = "RF", bohblrn = "TPE",
      random = "uniform", bohb = "KDE", progressive = "Progressive", tournament = "Tournament", hb = "HB", smashy = "equal")
    ifelse(x %in% names(trafo), trafo[x], x)
  }
  if (!nm %in% c("yval", "scenario")) {
    if(is.numeric(x)) {
      data.frame("nm" = nm, yi = c(0, 1), label = if (max(x) > 1) c(1, 1000) else c(0, 1)) # round(c(min(x), max(x)),2))
    } else {
      data.frame("nm" = nm, yi = seq(from = 0, to = 1, length.out = length(levels(x))), label = leveltranslation(levels(x)))
    }
  }
})

pdata <- pdf[top_q == TRUE, ][params %in% c("batch_method", "filter_algorithm", "sample", "surrogate_learner"),
  vals := vals + rnorm(length(vals), 0, .02)
  ]


library("latex2exp")
library("ggbeeswarm")



p1 = ggplot(pdata[params != "batch_method"], aes(x = (params), y = vals, color = as.factor(scenario))) +
  scale_x_discrete(labels = c(
#    batch_method = "batch_method",
    "mu" = parse(text = TeX("$\\mu(1)$")),
    "budget_log_step" = parse(text = TeX("$\\eta_{fid}$")),
    "eta_surv" = parse(text = TeX("$\\eta_{surv}$")),
    "filter_algorithm" = "filter_method",
    "surrogate_learner" = parse(text = TeX("$I_{f_{surr}}$")),
    "filter_factor" = parse(text = TeX("$N_s(0)$")),
    "filter_factor.end" = parse(text = TeX("$N_s(1)$")),
    "random_interleave_fraction" = parse(text = TeX("$\\rho(0)$")),
    "random_interleave_fraction.end" = parse(text = TeX("$\\rho(1)$")),
    "sample" = parse(text = TeX("$P_{\\lambda}(A)$"))
    )) +
  geom_quasirandom(data = pdata[params != "batch_method"], dodge.width = .8) +
  scale_shape_manual(values = c(21, 24), name = "γ* batch_method", breaks = c(TRUE, FALSE), labels = c("HB", "equal")) +
#  scale_size_manual(values = c(4, 4)) +
  scale_colour_Publication(name = "Scenario", labels = c("LCbench", "rbv2_super      ")) +
  scale_y_continuous(breaks = c(0, 1/3, 2/3, 1), labels = function(x) parse(text = TeX(sprintf("$10^%s$", x * 3))),
    sec.axis = sec_axis(identity, breaks = c(0, 1/3, 2/3, 1), labels = c(0, "1/3", "2/3", 1))) +
#  theme_bw() +
  theme(axis.text.x = element_text(angle = 0)) +
  geom_label(data = annot[nm != "batch_method"], aes(x = as.factor(nm), y = yi + ifelse(yi == 0, -.05, .07), label = label, colour = NULL, alpha = NULL), key_glyph = "point") +
  guides(alpha = 'none') +
  geom_point(data = bests.t[grepl("*", name, fixed = TRUE)][params != "batch_method"], aes(x = (params), y = vals, group = as.factor(scenario),
    shape = grepl("hb", name), fill = as.factor(scenario)), # fill = substr(name, 1, 6), size = grepl("*", name, fixed = TRUE)),
    color = "black", position = position_jitterdodge(dodge.width = .8, jitter.width = .3), size = 4) +
  scale_fill_manual(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33"), guide = "none") +
  ggtitle("Best Configuration Parameters") +
  xlab("Parameter") +
  ylab("Value")
# p1

ggsave("coordinateplot.pdf", plot = p1, device = cairo_pdf, width = 8.58, height = 5.04)


# ---------

p1 = ggplot(pdf[top_q == TRUE, ], aes(x = params, y = vals, color = -yval, alpha=-yval)) +
  geom_violin(data = pdf[top_q == TRUE,], color = "red", fill = "black", draw_quantiles=c(0.5), trim=TRUE, adjust=.3, scale='width') +
  geom_point(data = pdf[top_q == TRUE,],position = position_jitter(width =.01, height=.01), alpha =.4) +
  theme_bw() +
  scale_alpha_continuous(range = c(0, .5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_line(data = pdf[top_q == TRUE,], aes(x = params, y = vals, color = -yval, group = id),
    position = position_jitter(width =.01, height=.01)) +
  geom_label(data = annot, aes(x = nm, y = yi, label = label, colour = NULL, alpha = NULL)) +
  guides(alpha = 'none') +
  ggtitle("rbv2_super")


p1

p1 = ggplot(pdf[top_q == TRUE, ], aes(x = params, y = vals, color = -yval, alpha=-yval)) +
  geom_violin(data = pdf[top_q == TRUE,], color = "red", fill = "black", draw_quantiles=c(0.5), trim=TRUE, adjust=.3, scale='width') +
  geom_point(data = pdf[top_q == TRUE,],position = position_jitter(width =.1, height=.01), alpha =.4) +
  theme_bw() +
  scale_alpha_continuous(range = c(0, .5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_label(data = annot, aes(x = nm, y = yi, label = label, colour = NULL, alpha = NULL)) +
  guides(alpha = 'none') +
  ggtitle("rbv2_super")

p1

library("ggbeeswarm")

p1 = ggplot(pdf[top_q == TRUE, ], aes(x = params, y = vals, color = -yval)) +
  geom_quasirandom(data = pdf[top_q == TRUE,]) +
  theme_bw() +
  scale_alpha_continuous(range = c(0, .5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_label(data = annot, aes(x = nm, y = yi, label = label, colour = NULL, alpha = NULL)) +
  guides(alpha = 'none') +
  ggtitle("rbv2_super")


p1


p1 = ggplot(pdf[top_q == TRUE, ], aes(x = params, y = vals, color = -yval)) +
  geom_quasirandom(data = pdf[top_q == TRUE,]) +
  theme_bw() +
  scale_alpha_continuous(range = c(0, .5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_label(data = annot, aes(x = nm, y = yi, label = label, colour = NULL, alpha = NULL)) +
  guides(alpha = 'none') +
  ggtitle("rbv2_super")
p1

p1 = ggplot(pdf[top_q == TRUE, ], aes(x = params, y = vals, color = as.factor(x.batch_method))) +
  geom_quasirandom(data = pdf[top_q == TRUE,], dodge.width = .8) +
  theme_bw() +
  scale_alpha_continuous(range = c(0, .5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_label(data = annot, aes(x = nm, y = yi, label = label, colour = NULL, alpha = NULL)) +
  guides(alpha = 'none') +
  ggtitle("rbv2_super")
p1



p1 = ggplot(pdf[top_q == TRUE, ], aes(x = params, y = vals, color = as.factor(scenario))) +
  geom_quasirandom(data = pdf[top_q == TRUE,], dodge.width = .8) +
  theme_bw() +
  scale_alpha_continuous(range = c(0, .5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_label(data = annot, aes(x = nm, y = yi, label = label, colour = NULL, alpha = NULL)) +
  guides(alpha = 'none') +
  ggtitle("rbv2_super")
p1


p1 = ggplot(pdf[top_q == TRUE, ], aes(x = params, y = vals, color = as.factor(scenario), alpha = -yrank^2)) +
  geom_quasirandom(data = pdf[top_q == TRUE,], dodge.width = .8) +
  theme_bw() +
  scale_alpha_continuous(range = c(0, .5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_label(data = annot, aes(x = nm, y = yi, label = label, colour = NULL, alpha = NULL)) +
  guides(alpha = 'none') +
  ggtitle("rbv2_super")
p1



p1 = ggplot(pdf[top_q == TRUE, ], aes(x = params, y = vals, color = as.factor(scenario))) +
  geom_violin(data = pdf[top_q == TRUE,], trim=TRUE, adjust=.3, scale='width') +
#  geom_point(data = pdf[top_q == TRUE,], position=position_dodge(1)) +
  theme_bw() +
  scale_alpha_continuous(range = c(0, .5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_label(data = annot, aes(x = nm, y = yi, label = label, colour = NULL, alpha = NULL)) +
  guides(alpha = 'none') +
  ggtitle("rbv2_super")
p1



p1 = ggplot(pdf[top_q == TRUE, ], aes(x = params, y = vals, color = as.factor(x.batch_method))) +
  geom_violin(data = pdf[top_q == TRUE,], trim=TRUE, adjust=.3, scale='width') +
#  geom_point(data = pdf[top_q == TRUE,], position=position_dodge(1)) +
  theme_bw() +
  scale_alpha_continuous(range = c(0, .5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_label(data = annot, aes(x = nm, y = yi, label = label, colour = NULL, alpha = NULL)) +
  guides(alpha = 'none') +
  ggtitle("rbv2_super")
p1


p1 = ggplot(pdf[top_q == TRUE, ], aes(x = params, y = vals, color = as.factor(x.batch_method))) +
  geom_point(data = pdf[top_q == TRUE,], position=position_dodge(1)) +
  theme_bw() +
  scale_alpha_continuous(range = c(0, .5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_label(data = annot, aes(x = nm, y = yi, label = label, colour = NULL, alpha = NULL)) +
  guides(alpha = 'none') +
  ggtitle("rbv2_super")


p1
canonical








# discrete: batch_method, filter_algorithm, surrogate_learner
#
