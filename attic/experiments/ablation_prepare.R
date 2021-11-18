# RQ 1, 2, 3

canonical <- collected[mu == "muvary" & curseed == -1]

metainfo.1 <-canonical[siman & infillsearch == "all", .(objective, batchmethod, lambda)]
lambdas.1 <- metainfo.1$lambda

rq.1.tbl <- rbindlist(lambdas.1)

metainfo.1[, `:=`(lambda = NULL, rq = "rq1")]

# RQ 4
rq.4.tbl <- rbindlist(lambdas.1)[, budget_log_step := 100][]

metainfo.4 <- copy(metainfo.1)[, rq := "rq4"]

# RQ 5

# | conditional opt
lambdas.2 <- canonical[!siman & infillsearch == "all", ]$lambda
rq.5a.tbl <- rbindlist(lambdas.2)

metainfo.5a <- canonical[!siman & infillsearch == "all", .(objective, batchmethod, rq = "rq5a")]

# | setting values directly
rq.5a.tbl.cond <- rbindlist(lambdas.1)[,
  c("filter_factor_last", "filter_factor_last.end") := sqrt(filter_factor_last.end * filter_factor_last)][,
  c("filter_factor_first", "filter_factor_first.end") := sqrt(filter_factor_first.end * filter_factor_first)][,
  c("filter_select_per_tournament", "filter_select_per_tournament.end") := round(sqrt(filter_select_per_tournament.end * filter_select_per_tournament))][,
  c("random_interleave_fraction", "random_interleave_fraction.end") := (random_interleave_fraction + random_interleave_fraction.end) / 2][]

metainfo.5a.cond <- copy(metainfo.1)[, rq := "rq5a_cond"]


# RQ 5b
# | conditional opt
lambdas.3 <- canonical[!siman & infillsearch == "rs", ]$lambda
rq.5b.tbl <- rbindlist(lambdas.3)

metainfo.5b <- canonical[!siman & infillsearch == "rs", .(objective, batchmethod, rq = "rq5b")]

rq.5b.tbl.cond <- copy(rq.5a.tbl.cond)[,
  c("filter_factor_first", "filter_factor_last", "filter_factor_first.end", "filter_factor_last.end") :=
    sqrt(filter_factor_first * filter_factor_last)][filter_algorithm != "tournament", filter_select_per_tournament := 1][,
  filter_algorithm := "tournament"][]

metainfo.5b.cond <- copy(metainfo.5a.cond)[, rq := "rq5b_cond"]


# RQ 6
t1 <- rbindlist(canonical[siman & infillsearch == "all" & batchmethod == "smashy"]$lambda)[,
  original_surrogate_learner := surrogate_learner][,
  surrogate_learner := NULL][, id := 1:2]
t2 <- CJ(id = 1:2, surrogate_learner = c("knn1", "knn7", "bohblrn", "ranger"))


metainfo.6.surgrid <- canonical[siman & infillsearch == "all" & batchmethod == "smashy", .(objective, batchmethod, id = 1:2)]

surgrid <- t1[t2, on = "id"][surrogate_learner != original_surrogate_learner]

metainfo.6.surgrid <- metainfo.6.surgrid[surgrid[, .(id, surrogate_learner)], on = "id"][, id := NULL]

surgrid[, id := NULL][, original_surrogate_learner := NULL][]
rigrid <- rbind(
  rbindlist(canonical[siman & infillsearch == "all" & batchmethod == "smashy"]$lambda)[,
    c("random_interleave_fraction", "random_interleave_fraction.end") := 1][,
      sample := "random"],
  rbindlist(canonical[siman & infillsearch == "all" & batchmethod == "smashy"]$lambda)[,
    c("random_interleave_fraction", "random_interleave_fraction.end") := 1],
  rbindlist(canonical[siman & infillsearch == "all" & batchmethod == "smashy"]$lambda)[,
    c("random_interleave_fraction", "random_interleave_fraction.end") := 0]
)


rq.6.tbl <- rbind(surgrid, rigrid, use.names = TRUE)

metainfo.6 <- rbind(
  metainfo.6.surgrid,
  canonical[siman & infillsearch == "all" & batchmethod == "smashy", .(objective, batchmethod, surrogate_learner = "NONE")],
  canonical[siman & infillsearch == "all" & batchmethod == "smashy", .(objective, batchmethod, surrogate_learner = "KDE_ONLY")],
  canonical[siman & infillsearch == "all" & batchmethod == "smashy", .(objective, batchmethod, surrogate_learner = "FULL")]
)

metainfo.6[, rq := ifelse(surrogate_learner == "NONE", "rq6_fix", "rq6")]


# RQ 7
# special:
# budgetfactor: 8 * 30 == 240
rq.7.tbl.BUDGETFACTOR <- rbindlist(canonical[siman & infillsearch == "all" & batchmethod == "smashy"]$lambda)[, mu := 32]
metainfo.7 <- canonical[siman & infillsearch == "all" & batchmethod == "smashy", .(objective, batchmethod, rq = "rq7", parallel = TRUE)]


rq.1.tbl
rq.4.tbl
rq.5a.tbl
rq.5a.tbl.cond
rq.5b.tbl
rq.5b.tbl.cond
rq.6.tbl
rq.7.tbl.BUDGETFACTOR

ALL <- rbind(
  rq.1.tbl,
  rq.4.tbl,
  rq.5a.tbl,
  rq.5a.tbl.cond,
  rq.5b.tbl,
  rq.5b.tbl.cond,
  rq.6.tbl, fill = TRUE
)  # warning: NAs
rq.7.tbl.BUDGETFACTOR

nrow(ALL)
nrow(rq.7.tbl.BUDGETFACTOR)

metainfo.all <- rbind(
  metainfo.1,
  metainfo.4,
  metainfo.5a,
  metainfo.5a.cond,
  metainfo.5b,
  metainfo.5b.cond,
  metainfo.6,
  metainfo.7, fill = TRUE
)

metainfo.all[is.na(parallel), parallel := FALSE]
metainfo.all




relevantcollected <- collected[mu == "muvary" & (infillsearch == "all" | !siman),
  cbind(.SD, rbindlist(lapply(lambda, function(x) if (length(x)) x else list(NA)), fill = TRUE))]

relevantcollected[
    is.na(filter_factor_last), filter_factor_last := filter_factor_first][
    is.na(filter_algorithm), filter_algorithm := "tournament"][
    is.na(filter_select_per_tournament), filter_select_per_tournament := 1][
    is.na(filter_factor_first.end), filter_factor_first.end := filter_factor_first][
    is.na(random_interleave_fraction.end), random_interleave_fraction.end := random_interleave_fraction][
    is.na(filter_factor_last.end), filter_factor_last.end := filter_factor_last][
    is.na(filter_select_per_tournament.end), filter_select_per_tournament.end := filter_select_per_tournament]


relevantcollected[, eta_fid := exp(budget_log_step)]
relevantcollected[, eta_surv := 1 / survival_fraction]


textpart <- relevantcollected[,
  .(curseed, objective, siman, batchmethod, infillsearch,
    `\\textit{batch\\_method}` = ifelse(batchmethod == "hb", "\\texttt{HB}", "\\texttt{equal}"),
    `\\textit{filter\\_method}` = ifelse(is.na(filter_algorithm) | filter_algorithm == "tournament", "\\textsc{Trn}", "\\textsc{Prog}"),
    `$\\P_{\\lambdav}(\\archive)$` = ifelse(sample == "bohb", "\\texttt{KDE}", "\\texttt{uniform}"),
    `$\\inducer_{f_\\text{surr}}$` = c(knn7 = "\\texttt{KNN7}", knn1 = "\\texttt{KNN1}")[surrogate_learner],
    `\\textit{filter\\_mb}` = as.character(filter_with_max_budget),
    `$\\rho_\\text{random}$` = as.character(random_interleave_random))][,
      lapply(.SD, function(col) sprintf("%s%s", last(col), if (length(unique(col[!is.na(col)])) > 1) "(..)" else "")),
      by = c("objective", "siman", "batchmethod", "infillsearch")]


## numpart <- relevantcollected[, lapply(.SD, function(x) {
##   sprintf("%.3g [%.3g, %.3g]", last(x), min(x, na.rm = TRUE), max(x, na.rm = TRUE))
## }), by = c("objective", "siman", "batchmethod", "infillsearch"), .SDcols = is.numeric]
numpart <- relevantcollected[, lapply(.SD, function(x) {
  x <- round(x, 2)
  sprintf("%.3g [%.3g, %.3g]", last(x), min(x, na.rm = TRUE), max(x, na.rm = TRUE))
}), by = c("objective", "siman", "batchmethod", "infillsearch"), .SDcols = is.numeric]

## numpart <- relevantcollected[, lapply(.SD, function(x) {
##   if (all(x == round(x), na.rm = TRUE)) sprintf("%s [%s, %s]", last(x), min(x, na.rm = TRUE), max(x, na.rm = TRUE)) else sprintf("%s [%s, %s]", format(last(x), digits = 3), format(min(x, na.rm = TRUE), digits = 3), format(max(x, na.rm = TRUE), digits = 3))
## }), by = c("objective", "siman", "batchmethod", "infillsearch"), .SDcols = is.numeric]

allpart <- cbind(numpart, textpart[, -(1:5)])[,
  .(infillsearch, nosiman = !siman, objective, batchmethod, Parameter = "", Scenario = "",# `\\textit{batch\\_method}` = "",
    `$\\mu(1)$` = mu,
#    `\\textit{batch\\_method}`,
    `$\\eta_{\\text{fid}}$` = eta_fid,
    `$\\eta_{\\text{surv}}$` = eta_surv,
    `\\textit{filter\\_method}`,
    `$\\P_{\\lambdav}(\\archive)$`,
    `$\\inducer_{f_\\text{surr}}$`,
    `$n_{\\text{trn}}(0)$` = filter_select_per_tournament,
    `$n_{\\text{trn}}(1)$` = filter_select_per_tournament.end,
    `$N_\\text{s}^0(0)$` = filter_factor_first,
    `$N_\\text{s}^0(1)$` = filter_factor_first.end,
    `$N_\\text{s}^1(0)$` = filter_factor_last,
    `$N_\\text{s}^1(1)$` = filter_factor_last.end,
    `$\\rho(0)$` = random_interleave_fraction,
    `$\\rho(1)$` = random_interleave_fraction.end,
    `\\textit{filter\\_mb}`,
    `$\\rho_\\text{random}$`
  )]
allpart <- allpart[order(infillsearch, nosiman, objective, batchmethod)]



library("knitr")

## allpart[, `:=`(Name = c(sprintf("\\multirow{4}{*}{%s}", c("$\\gamma^*$", "$\\gamma^2$", "$\\gamma^3$")), "")[c(1, 4, 4, 4, 2, 4, 4, 4, 3, 4, 4, 4)], Scenario = ifelse(rep(c(TRUE, FALSE), 6), sprintf("\\multirow{2}{*}{\\textit{%s}}", gsub("_", "\\\\_", objective)), ""), `\\textit{batch\\_method}` = ifelse(batchmethod == "hb", "\\texttt{HB}", "\\texttt{equal}"))]
# kable(allpart[, -(1:4)], format = "latex", escape = FALSE)


allpart[, `:=`(Parameter = rep(c("$\\gamma^*$", "$\\gamma_2$", "$\\gamma_3$"), each = 4), Scenario = gsub("_", "\\\\_", objective))]


allpartx <- allpart[batchmethod == "hb"][, -(1:4)][]

allpartrot <- as.data.table(t(as.matrix(rbind(as.data.table(as.list(colnames(allpartx))), allpartx, use.names = FALSE))))
colnames(allpartrot) <- unlist(allpartrot[1])

kable(allpartrot[-1], format = "latex", escape = FALSE)


allpartx <- allpart[batchmethod != "hb"][, -(1:4)][]

allpartrot <- as.data.table(t(as.matrix(rbind(as.data.table(as.list(colnames(allpartx))), allpartx, use.names = FALSE))))
colnames(allpartrot) <- unlist(allpartrot[1])

kable(allpartrot[-1], format = "latex", escape = FALSE)


