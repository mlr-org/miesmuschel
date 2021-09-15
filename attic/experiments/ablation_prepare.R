# RQ 1, 2, 3

canonical <- collected[mu == "muvary" & curseed == -1]
lambdas.1 <- canonical[siman & infillsearch == "all", ]$lambda

rq.1.tbl <- rbindlist(lambdas.1)

# RQ 4
rq.4.tbl <- rbindlist(lambdas.1)[, budget_log_step := 100][]

# RQ 5

# | conditional opt
lambdas.2 <- canonical[!siman & infillsearch == "all", ]$lambda
rq.5a.tbl <- rbindlist(lambdas.2)

# | setting values directly
rq.5a.tbl.cond <- rbindlist(lambdas.1)[,
  c("filter_factor_last", "filter_factor_last.end") := sqrt(filter_factor_last.end * filter_factor_last)][,
  c("filter_factor_first", "filter_factor_first.end") := sqrt(filter_factor_first.end * filter_factor_first)][,
  c("filter_select_per_tournament", "filter_select_per_tournament.end") := round((filter_select_per_tournament.end + filter_select_per_tournament) / 2)][,
  c("random_interleave_fraction", "random_interleave_fraction.end") := (random_interleave_fraction + random_interleave_fraction.end) / 2][]

# RQ 5b
# | conditional opt
lambdas.3 <- canonical[!siman & infillsearch == "rs", ]$lambda
rq.5b.tbl <- rbindlist(lambdas.3)

rq.5b.tbl.cond <- copy(rq.5a.tbl.cond)[,
  c("filter_factor_first", "filter_factor_last", "filter_factor_first.end", "filter_factor_last.end") :=
    sqrt(filter_factor_first * filter_factor_last)][filter_algorithm != "tournament", filter_select_per_tournament := 1][,
  filter_algorithm := "tournament"][]

# RQ 6
t1 <- rbindlist(canonical[siman & infillsearch == "all" & batchmethod == "hb"]$lambda)[,
  original_surrogate_learner := surrogate_learner][,
  surrogate_learner := NULL][, id := 1:2]
t2 <- CJ(id = 1:2, surrogate_learner = c("knn1", "knn7", "bohblrn", "ranger"))

surgrid <- t1[t2, on = "id"][, id := NULL][surrogate_learner != original_surrogate_learner][, original_surrogate_learner := NULL][]
rigrid <- rbind(
  rbindlist(canonical[siman & infillsearch == "all" & batchmethod == "hb"]$lambda)[,
    c("random_interleave_fraction", "random_interleave_fraction.end") := 1],
  rbindlist(canonical[siman & infillsearch == "all" & batchmethod == "hb"]$lambda)[,
    c("random_interleave_fraction", "random_interleave_fraction.end") := 0]
)

rq.6.tbl <- rbind(surgrid, rigrid, use.names = TRUE)

# RQ 7
# special:
# budgetfactor: 8 * 30 == 240
rq.7.tbl.BUDGETFACTOR <- rbindlist(canonical[siman & infillsearch == "all" & batchmethod == "hb"]$lambda)[, mu := 32]



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
