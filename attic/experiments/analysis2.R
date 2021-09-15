

library("data.table")

collected <- readRDS("data/collected_2021-09-15_2.rds")

rbindlist(lapply(collected$search_space, function(x) structure(rep(list(1), x$length), names = x$ids())), fill = TRUE, use.names = TRUE)

rbindlist(lapply(collected[curseed != -1, archive], function(x) copy(x)[, seed := curseed]))

collected


origcol <- collected[curseed != -1][, groupid := .GRP, by = c("objective", "mu", "siman", "batchmethod", "infillsearch")]
allarchives <- lapply(seq_len(nrow(collected[curseed != -1])), function(i) {
  if (nrow(origcol$archive[[i]])) cbind(origcol$archive[[i]], origcol[i, .(curseed, objective, mu.setting = mu, siman, infillsearch, batch_method = batchmethod, groupid, runid = i)])
})



aa2 <- rbindlist(allarchives, fill = TRUE)[
    is.na(filter_factor_last), filter_factor_last := filter_factor_first][
    is.na(x.filter_factor_last), x.filter_factor_last := x.filter_factor_first][
    is.na(filter_algorithm), filter_algorithm := "tournament"][
    is.na(x.filter_algorithm), x.filter_algorithm := "tournament"][
    is.na(filter_select_per_tournament), filter_select_per_tournament := 1][
    is.na(x.filter_select_per_tournament), x.filter_select_per_tournament := 1][
    is.na(filter_factor_first.end), filter_factor_first.end := filter_factor_first][
    is.na(x.filter_factor_first.end), x.filter_factor_first.end := x.filter_factor_first][
    is.na(random_interleave_fraction.end), random_interleave_fraction.end := random_interleave_fraction][
    is.na(x.random_interleave_fraction.end), x.random_interleave_fraction.end := x.random_interleave_fraction][
    is.na(filter_factor_last.end), filter_factor_last.end := filter_factor_last][
    is.na(x.filter_factor_last.end), x.filter_factor_last.end := x.filter_factor_last][
    is.na(filter_select_per_tournament.end), filter_select_per_tournament.end := filter_select_per_tournament][
    is.na(x.filter_select_per_tournament.end), x.filter_select_per_tournament.end := x.filter_select_per_tournament][
    is.na(mu), mu := 32][
    is.na(x.mu), x.mu := 32][, x.batch_method := batch_method][]


saveRDS(aa2, "data/combined_2021-09-15_2.rds")




combined <- aa2

combined <- combined[mu.setting == "muvary"]
combined.X <- combined[, gsub("^x\\.", "", grep("^x\\.", colnames(combined), value = TRUE)), with = FALSE]
combined.X.trafod <- combined[, grep("^x\\.", colnames(combined), value = TRUE), with = FALSE]
combined.Y <- combined[, .(yval)]



