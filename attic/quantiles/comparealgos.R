


# get best results from run info.
pickresult <- function(restbl, maxbudget = TRUE) restbl[!maxbudget | budget == max(budget), min(performance)]

alltbl <- rbindlist(lapply(list.files("~/lcbench", full.names = TRUE, pattern = "^[^_]*\\.rds$", recursive = TRUE), readRDS))

alltbl[, resultnum := mapply(pickresult, result, sapply(full_budget, isFALSE))]  # numeric (scalar) result

# quantile info
source("~/miesmuschel/attic/quantileinfo.R")
quantiles <- lapply(1:8, function(i) readRDS(sprintf("~/mfrunner/problem_%i_seed_1.rds", i)))

# mapping task ID to quantileinfo index (quantileinfo files are numbered 1:8)
instances <- readRDS("~/instances.rds")
alltbl[, refnumber := instances[cfg == "lcbench" & !test][, index := seq_len(nrow(.SD))][task, index, on = "level"]]

# empirical result quantile
alltbl[!is.na(refnumber), resultquantile := mapply(function(rn, rf) toQuantile(rn, quantiles[[rf]]), resultnum, refnumber)]

# expected performance of random search
logoffset <- -log2(30 * 9)

alltbl[!is.na(refnumber), .(
  rqm.logmean = logoffset - log2(mean(resultquantile)),
  rqm.logmean.se = log2(sd(resultquantile) / sqrt(length(resultquantile)) / mean(resultquantile) + 1),
  rqm.meanlog = logoffset - mean(log2(resultquantile)), rqm.meanlog.se = sd(log2(resultquantile)) / sqrt(length(resultquantile))
), by = c("algorithm", "full_budget", "eta", "algorithm_type", "task")]



data <- alltbl[full_budget == TRUE & !is.na(refnumber),
  .(i = 1:270,
    q = apply(mapply(function(rn, rf) cummin(toQuantile(rn$performance, quantiles[[rf]])), result, refnumber), 1, mean)),
  by = c("task", "algorithm")]

ggplot(data, aes(x = i, y = -log2(i) - log2(q), color = paste0(task, algorithm))) + geom_point() + theme_bw()
