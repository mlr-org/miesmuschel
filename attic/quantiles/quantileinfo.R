library("checkmate")
library("data.table")

# deltafn: precision at quantile q
# quantile.min: minimum quantile at which to start accounting
# alpha: error probability
QuantileInfo <- function(deltafn = function(q) 0.01, quantile.min = 1 - deltafn(1), alpha = .01) {
  assertFunction(deltafn)
  assertNumber(quantile.min, lower = 0, upper = 1)
  assertNumber(alpha, lower = 0, upper = 1)
  assertNumber(deltafn(quantile.min), lower = 0, upper = quantile.min)  # deltafn < quantile.min
  if (deltafn(quantile.min) == 0) stop("deltafn must be > 0")
  if (quantile.min == 1) stop("deltafn must be > 0")
  assertNumber(deltafn(quantile.min - deltafn(quantile.min)), lower = 0, upper = deltafn(quantile.min))  # deltafn monotonically increasing



  structure(list(
    deltafn = deltafn,
    quantile.min = quantile.min,
    alpha.half.log = log(alpha / 2),
    quantile.map = data.table(q = numeric(0), val = numeric(0), key = c("val", "q")),
    n = 0,  # number of measurements total
    empiricals = data.table(empiricals = numeric(0), id = integer(0), key = "empiricals")
  ), class = "QuantileInfo")
}

print.QuantileInfo <- function(x, ...) {
  qirange <- if (nrow(x$quantile.map)) {
    sprintf(" (%.3g .. %.3g) +", min(x$quantile.map$q), max(x$quantile.map$q))
  } else {
    ""
  }
  cat(sprintf("QuantileInfo[n = %s]:%s {%s}\n", x$n, qirange, nrow(x$empiricals)))
}

updateQI <- function(qi, empiricals) {
  assertClass(qi, "QuantileInfo")
  assertNumeric(empiricals, any.missing = FALSE)
  if (!length(empiricals)) return(qi)

  qi$n <- qi$n + length(empiricals)
  if (nrow(qi$quantile.map)) {
    empiricals <- empiricals[empiricals <= first(qi$quantile.map$val)]
    current.q <- first(qi$quantile.map$q)
    current.q <- current.q - qi$deltafn(current.q)
  } else {
    current.q <- qi$quantile.min
  }
  empiricals <- data.table(empiricals = c(empiricals, qi$empiricals$empiricals), key = "empiricals")
  nemp <- nrow(empiricals)
  empiricals[, id := seq_len(nemp)]

  if (current.q >= 0) current.idx.upper <- qbinom(qi$alpha.half.log, qi$n, current.q, lower.tail = FALSE, log.p = TRUE) + 1
  quantile.map.extension <- list(qi$quantile.map)
  repeat {
    current.delta <- qi$deltafn(current.q)
    next.q <- current.q - current.delta
    if (current.q < 0) break  # reached the end
    if (next.q < 0) next.q <- 0


    current.idx.lower <- qbinom(qi$alpha.half.log, qi$n, current.q, lower.tail = TRUE, log.p = TRUE)
    current.idx.median <- qbinom(0.5, qi$n, current.q, lower.tail = TRUE)
    next.idx.upper <- qbinom(qi$alpha.half.log, qi$n, next.q, lower.tail = FALSE, log.p = TRUE) + 1
    if (next.idx.upper >= current.idx.lower) break  # indices must not overlap. this implies current.idx.lower > 0.
    current.idx.upper <- current.idx.upper
    current.idx.median <- current.idx.median
    current.idx.lower <- current.idx.lower

    if (current.idx.upper - current.idx.lower > current.delta * (qi$n + 1)) break  # desired accuracy not reached
    if (current.idx.median <= nemp) {
      val = empiricals$empiricals[[current.idx.median]]
    } else {
      # median index already taken by other empirical map -->
      # take the lowest value of the quantile map -- we enforce monotonicity
      val = last(quantile.map.extension)$val[[1]]
    }
    quantile.map.extension[[length(quantile.map.extension) + 1]] <- data.table(q = current.q, val = val)

    current.idx.upper <- next.idx.upper
    current.q <- next.q
  }

  qi$quantile.map <- rbindlist(quantile.map.extension)
  setkeyv(qi$quantile.map, c("val", "q"))
  if (nrow(qi$quantile.map)) {
    qi$empiricals <- empiricals[empiricals <= first(qi$quantile.map$val)]
  } else {
    qi$empiricals <- empiricals
  }
  qi
}

toQuantile <- function(x, qi) {
  assertClass(qi, "QuantileInfo")
  assertNumeric(x)

  qm <- qi$quantile.map[, .(q = mean(q)), keyby = "val"]

  resdt <- cbind(
    lower = qm[J(x), .(q, x.val), roll = Inf, mult = "last", on = "val"],
    upper = qm[J(x), .(q, x.val), roll = -Inf, mult = "first", on = "val"],
    lower = qi$empiricals[J(x), .(id, x.empiricals), roll = Inf, mult = "last"],
    upper = qi$empiricals[J(x), .(id, x.empiricals), roll = -Inf, mult = "first"],
    val = x
  )

  # afaik this currently only breaks if first(quantile.ma$val) and tail(empiricals$empiricals) are tied
  resdt[is.na(upper.q), result := qi$quantile.min]
  resdt[is.na(lower.q) & is.na(lower.x.empiricals), result := 0.5 / (qi$n + 1)]
  resdt[is.na(lower.q) & !is.na(lower.x.empiricals),
    result := ifelse(lower.x.empiricals == upper.x.empiricals,
      (lower.id + upper.id) * 0.5 / (qi$n + 1),  # handle ties by taking average quantile. THis happens when empirical distribution is discrete.
      (lower.id + (val - lower.x.empiricals) / (upper.x.empiricals - lower.x.empiricals)) / (qi$n + 1)
    )
  ]
  resdt[is.na(result), result := ifelse(lower.x.val == upper.x.val,
    lower.q,
    (upper.q * (val - lower.x.val) + lower.q * (upper.x.val - val)) / (upper.x.val - lower.x.val)
  )]
  resdt$result
}
