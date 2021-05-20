

x <- c(1e-11, .001, .01, .1, .4, .8)




qq <- QuantileInfo(deltafn = function(q) q^2, quantile.min = .5, alpha = .01)

# qq <- QuantileInfo(deltafn = function(q) 0.01, quantile.min = .5, alpha = (1e-1))

debugonce(updateQI)

qq <- updateQI(qq, runif(1e6))


qq <- QuantileInfo(deltafn = function(q) min(0.1 * q, 1e-3), quantile.min = .5)
qq <- updateQI(qq, runif(1e8))
qq
nrow(qq$quantile.map)

qq <- QuantileInfo(deltafn = function(q) min(0.1 * q, 1e-3), quantile.min = .5)
qq <- updateQI(qq, runif(1e9))
qq
nrow(qq$quantile.map)

qq <- QuantileInfo(deltafn = function(q) min(0.1 * q^2, 1e-3), quantile.min = .5)
qq <- updateQI(qq, runif(1e8))
qq
nrow(qq$quantile.map)

qq <- QuantileInfo(deltafn = function(q) min(0.1 * q^2, 1e-3), quantile.min = .5)
qq <- updateQI(qq, runif(1e9))
qq
nrow(qq$quantile.map)


qq <- QuantileInfo(deltafn = function(q) min(q^2, 1e-3), quantile.min = .5)
qq <- updateQI(qq, runif(1e8))
qq
nrow(qq$quantile.map)


qq <- QuantileInfo(deltafn = function(q) min(q^2, 1e-3), quantile.min = .5)
qq <- updateQI(qq, runif(1e9))
qq
nrow(qq$quantile.map)

qq <- QuantileInfo(deltafn = function(q) min(q^2, 1e-3), quantile.min = .5)
for (i in 1:6) qq <- updateQI(qq, runif(1e9))
qq
nrow(qq$quantile.map)



qq <- QuantileInfo(deltafn = function(q) min(0.1 * q, 1e-3), quantile.min = .5)
for (i in 1:6) qq <- updateQI(qq, runif(1e9))
qq
nrow(qq$quantile.map)



nrow(qq$quantile.map)
first(qq$quantile.map)
last(qq$quantile.map)

qq$quantile.map

qq$deltafn(first(qq$quantile.map))
log10(length(qq$empiricals))

qq


for (i in 1:1000) qq <- updateQI(qq, runif(1e6))




options(warn = 2)
options(error=recover)


qq$quantile.map[, quantile(abs(val - q), c(.5, .9, .99))]

qq$quantile.map[, quantile(abs(1 / val - 1 / q), c(.5, .9, .99, 1))]




nrow(qq$quantile.map)



last(qq$quantile.map$q) - qq$deltafn(last(qq$quantile.map$q))


qq <- QuantileInfo(deltafn = function(q) 0.1 * q^2, quantile.min = .5)

qq <- QuantileInfo(deltafn = function(q) 0.1 * q, quantile.min = .5)

qq <- updateQI(qq, runif(1e6))
qq

x <- seq(1e-6, .5, by = 1e-6)
diffs <- log(toQuantile(x, qq)) - log(x)

diffs <- 1 / toQuantile(x, qq) - 1 / x

quantile(abs(diffs), c(.5, .9, .99, 1))

plot(log10(abs(diffs / log(x))))



diffs[1:1000]


qq$quantile.map[J(x), roll = -Inf]
qq$quantile.map[J(x), roll = Inf]

qq$quantile.map
qq$empiricals



qq <- QuantileInfo(deltafn = function(q) 0.1^10 * q, quantile.min = .5)
for (i in seq_len(100)) qq <- updateQI(qq, round(runif(1e5, 0, 10)))

sum(duplicated(qq$empiricals$empiricals))

plot(qq$quantile.map$q)

plot(qq$quantile.map)
abline(v = 0.5)
abline(v = 0.4)
abline(v = 0.3)
abline(v = 0.2)
abline(v = 0.1)
abline(v = 0.1)


plot(sapply(1:5, toQuantile, qq) - (1:5) / 10)
toQuantile(3, qq)
toQuantile(4, qq)



qq$quantile.map


debugonce(updateQI)
plot(updateQI(updateQI(qq, round(runif(1e4, 0, 10))), round(runif(1e4, 0, 10)))$quantile.map$q)
