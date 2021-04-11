
library("paradox")
# library("tinytest")
library("checkmate")
library("data.table")

library("mlr3")

devtools::document()

devtools::load_all()



objective <- ObjectiveRFun$new(
  fun = function(xs) {
    z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
    z <- z + rnorm(1, sd = 1 / sqrt(xs$b))
    list(Obj = z)
  },
  domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4), b = p_int(1)),
  codomain = ps(Obj = p_dbl(tags = "maximize"))
)

search_space = objective$domain$search_space(list(
  x = to_tune(),
  y = to_tune(),
  b = to_tune(p_int(1, 2^10, logscale = TRUE, tags = "budget"))
))

oi <- OptimInstanceSingleCrit$new(objective,
  search_space = search_space,
  terminator = trm("gens", generations = 10)
)


tuner <- OptimizerSumoHB$new()
tuner$param_set$values$survival_fraction = 2/3
tuner$param_set$values$mu = 10

tuner$optimize(oi)

oi$archive$data

unnest(oi$archive$data[, .(x_domain)], "x_domain")

library("ggplot2")
ggplot(oi$archive$data, aes(x = x, y = y, color = dob)) + geom_point()


library("mlr3learners")
tuner_sumo <- OptimizerSumoHB$new(lrn("regr.ranger"))


oi$clear()

tuner_sumo$param_set$values$filter_rate_first = 100
tuner_sumo$param_set$values$filter_rate_per_sample = 100
tuner_sumo$param_set$values$survival_fraction = 2/3
tuner_sumo$param_set$values$mu = 10


tuner_sumo$optimize(oi)

oi$archive$data

unnest(oi$archive$data[, .(x_domain)], "x_domain")

ggplot(oi$archive$data, aes(x = x, y = y, color = dob)) + geom_point()


oi$archive$data[, id := sapply(paste(x, y), function(x) substr(digest::digest(x), 1, 5))]

ggplot(oi$archive$data, aes(x = dob, y = Obj, group = id)) + geom_line() + geom_point()


