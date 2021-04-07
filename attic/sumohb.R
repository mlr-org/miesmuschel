
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
  domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4),
    b = p_int(1, tags = "budget")),
  codomain = ps(Obj = p_dbl(tags = "maximize"))
)

oi <- OptimInstanceSingleCrit$new(objective,
  search_space = objective$domain$search_space(list(x = to_tune(), y = to_tune(), b = to_tune(p_int(1, 32, logscale = TRUE, tags = "budget")))),
  terminator = trm("combo", list(trm("evals", n_evals = 200), trm("gens", generations = 5)))
)


tuner <- OptimizerSumoHB$new()


tuner$surrogate_learner

tuner$optimize(oi)

oi$archive
