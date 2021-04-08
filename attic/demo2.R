
# Define the objective to optimize
# The 'budget' here simulates averaging 'b' samples from a noisy function
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

# Get a new OptimInstance. Here we determine that the optimizatoin goes
# for 10 generations.
oi <- OptimInstanceSingleCrit$new(objective,
  search_space = search_space,
  terminator = trm("gens", generations = 10)
)

library("mlr3learners")
# use the 'regr.ranger' as surrogate.
# The following settings have 30 individuals in a batch, the 20 best
# of which survive, while 10 are sampled new.
# For this, 100 individuals are sampled randomly, and the top 10, according
# to the surrogate model, are used.
sumohb_opt <- opt("sumohb", surrogate_learner = mlr3::lrn("regr.ranger"),
  mu = 30, survival_fraction = 2/3,
  filter_rate_first = 100, filter_rate_per_sample = 0
)
# sumohb_opt$optimize performs SumoHB optimization and returns the optimum
sumohb_opt$optimize(oi)

print(oi$archive$data, nrows = 300)


library("ggplot2")
oi$archive$data[, id := sapply(paste(x, y), function(x) substr(digest::digest(x), 1, 5))]
ggplot(oi$archive$data, aes(x = dob, y = Obj, group = id, color = as.numeric(as.factor(id)))) + geom_line() + geom_point()

#####
# Optimizing a Machine Learning Method
#####

# Note that this is a short example, aiming at clarity and short runtime.
# The settings are not optimal for hyperparameter tuning.

library("mlr3")
library("mlr3learners")
library("mlr3tuning")

# The Learner to optimize
learner = lrn("classif.xgboost")

# The hyperparameters to optimize
learner$param_set$values[c("eta", "booster")] = list(to_tune())
learner$param_set$values$nrounds = to_tune(p_int(1, 4, tags = "budget", logscale = TRUE))

# Get a TuningInstance
ti = TuningInstanceSingleCrit$new(
  task = tsk("iris"),
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.acc"),
  terminator = trm("gens", generations = 3)
)

sumohb_tune <- tnr("sumohb", surrogate_learner = lrn("regr.ranger"),
  mu = 20, survival_fraction = 0.5,
  filter_rate_first = 100, filter_rate_per_sample = 0
)
# sumohb_tune$optimize performs SumoHB optimization and returns the optimum
sumohb_tune$optimize(ti)

ti$archive$data

unnest(ti$archive$data[, .(x_domain, dob, eol, classif.acc)], "x_domain")



