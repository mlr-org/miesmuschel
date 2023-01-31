library("paradox")
library("tinytest")
library("checkmate")
library("data.table")



devtools::document()
devtools::load_all()

devtools::run_examples(run_donttest = TRUE)

options(miesmuschel.testing = TRUE)

source("inst/tinytest/setup.R", local = TRUE, chdir = TRUE)


pp = ps(a = p_dbl(0, 10), b = p_dbl(-10, 0))

des <- generate_design_random(pp, 1000)$data
des$a <- 1




plot(mut("gauss", sdev = 1, sdev_is_relative = FALSE, truncated_normal = FALSE)$prime(pp)$operate(des)$a)



pp = ps(a = p_lgl(), b = p_fct(letters[1:4]))
des <- generate_design_random(pp, 10)$data
des$a <- TRUE


mut("unif")$prime(pp)$operate(des)
mut("unif", can_mutate_to_same = FALSE, p = 1)$prime(pp)$operate(des)


rec()



rec("null")$prime(pp)$operate(des)

rec("xounif")$prime(pp)$operate(des)


pp = ps(a = p_lgl(), b = p_fct(letters[1:4]), c = p_dbl(0, 10))
des <- generate_design_random(pp, 10)$data
rec("xounif")$prime(pp)$operate(des)


tinytest::test_all()

tinytest::run_test_file("inst/tinytest/test_dictionaries.R")


tinytest::run_test_file("inst/tinytest/test_selector_proxy.R")      # ...
tinytest::run_test_file("inst/tinytest/test_recombinator_proxy.R")  # ...


tinytest::run_test_file("inst/tinytest/test_filtor_maybe.R")
tinytest::run_test_file("inst/tinytest/test_filtor_null.R")
tinytest::run_test_file("inst/tinytest/test_filtor_proxy.R")
tinytest::run_test_file("inst/tinytest/test_filtor_surprog.R")
tinytest::run_test_file("inst/tinytest/test_mies_evaluate_offspring.R")
tinytest::run_test_file("inst/tinytest/test_mies_filter_offspring.R")
tinytest::run_test_file("inst/tinytest/test_mies_generate_offspring.R")
tinytest::run_test_file("inst/tinytest/test_mies_get_fitnesses.R")
tinytest::run_test_file("inst/tinytest/test_mies_init_population.R")
tinytest::run_test_file("inst/tinytest/test_mies_prime_operators.R")
tinytest::run_test_file("inst/tinytest/test_mies_select_from_archive.R")
tinytest::run_test_file("inst/tinytest/test_mies_step_fidelity.R")
tinytest::run_test_file("inst/tinytest/test_mies_survival_comma.R")
tinytest::run_test_file("inst/tinytest/test_mies_survival_plus.R")
tinytest::run_test_file("inst/tinytest/test_mutator_cmpmaybe.R")
tinytest::run_test_file("inst/tinytest/test_mutator_erase.R")
tinytest::run_test_file("inst/tinytest/test_mutator_gauss.R")
tinytest::run_test_file("inst/tinytest/test_mutator_maybe.R")
tinytest::run_test_file("inst/tinytest/test_mutator_null.R")
tinytest::run_test_file("inst/tinytest/test_mutator_proxy.R")
tinytest::run_test_file("inst/tinytest/test_mutator_unif.R")
tinytest::run_test_file("inst/tinytest/test_operator.R")
tinytest::run_test_file("inst/tinytest/test_operatorcombination.R")
tinytest::run_test_file("inst/tinytest/test_OptimizerMies.R")
tinytest::run_test_file("inst/tinytest/test_ParamSetShadow.R")
tinytest::run_test_file("inst/tinytest/test_recombinator_maybe.R")
tinytest::run_test_file("inst/tinytest/test_recombinator_null.R")
tinytest::run_test_file("inst/tinytest/test_recombinator_proxy.R")
tinytest::run_test_file("inst/tinytest/test_recombinator_sbx.R")
tinytest::run_test_file("inst/tinytest/test_recombinator_xounif.R")
tinytest::run_test_file("inst/tinytest/test_scalor_one.R")
tinytest::run_test_file("inst/tinytest/test_selector_best.R")
tinytest::run_test_file("inst/tinytest/test_selector_proxy.R")
tinytest::run_test_file("inst/tinytest/test_selector_random.R")
tinytest::run_test_file("inst/tinytest/test_shortforms.R")
tinytest::run_test_file("inst/tinytest/test_TerminatorBudget.R")
tinytest::run_test_file("inst/tinytest/test_TerminatorGenerations.R")
tinytest::run_test_file("inst/tinytest/test_TunerMies.R")
tinytest::run_test_file("inst/tinytest/test_utils.R")

objective <- ObjectiveRFun$new(
  fun = function(xs) list(y = xs$x + 10),
  domain = ps(x = p_dbl(-2, 4), z = p_dbl(-2, 4)),
  codomain = ps(y = p_dbl(tags = "maximize"))
)
oi <- OptimInstanceSingleCrit$new(objective, terminator = trm("evals"))

oi$eval_batch(data.table(x = 1, z = 2))

oi$archive$data

oi$archive$data

opt("random_search")$optimize(oi)

covr::report(xx <- covr::package_coverage(type = "tests"))


m0 = mut("null")  # no mutation
msmall = mut("gauss", sdev = 0.1)  # mutates to small value around 0

dd <- mut("sequential", list(mut("gauss", sdev = 0.1), msmall))


dd$prime(ps(x = p_dbl(0, 1)))

sd(dd$operate(data.table(x = rep(0.5, 1000)))$x)^2
1+1

# RecombinatorSequential$new(list(rec("


MutatorSequential$new(list(mut("gauss"))

mut("sequential", list(mut("gauss"), mut("unif")))


mut("sequential", list(
    mut("gauss", sdev = .3),
    mut("gauss", sdev = .4)
))


mut()




p = ps(x = p_dbl(-5, 5))
data = data.frame(x = rep(0, 5))

sa = scl("aggregate", list(
    scl("one", objective = 1),
    scl("one", objective = 2)
))
sa$prime(p)

(fitnesses = matrix(c(1, 5, 2, 3, 0, 3, 1, 0, 10, 8), ncol = 2))

# to see the fitness matrix, use:
## plot(fitnesses, pch = as.character(1:5))

domhv_improvement(fitnesses)

domhv_improvement(fitnesses, fitnesses[1, , drop = FALSE])





# default weight 1 -- sum of both objectives
sa$operate(data, fitnesses)



# only first objective
sa$param_set$values[c("weight_1", "weight_2")] = c(1, 0)
sa$operate(data, fitnesses)

# only 2 * second objective
sa$param_set$values[c("weight_1", "weight_2")] = c(0, 2)
sa$operate(data, fitnesses)


sd$param_set$values$scale_output = FALSE
sd$operate(data, fitnesses)

sd$param_set$values$output = "count_dominated"
# point 4 dominates three other points, point 2 only one other point.
sd$operate(data, fitnesses)


sa = scl("aggregate")
q





library("bbotk")

lgr::threshold("warn")

op.m <- mut("gauss", sdev = 0.1)
op.r <- rec("xounif", p = .3)
op.parent <- sel("random")
op.survival <- sel("best")


# Define the objective to optimize
objective <- ObjectiveRFun$new(
  fun = function(xs) {
    z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2) + rnorm(1, 0, sqrt(1 / xs$budget))
    list(Obj = z)
  },
  domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4), budget = p_dbl(1, 20, tags = "budget")),
  codomain = ps(Obj = p_dbl(tags = "maximize"))
)

# Get a new OptimInstance
oi <- OptimInstanceSingleCrit$new(objective,
  terminator = trm("evals", n_evals = 50)
)

# Create OptimizerMies object
mies_opt <- opt("mies", mutator = op.m, recombinator = op.r,
  parent_selector = op.parent, survival_selector = op.survival,
  multi_fidelity = TRUE,
  fidelity = function(inst, budget_id, last_fidelity, last_fidelity_offspring) if (max(inst$archive$data$dob) < 3) 10 else 20,
  fidelity_offspring = function(inst, budget_id, last_fidelity, last_fidelity_offspring) last_fidelity,
  mu = 10, lambda = 5)

# mies_opt$optimize performs MIES optimization and returns the optimum
mies_opt$optimize(oi)


oi$archive




objective <- ObjectiveRFun$new(
  fun = function(xs) {
    list(y1 = xs$x1, y2 = xs$x2)
  },
  domain = ps(x1 = p_dbl(0, 1), x2 = p_dbl(-1, 0)),
  codomain = ps(y1 = p_dbl(0, 1, tags = "maximize"),
    y2 = p_dbl(-1, 0, tags = "minimize"))
)

oi <- OptimInstanceMultiCrit$new(objective, terminator = trm("none"))

mies_aggregate_single_generation(oi$archive, identity)

mies_aggregate_single_generation(oi$archive, function(fitnesses) fitnesses)


mies_init_population(oi, 2, budget_id = "x1", fidelity = .5)

oi$archive$data

mies_aggregate_single_generation(oi$archive, function(fitnesses) fitnesses)

# notice how fitnesses are positive, since x2 is scaled with -1.
# To get the original objective-values, use objectives_unscaled:
mies_aggregate_single_generation(oi$archive,
  function(objectives_unscaled) objectives_unscaled)

# When `...` is used, all information is passed:
mies_aggregate_single_generation(oi$archive, function(...) names(list(...)))

# generation 10 is not present, but individuals with eol `NA` are still
# considered alive:
mies_aggregate_single_generation(oi$archive, function(fitnesses) fitnesses,
  generation = 10)

# re-evaluating points with higher "fidelity" (x1)
mies_step_fidelity(oi, budget_id = "x1", fidelity = 0.7)

oi$archive$data
# lower-fidelity values are considered dead now, even for generation 1:
mies_aggregate_single_generation(oi$archive, function(fitnesses) fitnesses,
  generation = 1)

# this adds two new alive individuals at generation 2.
# Also the individuals from gen 1 are reevaluated with fidelity 0.8
mies_evaluate_offspring(oi, offspring = data.table(x2 = c(-0.1, -0.2)),
  budget_id = "x1", fidelity = 0.9, reevaluate_fidelity = 0.8)

mies_aggregate_single_generation(oi$archive, function(fitnesses) fitnesses,
  generation = 1)

mies_aggregate_single_generation(oi$archive, function(fitnesses) fitnesses,
  generation = 2)

mies_aggregate_single_generation(oi$archive, function(fitnesses) fitnesses,
  generation = 2, include_previous_generations = TRUE)


# typical use-case: get dominated hypervolume
mies_aggregate_single_generation(oi$archive, function(fitnesses) domhv(fitnesses))

# get generation-wise mean fitness values
mies_aggregate_single_generation(oi$archive, function(fitnesses) {
  apply(fitnesses, 2, mean)
})


set.seed(1)
objective <- ObjectiveRFun$new(
  fun = function(xs) {
    list(y1 = xs$x1, y2 = xs$x2)
  },
  domain = ps(x1 = p_dbl(0, 1), x2 = p_dbl(-1, 0)),
  codomain = ps(y1 = p_dbl(0, 1, tags = "maximize"),
    y2 = p_dbl(-1, 0, tags = "minimize"))
)
oi <- OptimInstanceMultiCrit$new(objective,
  terminator = trm("evals", n_evals = 40))

op <- opt("mies",
  lambda = 4, mu = 4,
  mutator = mut("gauss", sdev = 0.1),
  recombinator = rec("xounif"),
  parent_selector = sel("random"),
  survival_selector = sel("best", scl("hypervolume"))
)

op$optimize(oi)

# Aggregated hypervolume of individuals alive in each gen:
mies_generation_apply(oi$archive, function(fitnesses) {
  domhv(fitnesses)
})

# Aggregated hypervolume of all points evaluated up to each gen
# (may be slightly more, since the domhv of more points is evaluated):
mies_generation_apply(oi$archive, function(fitnesses) {
  domhv(fitnesses)
}, include_previous_generations = TRUE)

# The following are simpler with mies_aggregate_single_generations():
mies_generation_apply(oi$archive, function(fitnesses) {
  apply(fitnesses, 2, mean)
})
# Compare:
mies_aggregate_generations(oi, aggregations = list(mean = mean))

mies_generation_apply(oi$archive, function(objectives_unscaled) {
  apply(objectives_unscaled, 2, mean)
})
# Compare:
mies_aggregate_generations(oi, aggregations = list(mean = mean),
  as_fitnesses = FALSE)





tg <- trm("genstag",
  fitness_aggregator = function(fitnesses) domhv(fitnesses),
  include_previous_generations = TRUE,
  min_delta = 0.1,
  patience = 3
)

set.seed(1)
objective <- ObjectiveRFun$new(
  fun = function(xs) {
    list(y1 = xs$x1, y2 = xs$x2)
  },
  domain = ps(x1 = p_dbl(0, 1), x2 = p_dbl(-1, 0)),
  codomain = ps(y1 = p_dbl(0, 1, tags = "maximize"),
    y2 = p_dbl(-1, 0, tags = "minimize"))
)

oi <- OptimInstanceMultiCrit$new(objective, terminator = tg)

op <- opt("mies",
  lambda = 4, mu = 4,
  mutator = mut("gauss", sdev = 0.1),
  recombinator = rec("xounif"),
  parent_selector = sel("random"),
  survival_selector = sel("best", scl("hypervolume"))
)

op$optimize(oi)

# the observed aggregated values:
oi$archive$data_extra$TerminatorGenerationStagnation

# ... or as calculated by mies_generation_apply
mies_generation_apply(oi$archive, function(fitnesses) {
  domhv(fitnesses)
}, include_previous_generations = TRUE)






set.seed(1)
library("bbotk")
lgr::threshold("warn")

# Terminate when hypervolume with nadir `c(0, 0, ...)`
# does not improve for 3 generations by at least 0.1:
tg <- trm("genperfreached",
  fitness_aggregator = function(fitnesses) domhv(fitnesses),
  include_previous_generations = TRUE,
  level = 1
)

set.seed(1)
objective <- ObjectiveRFun$new(
  fun = function(xs) {
    list(y1 = xs$x1, y2 = xs$x2)
  },
  domain = ps(x1 = p_dbl(0, 1), x2 = p_dbl(-1, 0)),
  codomain = ps(y1 = p_dbl(0, 1, tags = "maximize"),
    y2 = p_dbl(-1, 0, tags = "minimize"))
)

oi <- OptimInstanceMultiCrit$new(objective, terminator = tg)

op <- opt("mies",
  lambda = 4, mu = 4,
  mutator = mut("gauss", sdev = 0.1),
  recombinator = rec("xounif"),
  parent_selector = sel("random"),
  survival_selector = sel("best", scl("hypervolume"))
)

op$optimize(oi)

# the observed aggregated values:
oi$archive$data_extra$TerminatorGenerationPerfReached

# ... or as calculated by mies_generation_apply
mies_generation_apply(oi$archive, function(fitnesses) {
  domhv(fitnesses)
}, include_previous_generations = TRUE)
