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
