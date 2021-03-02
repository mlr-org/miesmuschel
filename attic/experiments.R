
library("paradox")
library("tinytest")
library("checkmate")
library("data.table")

devtools::document()
devtools::load_all()

devtools::run_examples(run_donttest = TRUE)





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

tinytest::run_test_file("inst/tinytest/test_mutator_gauss.R")

tinytest::run_test_file("inst/tinytest/test_selector_best.R")

tinytest::run_test_file("inst/tinytest/test_recombinator_xounif.R")

tinytest::run_test_file("inst/tinytest/test_dictionaries.R")

tinytest::run_test_file("inst/tinytest/test_mies_init_population.R")


tinytest::run_test_file("inst/tinytest/test_mies_survival_comma.R")


objective <- ObjectiveRFun$new(
  fun = function(xs) list(x = xs$x + 10),
  domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4)),
  codomain = ps(x = p_dbl(tags = "maximize"))
)
oi <- OptimInstanceSingleCrit$new(objective, terminator = trm("none"))

oi$eval_batch(data.table(x = 1, y = 2))

oi$archive$data
