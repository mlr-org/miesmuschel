
using("checkmate")
source("helper_mutators.R", local = TRUE)

munif = MutatorDiscreteUniform$new()

expect_mutator(munif, "MutatorDiscreteUniform")

set.seed(1)

p = ps(x = p_lgl(), y = p_fct(c("TRUE", "FALSE")))

munif$prime(p)

operated = munif$operate(data.table(x = rep(TRUE, 1000), y = rep("FALSE", 100)))
expect_logical(operated$x)
expect_character(operated$y)

expect_true(mean(operated$x) > .7)
expect_true(mean(operated$x) < .8)
expect_true(mean(operated$y == "FALSE") > .7)
expect_true(mean(operated$y == "FALSE") < .8)

munif$param_set$values$p = 1
operated = munif$operate(data.table(x = rep(TRUE, 1000), y = rep("FALSE", 100)))
expect_true(mean(operated$x) > .45)
expect_true(mean(operated$x) < .55)
expect_true(mean(operated$y == "FALSE") > .45)
expect_true(mean(operated$y == "FALSE") < .55)

munif$param_set$values$can_mutate_to_same = FALSE
operated = munif$operate(data.table(x = rep(TRUE, 10), y = rep("FALSE", 10)))
expect_true(all(operated$x == FALSE))
expect_true(all(operated$y == "TRUE"))

munif$param_set$values$p = .5
operated = munif$operate(data.table(x = rep(TRUE, 1000), y = rep("FALSE", 100)))
expect_true(mean(operated$x) > .45)
expect_true(mean(operated$x) < .55)
expect_true(mean(operated$y == "FALSE") > .45)
expect_true(mean(operated$y == "FALSE") < .55)
