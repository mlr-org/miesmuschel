source("setup.R", local = TRUE)

fsp = FiltorSurrogateProgressive$new(mlr3::lrn("regr.featureless"))
expect_filtor(fsp, "FiltorSurrogateProgressive")

fsp = FiltorSurrogateProgressive$new(mlr3::lrn("regr.rpart"))
fsp$param_set$values$filter.pool_factor = 2
fsp$param_set$values$filter.pool_factor_last = 5
expect_filtor(fsp, "FiltorSurrogateProgressive")
expect_read_only(fsp, "surrogate_learner")
expect_read_only(fsp, "surrogate_selector")

expect_equal(fsp$needed_input(1), 2)
expect_equal(fsp$needed_input(2), 10)
expect_equal(fsp$needed_input(3), 15)


library("mlr3learners")
fsp = FiltorSurrogateProgressive$new(mlr3::lrn("regr.lm"))

p = ps(x = p_dbl(-10, 10))
fsp$prime(p)

fsp$param_set$values$filter.pool_factor = 2
fsp$param_set$values$filter.pool_factor_last = 6


known_data = data.frame(x = c(1, -1))
fitnesses = c(1, 2)

data = data.table(x = c(1, 2, -1, -2, 10, -10, 8, -8))
expect_equal(fsp$operate(data, known_data, fitnesses, 0), integer(0))
expect_equal(fsp$operate(data, known_data, fitnesses, 1), 1)
expect_error(fsp$operate(data, known_data, fitnesses, 2), "Needs at least 12 individuals.*got 8")
expect_equal(fsp$operate(rbind(data, data.table(x = rep(9, 4))), known_data, fitnesses, 2), c(4, 6))
expect_equal(fsp$operate(rbind(data, data.table(x = rep(9, 10))), known_data, fitnesses, 3), c(6, 8, 4))

data = data.table(x = c(1, 2, -1, -2, 10, -10, 8, -8, 0, 3, -3, 4, 5, 6, -6, -5, -4, -9))
expect_equal(fsp$operate(data, known_data, fitnesses, 0), integer(0))

# increasing pool_factor: 2 .. 6
expect_equal(fsp$operate(data, known_data, fitnesses, 1), 1)  # select from 1:2
expect_equal(fsp$operate(data.table(x = c(2, 1)), known_data, fitnesses, 1), 2)  # select from 1:2
expect_equal(fsp$operate(data.table(x = c(2, 1, 0)), known_data, fitnesses, 1), 2)  # select from 1:2

expect_equal(fsp$operate(data, known_data, fitnesses, 2), c(4, 6))  # select from 1:4, then 1:12
expect_equal(fsp$operate(data.table(x = seq(10, -10)), known_data, fitnesses, 2), c(4, 12))  # select from 1:4, then 1:12
expect_equal(fsp$operate(data.table(x = seq(-10, 10)), known_data, fitnesses, 2), c(1, 2))  # select from 1:4, then 1:12

expect_equal(fsp$operate(data, known_data, fitnesses, 3), c(6, 8, 18))  # select from 1:6, 1:10, 1:18
expect_equal(fsp$operate(data.table(x = seq(-10, 10)), known_data, fitnesses, 3), c(1, 2, 3))  # select from 1:6, 1:10, 1:18
expect_equal(fsp$operate(data.table(x = seq(10, -10)), known_data, fitnesses, 3), c(6, 10, 18))  # select from 1:6, 1:10, 1:18

expect_equal(fsp$operate(data, known_data, fitnesses, 0), integer(0))
expect_equal(fsp$operate(data, known_data, fitnesses, 1), 1)
expect_equal(fsp$operate(data, known_data, fitnesses, 2), c(4, 6))
expect_equal(fsp$operate(data, known_data, fitnesses, 3), c(6, 8, 18))

expect_equal(fsp$operate(data, known_data, -fitnesses, 0), integer(0))
expect_equal(fsp$operate(data, known_data, -fitnesses, 1), 2)
expect_equal(fsp$operate(data, known_data, -fitnesses, 2), c(2, 5))
expect_equal(fsp$operate(data, known_data, -fitnesses, 3), c(5, 7, 14))

expect_equal(fsp$operate(-data, known_data, fitnesses, 0), integer(0))
expect_equal(fsp$operate(-data, known_data, fitnesses, 1), 2)
expect_equal(fsp$operate(-data, known_data, fitnesses, 2), c(2, 5))
expect_equal(fsp$operate(-data, known_data, fitnesses, 3), c(5, 7, 14))

# decreasing pool_factor: 6 .. 2
fsp$param_set$values$filter.pool_factor_last = 2
fsp$param_set$values$filter.pool_factor = 6
## fitnesses = -fitnesses

expect_equal(fsp$operate(data, known_data, fitnesses, 1), 6)  # select from 1:6
expect_equal(fsp$operate(data.table(x = 6:1), known_data, fitnesses, 1), 6)  # select from 1:6
expect_equal(fsp$operate(data.table(x = seq(6, -6)), known_data, fitnesses, 1), 6)  # select from 1:6

expect_equal(fsp$operate(data, known_data, fitnesses, 2), c(6, 3))  # select from 1:12, then 1:3 because 1st chosen is > 4
expect_equal(fsp$operate(data.table(x = seq(10, -10)), known_data, fitnesses, 2), c(12, 3))  # as above
expect_equal(fsp$operate(data.table(x = seq(-10, 10)), known_data, fitnesses, 2), c(1, 2))  # 1:12, then 1:4
expect_equal(fsp$operate(data.table(x = c(-10, seq(10, -9))), known_data, fitnesses, 2), c(1, 4)) # 1:12, then 1:4

expect_equal(fsp$operate(data, known_data, fitnesses, 3), c(6, 8, 4))  # select 1:18, 1:10, 1:4
expect_equal(fsp$operate(data.table(x = seq(-10, 10)), known_data, fitnesses, 3), c(1, 2, 3))  # select from 1:18, 1:10, 1:6
expect_equal(fsp$operate(data.table(x = seq(10, -10)), known_data, fitnesses, 3), c(18, 9, 4))  # select from 1:8, 1:9, 1:4

fitnesses = -fitnesses

expect_equal(fsp$operate(data, known_data, fitnesses, 0), integer(0))
expect_equal(fsp$operate(data, known_data, fitnesses, 1), 5)
expect_equal(fsp$operate(data, known_data, fitnesses, 2), c(5, 2))
expect_equal(fsp$operate(data, known_data, fitnesses, 3), c(5, 7, 2))


# same pool factor (with default for _last): 2
fsp$param_set$values$filter.pool_factor = 2
fsp$param_set$values$filter.pool_factor_last = NULL

expect_equal(fsp$operate(data, known_data, fitnesses, 0), integer(0))
expect_equal(fsp$operate(data, known_data, fitnesses, 1), 2)
expect_equal(fsp$operate(data, known_data, fitnesses, 2), c(2, 1))
expect_equal(fsp$operate(data, known_data, fitnesses, 3), c(5, 2, 1))

## # fractional filter pools and filter pool underrun



# fitnesses column name collision

pf = ps(fitnesses = p_dbl(-10, 10), .fitnesses = p_dbl(-10, 10))

fsp$prime(pf)

known_data_f = data.frame(fitnesses = c(0, 0, 1), .fitnesses = c(0, 1, 0))
fitnesses_f = c(0, 2, 1)

data = data.frame(fitnesses = c(1, 1, 2, -1, -1, -2), .fitnesses = c(1, 2, 1, -1, -2, -1))

expect_equal(fsp$operate(data, known_data_f, fitnesses_f, 0), integer(0))
expect_equal(fsp$operate(data, known_data_f, fitnesses_f, 1), 2)
expect_equal(fsp$operate(data, known_data_f, fitnesses_f, 2), c(2, 3))


# fractional filter pools and filter pool underrun
fsp$prime(p)

fsp$param_set$values$filter.pool_factor = 5 / 3
expect_equal(fsp$needed_input(1), 2)
expect_equal(fsp$needed_input(3), 5)


fitnesses = -fitnesses

expect_equal(fsp$operate(data.table(x = 6:1), known_data, fitnesses, 1), 2)  # select from 1:2
expect_equal(fsp$operate(data.table(x = 6:1), known_data, fitnesses, 3), c(5, 4, 3))  # select from 1:5, 1:4, 1:3

