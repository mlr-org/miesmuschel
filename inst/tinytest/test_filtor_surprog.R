source("setup.R", local = TRUE)


fsp = FiltorSurrogateProgressive$new(mlr3::lrn("regr.featureless"))
# TODO
## expect_filtor(fsp, "FiltorSurrogateProgressive")

## fsp = FiltorSurrogateProgressive$new(mlr3::lrn("regr.rpart"))
## fsp$param_set$values$filter_pool_first = 10
## fsp$param_set$values$filter_pool_per_sample = 4
## expect_filtor(fsp, "FiltorSurrogateProgressive")
## expect_read_only(fsp, "surrogate_learner")

## expect_equal(fsp$needed_input(1), 10)
## expect_equal(fsp$needed_input(2), 14)
## expect_equal(fsp$needed_input(3), 18)


## library("mlr3learners")
## fsp = FiltorSurrogateProgressive$new(mlr3::lrn("regr.lm"))

## p = ps(x = p_dbl(-10, 10))

## fsp$param_set$values$filter_pool_first = 4
## fsp$param_set$values$filter_pool_per_sample = 2

## fsp$prime(p)

## known_data = data.frame(x = c(1, -1))
## fitnesses = c(1, 2)

## data = data.table(x = c(1, 2, -1, -2, 10, -10, 8, -8))
## expect_equal(fsp$operate(data, known_data, fitnesses, 0), integer(0))
## expect_equal(fsp$operate(data, known_data, fitnesses, 1), 4)
## expect_equal(fsp$operate(data, known_data, fitnesses, 2), c(4, 6))
## expect_equal(fsp$operate(data, known_data, fitnesses, 3), c(4, 6, 8))

## fitnesses = -fitnesses

## expect_equal(fsp$operate(data, known_data, fitnesses, 0), integer(0))
## expect_equal(fsp$operate(data, known_data, fitnesses, 1), 2)
## expect_equal(fsp$operate(data, known_data, fitnesses, 2), c(2, 5))
## expect_equal(fsp$operate(data, known_data, fitnesses, 3), c(2, 5, 7))


## # fitnesses column name collision

## p = ps(fitnesses = p_dbl(-10, 10), .fitnesses = p_dbl(-10, 10))

## fsp$prime(p)

## known_data = data.frame(fitnesses = c(0, 0, 1), .fitnesses = c(0, 1, 0))
## fitnesses = c(0, 2, 1)

## data = data.frame(fitnesses = c(1, 1, 2, -1, -1, -2), .fitnesses = c(1, 2, 1, -1, -2, -1))

## expect_equal(fsp$operate(data, known_data, fitnesses, 0), integer(0))
## expect_equal(fsp$operate(data, known_data, fitnesses, 1), 2)
## expect_equal(fsp$operate(data, known_data, fitnesses, 2), c(2, 3))


## # fractional filter pools and filter pool underrun


## fsp$param_set$values$filter_pool_per_sample = 0.5
## expect_equal(fsp$needed_input(1), 4)
## expect_equal(fsp$needed_input(3), 5)
## expect_equal(fsp$needed_input(5), 6)
## expect_equal(fsp$needed_input(7), 7)
## expect_error(fsp$needed_input(8), "filter_pool_first \\(which is 4\\) \\+ filter_pool_per_sample \\(which is 0\\.5\\) times .* must at least be output_size")


