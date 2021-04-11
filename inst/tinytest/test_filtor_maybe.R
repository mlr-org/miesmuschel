source("setup.R", local = TRUE)

fm = FiltorMaybe$new(FiltorNull$new())

fm$param_set$values$p = 0.5
fm$param_set$values$random_choice = FALSE

expect_filtor(fm, "FiltorMaybe")

fm$param_set$values$random_choice = TRUE

expect_filtor(fm, "FiltorMaybe", is_primed = TRUE)


fm = FiltorMaybe$new(
  FiltorDebug$new(
    function(v, k, f, n, p) {
      p$step * seq_len(n) - k[[1]][[1]] + v[[1]][[1]]
    },
    function(o, p) p$step * o,
    param_classes = c("ParamLgl", "ParamDbl", "ParamInt"),
    param_set = ps(step = p_int(1))
  ),
  FiltorDebug$new(
    function(v, k, f, n, p) {
      p$step * (seq_len(n) - 1) + k[[1]][[1]] - v[[1]][[1]]
    },
    function(o, p) p$step * o,
    param_classes = c("ParamFct", "ParamDbl", "ParamInt"),
    param_set = ps(step = p_int(1))
  )
)

fm$param_set$values$maybe.step = 4
fm$param_set$values$maybe_not.step = 3

fm$param_set$values$random_choice = FALSE
fm$param_set$values$p = 0.25

# param_classes is set-intersection
expect_set_equal(fm$param_classes, c("ParamDbl", "ParamInt"))

p = ps(x = p_int(-4, 3), y = p_dbl(-10, 10))

known_data = data.frame(x = c(1, 3, 2), y = c(0, 0.1, 0.2))
fitnesses = c(2, 2, 2)
fitnesses_mf = matrix(c(fitnesses, fitnesses), ncol = 2)


fm$prime(p)

# correct part of input vaues is given to filtors
expect_equal(fm$needed_input(4), 13)

expect_equal(fm$operate(data.frame(x = rep(0, 13), y = rep(0, 13)), known_data, fitnesses, 4), c(3, 5, 8, 11))

expect_equal(fm$operate(data.frame(x = rep(c(-1, -2, -3, -4), each = 4), y = rep(0, 16)), known_data, fitnesses, 4), c(2, 7, 10, 13))

# random choice FiltorMaybe: check that all possibilities are sampled and do chi-squared test for distribution
fm$param_set$values$random_choice = TRUE
expect_equal(fm$needed_input(4), 28)

set.seed(1)
fm$param_set$values$p = 0.5

data = data.frame(x = rep(rep(c(-1, -2, 0, -2), 2), each = 4), y = rep(0, 32))

result_options = sapply(list(
  c(2, 5, 8, 11),
  c(2, 7, 10, 13),
  c(2, 6, 9, 12),
  c(2, 6, 10, 15),
  c(2, 6, 10, 14)
), paste, collapse = ",")

results = replicate(200, paste(fm$operate(data, known_data, fitnesses, 4), collapse = ","))
expect_set_equal(results, result_options)

counts = table(factor(results, levels = result_options))
expected = dbinom(0:4, 4, 0.5)

expect_true(chisq.test(counts, p = expected)$p.value > 0.05)

# needed_input does not consider too low probabilities
fm$param_set$values$p = 0.01
expect_equal(fm$needed_input(1000),
  qbinom(-20, 1000, .01, log.p = TRUE, lower.tail = FALSE) * 4 + 3000
)
