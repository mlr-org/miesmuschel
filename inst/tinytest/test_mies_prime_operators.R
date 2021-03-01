
source("setup.R", local = TRUE)

expect_equal(mies_prime_operators(ps()), list(mutators = list(), recombinators = list(), selectors = list()))

ms = muts(c("null", "gauss"))
rs = recs(c("null", "xounif"))
ss = sels(c("best", "random"))

p = ps(x = p_dbl(0, 1), y = p_int(-1, 1))

expect_equal(mies_prime_operators(p, mutators = ms, recombinators = rs, selectors = ss),
  list(mutators = ms, recombinators = rs, selectors = ss))

lapply(c(ms, rs, ss), function(x) expect_equal(x$primed_ps$params, p$params))

expect_error(mies_prime_operators(p, mutators = ms, recombinators = rs, selectors = ss, budget_id = "z"),
  "Must be element.*x.*y.* 'z'")

expect_equal(mies_prime_operators(p, mutators = ms, recombinators = rs, selectors = ss, budget_id = "y"),
  list(mutators = ms, recombinators = rs, selectors = ss))


p_wo_y = ps(x = p_dbl(0, 1))
lapply(ss, function(x) expect_equal(x$primed_ps$params, p$params))
lapply(c(ms, rs), function(x) expect_equal(x$primed_ps$params, p_wo_y$params))

p_ad = ps(z = p_dbl(-1, 0), z2 = p_dbl(-2, 2))

expect_error(mies_prime_operators(p, mutators = ms, recombinators = rs, selectors = ss, additional_components = ps(x = p_dbl(0, 1))),
  "Must have unique names")

expect_equal(mies_prime_operators(p, mutators = ms, recombinators = rs, selectors = ss, additional_components = p_ad),
  list(mutators = ms, recombinators = rs, selectors = ss))

p_all = ps(x = p_dbl(0, 1), y = p_int(-1, 1), z = p_dbl(-1, 0), z2 = p_dbl(-2, 2))

lapply(c(ms, rs, ss), function(x) expect_equal(x$primed_ps$params, p_all$params))

expect_error(mies_prime_operators(p, mutators = ms, recombinators = rs, selectors = ss, additional_components = p_ad, budget_id = "z"),
  "Must be element.*x.*y.* 'z'")

expect_equal(mies_prime_operators(p, mutators = ms, recombinators = rs, selectors = ss, additional_components = p_ad, budget_id = "x"),
  list(mutators = ms, recombinators = rs, selectors = ss))

p_all_wo_x = ps(y = p_int(-1, 1), z = p_dbl(-1, 0), z2 = p_dbl(-2, 2))

lapply(ss, function(x) expect_equal(x$primed_ps$params, p_all$params))
lapply(c(ms, rs), function(x) expect_equal(x$primed_ps$params, p_all_wo_x$params))
