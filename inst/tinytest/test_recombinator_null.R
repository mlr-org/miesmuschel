
source("setup.R", local = TRUE)

rnull = RecombinatorNull$new()

expect_recombinator(rnull, "RecombinatorNull")

p = ps(ParamLgl = p_lgl(), ParamDbl = p_dbl(0, 1), ParamInt = p_int(0, 1), ParamFct = p_fct(c("a", "b", "c")))
rnull$prime(p)
vals = generate_design_random(p, 10)$data
expect_identical(vals, rnull$operate(vals))
expect_equal(rnull$n_indivs_in, 1)
expect_equal(rnull$n_indivs_out, 1)

rnull2 = RecombinatorNull$new(n_indivs_in = 2)
rnull2$prime(p)
vals = generate_design_random(p, 10)$data
expect_identical(vals, rnull2$operate(vals))

vals = generate_design_random(p, 9)$data
expect_error(rnull2$operate(vals), "%%.* == 0.*Must be TRUE")

expect_equal(rnull2$n_indivs_in, 2)
expect_equal(rnull2$n_indivs_out, 2)

rnull21 = RecombinatorNull$new(n_indivs_in = 2, n_indivs_out = 1)
rnull21$prime(p)
vals = generate_design_random(p, 10)$data
expect_identical(vals[(1:5) * 2 - 1], rnull21$operate(vals))

expect_equal(rnull21$n_indivs_in, 2)
expect_equal(rnull21$n_indivs_out, 1)
