
source("setup.R", local = TRUE)

mnull = MutatorNull$new()

expect_mutator(mnull, "MutatorNull")

p = ps(ParamLgl = p_lgl(), ParamDbl = p_dbl(0, 1), ParamInt = p_int(0, 1), ParamFct = p_fct(c("a", "b", "c")))
mnull$prime(p)
vals = generate_design_random(p, 10)$data
expect_identical(vals, mnull$operate(vals))
