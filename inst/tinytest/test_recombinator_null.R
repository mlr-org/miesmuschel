

using("checkmate")
source("helper_recombinators.R", local = TRUE)

rnull = RecombinatorNull$new()

expect_recombinator(rnull, "RecombinatorNull")

p = ps(ParamLgl = p_lgl(), ParamDbl = p_dbl(0, 1), ParamInt = p_int(0, 1), ParamFct = p_fct(c("a", "b", "c")))
rnull$prime(p)
vals = generate_design_random(p, 10)$data
expect_identical(vals, rnull$operate(vals))
