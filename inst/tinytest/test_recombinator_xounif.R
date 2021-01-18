
using("checkmate")
source("helper_recombinators.R", local = TRUE)

rxounif = RecombinatorCrossoverUniform$new(keep_complement = TRUE)
expect_recombinator(rxounif, "RecombinatorCrossoverUniform(keep_complement = TRUE)")

rxounif = RecombinatorCrossoverUniform$new(keep_complement = FALSE)
expect_recombinator(rxounif, "RecombinatorCrossoverUniform(keep_complement = FALSE)")

set.seed(1)

p = ps(ParamLgl = p_lgl(), ParamDbl = p_dbl(0, 1), ParamInt = p_int(0, 1), ParamFct = p_fct(c("a", "b", "c")))
rxounif$prime(p)
vals = generate_design_random(p, 10)$data

rxounif$param_set$values$p = 0
expect_identical(vals[(0:4) * 2 + 1], rxounif$operate(vals))

rxounif$param_set$values$p = 1
expect_identical(vals[(0:4) * 2 + 2], rxounif$operate(vals))


rxounif = RecombinatorCrossoverUniform$new(keep_complement = TRUE)
rxounif$prime(p)

rxounif$param_set$values$p = 0
expect_identical(vals, rxounif$operate(vals))

rxounif$param_set$values$p = 1
expect_identical(vals[rep((0:4) * 2, each = 2) + c(2, 1)], rxounif$operate(vals))

p = ParamInt$new("x", 0, 1)$rep(100)
rxounif$prime(p)
rxounif$param_set$values$p = 0.5

data = as.data.table(matrix(c(0, 1), nrow = 10, ncol = 100))
colnames(data) = p$ids()
recombined = rxounif$operate(data)

rs = rowSums(recombined)
expect_true(all(rs > 30 & rs < 70))

expect_true(all(rs[(0:4) * 2 + 1] + rs[(0:4) * 2 + 2] == 100))
