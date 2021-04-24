
source("setup.R", local = TRUE)

sone = ScalorOne$new()
expect_scalor(sone, "SelectorOne", can_oversample = FALSE)

expect_equal(sone$supported, c("single-crit", "multi-crit"))
expect_equal(sone$packages, character(0))

p = ps(x = p_dbl(0, 1))
data = data.table(x = seq(0, 1, length.out = 4))

sone$prime(p)
expect_equal(sone$operate(data, 1:4), 1:4)
expect_equal(sone$operate(data, matrix(1:4, ncol = 1)), 1:4)
expect_equal(sone$operate(data, matrix(1:8, ncol = 2)), 1:4)
expect_equal(sone$operate(data, matrix(1:8, ncol = 2, byrow = TRUE)), c(1:4) * 2 - 1)

expect_equal(sone$operate(data[3], 3), 3)
expect_equal(sone$operate(data[3], matrix(2:3, ncol = 2)), 2)

sone$param_set$values$objective = 2

expect_error(sone$operate(data, 1:4), "subscript out of bounds")
expect_error(sone$operate(data, matrix(1:4, ncol = 1)), "subscript out of bounds")
expect_error(sone$operate(data[3], 3), "subscript out of bounds")

expect_equal(sone$operate(data, matrix(1:8, ncol = 2)), 5:8)
expect_equal(sone$operate(data, matrix(1:8, ncol = 2, byrow = TRUE)), c(1:4) * 2)


expect_equal(sone$operate(data[3], matrix(2:3, ncol = 2)), 3)

