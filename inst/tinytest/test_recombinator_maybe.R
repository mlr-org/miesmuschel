
source("setup.R", local = TRUE)

## general tests
rmaybe = RecombinatorMaybe$new(RecombinatorCrossoverUniform$new())
rmaybe$param_set$values$p = 0.5
expect_recombinator(rmaybe, "RecombinatorMaybe(RecombinatorCrossoverUniform)")


rmaybe = RecombinatorMaybe$new(RecombinatorCrossoverUniform$new(), RecombinatorCrossoverUniform$new())
rmaybe$param_set$values$p = 0.5
expect_recombinator(rmaybe, "RecombinatorMaybe(RecombinatorCrossoverUniform)")

set.seed(1)

rmaybe = RecombinatorMaybe$new(RecombinatorCrossoverUniform$new())
p = ParamInt$new("x", 0, 1)$rep(100)
rmaybe$prime(p)

if (at_home()) {
  data = as.data.table(matrix(c(0, 1), nrow = 100, ncol = 100))
  colnames(data) = p$ids()

  rmaybe$param_set$values$p = 1
  recombined = rmaybe$operate(data)
  rs = rowSums(recombined)
  expect_true(all(rs > 20 & rs < 80))
  expect_true(all(rs[(0:49) * 2 + 1] + rs[(0:49) * 2 + 2] == 100))

  rmaybe$param_set$values$p = 0
  recombined = rmaybe$operate(data)
  expect_equal(recombined, data)


  rmaybe$param_set$values$p = 0.5
  recombined = rmaybe$operate(data)
  rs = rowSums(recombined)
  expect_true(mean(rs > 20 & rs < 80) > .4)
  expect_true(mean(rs == 0) > .2)
  expect_true(mean(rs == 100) > .2)
  expect_true(all(rs[(0:49) * 2 + 1] + rs[(0:49) * 2 + 2] == 100))


  rmaybe = RecombinatorMaybe$new(RecombinatorCrossoverUniform$new(), RecombinatorCrossoverUniform$new())
  rmaybe$param_set$values$p = 0.5
  rmaybe$param_set$values$maybe.p = 0.5
  rmaybe$param_set$values$maybe_not.p = 0.9
  rmaybe$prime(p)
  recombined = rmaybe$operate(data)
  rs = rowSums(recombined)
  expect_true(mean(rs > 30 & rs < 70) > .4)
  expect_true(mean(rs > 0) > .2)
  expect_true(mean(rs < 30) > .2)
  expect_true(mean(rs > 70) > .2)
  expect_true(mean(rs < 100) > .2)
  expect_true(all(rs[(0:49) * 2 + 1] + rs[(0:49) * 2 + 2] == 100))
}
