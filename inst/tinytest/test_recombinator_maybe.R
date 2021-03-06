
source("setup.R", local = TRUE)

## general tests
rmaybe = RecombinatorMaybe$new(RecombinatorCrossoverUniform$new())
rmaybe$param_set$values$p = 0.5
expect_recombinator(rmaybe, "RecombinatorMaybe(RecombinatorCrossoverUniform)")


rmaybe = RecombinatorMaybe$new(RecombinatorCrossoverUniform$new(), RecombinatorCrossoverUniform$new())
rmaybe$param_set$values$p = 0.5
expect_recombinator(rmaybe, "RecombinatorMaybe(RecombinatorCrossoverUniform)")

set.seed(1)

raddergen = function(nin, nout) {
  RecombinatorDebug$new(function(n, v, p) head(v, nout) + p$x + seq_len(nout),
    c("ParamDbl", "ParamInt"), ps(x = p_int()),
    n_indivs_in = nin, n_indivs_out = nout)
}

# maybe is raddergen, maybe_not is NULL
rmaybe = RecombinatorMaybe$new(raddergen(2, 2))
p = ParamInt$new("x", 0, 10)$rep(3)
rmaybe$prime(p)

data = as.data.table(matrix(c(0, 1), nrow = 40, ncol = 3))
colnames(data) = p$ids()
rmaybe$param_set$values$maybe.x = 2

# p == 1
rmaybe$param_set$values$p = 1
recombined = rmaybe$operate(data)
dataplus = setnames(as.data.table(matrix(c(3, 5), nrow = 40, ncol = 3)), paste0("x_rep_", 1:3))
expect_equal(recombined, dataplus)

# p == 0
rmaybe$param_set$values$p = 0
recombined = rmaybe$operate(data)
expect_equal(recombined, data)

# p == 0.5
rmaybe$param_set$values$p = 0.5
recombined = rmaybe$operate(data)
expect_true(all(recombined == data | recombined == dataplus))
rowequality = apply(recombined == data, 1, all)
expect_equal(rowequality[(1:20) * 2 - 1], rowequality[(1:20) * 2] )
expect_true(mean(rowequality) < .65)
expect_true(mean(rowequality) > .35)

# maybe and maybe_not are raddergen
rmaybe = RecombinatorMaybe$new(raddergen(2, 2), raddergen(2, 2))
rmaybe$param_set$values$p = 0.5
rmaybe$param_set$values$maybe.x = -1
rmaybe$param_set$values$maybe_not.x = 2
rmaybe$prime(p)
recombined = rmaybe$operate(data)
dataminus = setnames(as.data.table(matrix(c(0, 2), nrow = 40, ncol = 3)), paste0("x_rep_", 1:3))
expect_true(all(recombined == dataminus | recombined == dataplus))
rowequality = apply(recombined == dataminus, 1, all)
expect_equal(rowequality[(1:20) * 2 - 1], rowequality[(1:20) * 2] )
expect_true(mean(rowequality) < .65)
expect_true(mean(rowequality) > .35)

# maybe and maybe_not are raddergen, different number of nin/nout

expect_error(RecombinatorMaybe$new(raddergen(2, 2), raddergen(2, 1)), "must have the same number of in / out individuals")
expect_error(RecombinatorMaybe$new(raddergen(2, 2), raddergen(1, 1)), "must have the same number of in / out individuals")

rmaybe = RecombinatorMaybe$new(raddergen(2, 1))
rmaybe$param_set$values$p = 0.5
rmaybe$param_set$values$maybe.x = 1
rmaybe$prime(p)
recombined = rmaybe$operate(data)
expect_true(all(as.matrix(recombined) %in% c(0, 2)))
rowequality = apply(recombined == 0, 1, all)
expect_true(mean(rowequality) < .65)
expect_true(mean(rowequality) > .35)


