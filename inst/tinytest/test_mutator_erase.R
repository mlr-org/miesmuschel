
source("setup.R", local = TRUE)

merase = MutatorErase$new()

expect_mutator(merase, "MutatorErase")

set.seed(1)

p = ps(x = p_lgl(), y = p_fct(c("TRUE", "FALSE")), a = p_dbl(-1, 1), b = p_int(0, 4))
indata = data.table(x = rep(TRUE, 1000), y = rep("FALSE", 1000), a = seq(-1, 0, length.out = 1000), b = rep(0:3, 250))
merase$prime(p)
operated = merase$operate(indata)
expect_true(mean(operated$x) > .45)
expect_true(mean(operated$x) < .55)
expect_true(mean(operated$y == "FALSE") > .45)
expect_true(mean(operated$y == "FALSE") < .55)
expect_true(mean(operated$a < 0) > .45)
expect_true(mean(operated$a < 0) < .55)
expect_true(mean(operated$b == 4) > .15)
expect_true(mean(operated$b == 4) < .25)

expect_true(cor.test(as.numeric(operated$x), as.numeric(as.logical(operated$y)))$p.value > .05)

# works with mutator component-wise maybe
merasemaybe = MutatorCmpMaybe$new(munif)
merasemaybe$param_set$values$p = .5
merasemaybe$prime(p)
operated = merasemaybe$operate(indata)

expect_true(mean(operated$x) > .7)
expect_true(mean(operated$x) < .8)
expect_true(mean(operated$y == "FALSE") > .7)
expect_true(mean(operated$y == "FALSE") < .8)
expect_true(mean(operated$a < 0) > .7)
expect_true(mean(operated$a < 0) < .8)
expect_true(mean(operated$b == 4) > .075)
expect_true(mean(operated$b == 4) < .125)




merase$param_set$values$initializer = generate_design_random_increasing

p = ps(x = p_lgl(), y = p_fct(c("TRUE", "FALSE")), p1 = p_dbl(0, 10))
merase$prime(p)
expect_equal(merase$operate(data.table(x = rep(TRUE, 10), y = rep("FALSE", 10), p1 = rep(0, 10)))$p1, 1:10)
