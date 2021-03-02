
source("setup.R", local = TRUE)

oibigmax = as_oi(get_objective_passthrough("maximize", FALSE, "bud"))
oibigmultiboth = as_oi(get_objective_passthrough(c("minimize", "maximize"), FALSE, "bud"))

design = cbind(generate_design_random(oibigmax$search_space, 9)$data[, p1 := c(1, 1, 1, 3, 3, 7, 5, 5, 9)],
  data.table(additional = 1:9, dob = rep(1:3, each = 3), eol = rep(c(3, NA, NA), 3))
)

designmultiobj = cbind(generate_design_random(oibigmultiboth$search_space, 9)$data[, bud := c(1, 1, 1, 3, 3, 7, 5, 5, 9)],
  data.table(additional = 1:9, dob = rep(1:3, each = 3), eol = rep(c(3, NA, NA), 3))
)

sb = sel("best")$prime(oibigmax$search_space$clone(deep = TRUE)$add(ps(additional = p_int(1, 9))))
sr = sel("random")$prime(oibigmultiboth$search_space$clone(deep = TRUE)$add(ps(additional = p_int(1, 9))))

oibigmax$eval_batch(copy(design))

oibigmultiboth$eval_batch(copy(designmultiobj))


archive_before = copy(oibigmax$archive$data)

# killing two individuals
expect_equal(mies_survival_plus(oibigmax, 4, sb), oibigmax$archive$data)
expect_equal(oibigmax$archive$data, archive_before[2:3, eol := 3])

# no individuals die
expect_equal(mies_survival_plus(oibigmax, 4, sb), oibigmax$archive$data)
expect_equal(oibigmax$archive$data, archive_before)

# fewer alive than mu
expect_equal(mies_survival_plus(oibigmax, 6, sb), oibigmax$archive$data)
expect_equal(oibigmax$archive$data, archive_before)

# killing more individuals
expect_equal(mies_survival_plus(oibigmax, 2, sb), oibigmax$archive$data)
expect_equal(oibigmax$archive$data, archive_before[c(5, 8), eol := 3])

expect_equal(mies_survival_plus(oibigmax, 1, sb), oibigmax$archive$data)
expect_equal(oibigmax$archive$data, archive_before[6, eol := 3])

# multiobjective

archive_before = copy(oibigmultiboth$archive$data)

# killing two individuals
expect_equal(mies_survival_plus(oibigmultiboth, 4, sr), oibigmultiboth$archive$data)
expect_equal(sum(is.na(oibigmultiboth$archive$data$eol)), 4)
expect_equal(oibigmultiboth$archive$data, archive_before[, eol := oibigmultiboth$archive$data$eol])

# no individuals die
expect_equal(mies_survival_plus(oibigmultiboth, 4, sr), oibigmultiboth$archive$data)
expect_equal(oibigmultiboth$archive$data, archive_before)

# fewer alive than mu
expect_equal(mies_survival_plus(oibigmultiboth, 6, sr), oibigmultiboth$archive$data)
expect_equal(oibigmultiboth$archive$data, archive_before)


# killing more individuals
expect_equal(mies_survival_plus(oibigmultiboth, 2, sr), oibigmultiboth$archive$data)
expect_equal(sum(is.na(oibigmultiboth$archive$data$eol)), 2)
expect_equal(oibigmultiboth$archive$data, archive_before[, eol := oibigmultiboth$archive$data$eol])

expect_equal(mies_survival_plus(oibigmultiboth, 1, sr), oibigmultiboth$archive$data)
expect_equal(sum(is.na(oibigmultiboth$archive$data$eol)), 1)
expect_equal(oibigmultiboth$archive$data, archive_before[, eol := oibigmultiboth$archive$data$eol])

# fill in coverage
oibigmax$clear()
oibigmax$eval_batch(copy(design)[, eol := NULL])
expect_error(mies_survival_plus(oibigmax, 1, sb), "No alive individuals. Need to run mies_init_population")
