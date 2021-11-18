
source("setup.R", local = TRUE)

oibigmax = as_oi(get_objective_passthrough("maximize", FALSE, "bud"))
oibigmultiboth = as_oi(get_objective_passthrough(c("minimize", "maximize"), FALSE, "bud"))

design = cbind(generate_design_random(oibigmax$search_space, 9)$data[, p1 := c(1, 1, 1, 3, 3, 7, 5, 5, 9)],
  data.table(additional = 1:9, dob = rep(1:3, each = 3), eol = rep(c(3, NA, NA), 3))
)

designmultiobj = cbind(generate_design_random(oibigmultiboth$search_space, 9)$data[, bud := c(1, 1, 1, 3, 3, 7, 5, 5, 9)],
  data.table(additional = 1:9, dob = rep(1:3, each = 3), eol = rep(c(3, NA, NA), 3))
)

p = oibigmax$search_space$clone(deep = TRUE)$add(ps(additional = p_int(1, 9)))

sb = sel("best")$prime(oibigmax$search_space$clone(deep = TRUE)$add(ps(additional = p_int(1, 9))))
sr = sel("random")$prime(oibigmultiboth$search_space$clone(deep = TRUE)$add(ps(additional = p_int(1, 9))))

oibigmax$eval_batch(design)

oibigmultiboth$eval_batch(designmultiobj)


archive_before = copy(oibigmax$archive$data)


# fewer alive than mu, elites
expect_equal(mies_survival_comma(oibigmax, 8, sb, n_elite = 6, elite_selector = sb), oibigmax$archive$data)
expect_equal(oibigmax$archive$data, archive_before)

# same alive as mu, elites
expect_equal(mies_survival_comma(oibigmax, 6, sb, n_elite = 4, elite_selector = sb), oibigmax$archive$data)
expect_equal(oibigmax$archive$data, archive_before)

# killing elites
expect_equal(mies_survival_comma(oibigmax, 4, sb, n_elite = 2, elite_selector = sb), oibigmax$archive$data)
expect_equal(oibigmax$archive$data, archive_before[2:3, eol := 3])

# killing younglings
expect_equal(mies_survival_comma(oibigmax, 3, sb, n_elite = 2, elite_selector = sb), oibigmax$archive$data)
expect_equal(oibigmax$archive$data, archive_before[8, eol := 3])

# no more elites (doesn't need elite_selector)
expect_equal(mies_survival_comma(oibigmax, 3, sb, n_elite = 0), oibigmax$archive$data)
expect_equal(oibigmax$archive$data, archive_before[5:6, eol := 3])

# running when there are no elites
expect_equal(mies_survival_comma(oibigmax, 1, sb, n_elite = 1, elite_selector = sb), oibigmax$archive$data)
expect_equal(oibigmax$archive$data, archive_before)

oibigmax$clear()
oibigmax$eval_batch(design)
archive_before$timestamp = oibigmax$archive$data$timestamp
# no more elites (doesn't need elite_selector), killing non-elites at the same time
expect_equal(mies_survival_comma(oibigmax, 1, sb, n_elite = 0), oibigmax$archive$data)
expect_equal(oibigmax$archive$data, archive_before)

oibigmax$clear()
oibigmax$eval_batch(design)
archive_before = copy(oibigmax$archive$data)
# no more elites (doesn't need elite_selector)
expect_equal(mies_survival_comma(oibigmax, 3, sb, n_elite = 0), oibigmax$archive$data)
expect_equal(oibigmax$archive$data, archive_before[1:6, eol := 3])
# n_elite > number of alive individuals doesn't break things
expect_equal(mies_survival_comma(oibigmax, 3, sb, n_elite = 2), oibigmax$archive$data)
expect_equal(oibigmax$archive$data, archive_before)
# n_elite > number of alive individuals doesn't break things
expect_equal(mies_survival_comma(oibigmax, 3, sb, n_elite = 2), oibigmax$archive$data)
expect_equal(oibigmax$archive$data, archive_before)


# kill all individuals

expect_equal(mies_survival_comma(oibigmax, 0, n_elite = 0, elite_selector = sb), oibigmax$archive$data)
expect_equal(oibigmax$archive$data, archive_before[c(8:9), eol := 3])

expect_error(mies_survival_comma(oibigmax, 0, n_elite = 0, elite_selector = sb), "No alive individuals")


oibigmax$clear()
oibigmax$eval_batch(design)
archive_before = copy(oibigmax$archive$data)

# kill non-elites; doesn't need non-elite selector
expect_equal(mies_survival_comma(oibigmax, 2, n_elite = 2, elite_selector = sb), oibigmax$archive$data)
expect_equal(oibigmax$archive$data, archive_before[c(2:3, 8:9), eol := 3])




# multi-objective
archive_before = copy(oibigmultiboth$archive$data)
# fewer alive than mu, elites
expect_equal(mies_survival_comma(oibigmultiboth, 8, sr, n_elite = 6, elite_selector = sr), oibigmultiboth$archive$data)
expect_equal(oibigmultiboth$archive$data, archive_before)

# same alive as mu, elites
expect_equal(mies_survival_comma(oibigmultiboth, 6, sr, n_elite = 4, elite_selector = sr), oibigmultiboth$archive$data)
expect_equal(oibigmultiboth$archive$data, archive_before)

# killing elites
expect_equal(mies_survival_comma(oibigmultiboth, 4, sr, n_elite = 2, elite_selector = sr), oibigmultiboth$archive$data)
expect_equal(oibigmultiboth$archive$data[1:6, sum(is.na(eol))], 2)
expect_equal(oibigmultiboth$archive$data, archive_before[1:6, eol := oibigmultiboth$archive$data[1:6, eol]])

# killing younglings
expect_equal(mies_survival_comma(oibigmultiboth, 3, sr, n_elite = 2, elite_selector = sr), oibigmultiboth$archive$data)
expect_equal(oibigmultiboth$archive$data[7:9, sum(is.na(eol))], 1)
expect_equal(oibigmultiboth$archive$data, archive_before[7:9, eol := oibigmultiboth$archive$data[7:9, eol]])

# no more elites (doesn't need elite_selector)
expect_equal(mies_survival_comma(oibigmultiboth, 3, sr, n_elite = 0), oibigmultiboth$archive$data)
expect_equal(oibigmultiboth$archive$data, archive_before[1:6, eol := 3])

oibigmultiboth$clear()
oibigmultiboth$eval_batch(designmultiobj)
archive_before$timestamp = oibigmultiboth$archive$data$timestamp
# no more elites (doesn't need elite_selector), killing non-elites at the same time
expect_equal(mies_survival_comma(oibigmultiboth, 1, sr, n_elite = 0), oibigmultiboth$archive$data)
expect_equal(oibigmultiboth$archive$data[7:9, sum(is.na(eol))], 1)
expect_equal(oibigmultiboth$archive$data, archive_before[7:9, eol := oibigmultiboth$archive$data[7:9, eol]])

oibigmultiboth$clear()
oibigmultiboth$eval_batch(designmultiobj)
archive_before = copy(oibigmultiboth$archive$data)

# kill non-elites; doesn't need non-elite selector
expect_equal(mies_survival_comma(oibigmultiboth, 2, n_elite = 2, elite_selector = sr), oibigmultiboth$archive$data)
expect_equal(oibigmultiboth$archive$data[1:6, sum(is.na(eol))], 2)
expect_equal(oibigmultiboth$archive$data, archive_before[8:9, eol := 3][1:6, eol := oibigmultiboth$archive$data[1:6, eol]])





# fill in coverage
oibigmax$clear()
oibigmax$eval_batch(copy(design)[, eol := NULL])
expect_error(mies_survival_comma(oibigmax, 2, sb, n_elite = 1, elite_selector = sb), "No alive individuals. Need to run mies_init_population")

# error when selector selects the same row twice
sdb = SelectorDebug$new(function(v, f, n, p) {
  c(1, seq_len(n - 1))
})$prime(p)
oibigmax$clear()
oibigmax$eval_batch(copy(design))
expect_error(mies_survival_comma(oibigmax, 3, sdb, n_elite = 1, elite_selector = sb), "survival_selector.*not generate duplicates")
expect_error(mies_survival_comma(oibigmax, 3, sb, n_elite = 3, elite_selector = sdb), "elite_selector.*not generate duplicates")
