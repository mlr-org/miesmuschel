
source("setup.R", local = TRUE)

oibig = as_oi(get_objective_passthrough("maximize", FALSE, "bud"))
design = cbind(generate_design_random(oibig$search_space, 9)$data[, p1 := c(1, 1, 1, 3, 3, 7, 5, 9, 5)],
  data.table(additional = 1:9, dob = rep(1:3, each = 3), eol = rep(c(3, NA, NA), 3))
)
oibig$eval_batch(design)
p = oibig$search_space$clone(deep = TRUE)$add(ps(additional = p_int(1, 9)))
p_nobudget = as_oi(get_objective_passthrough("maximize"))$search_space$clone(deep = TRUE)$add(ps(additional = p_int(1, 9)))

# no operator all: doesn't recognize additional component
expect_equal(mies_generate_offspring(oibig, 1), design[8, -c("dob", "eol", "additional")])
expect_equal(mies_generate_offspring(oibig, 2)[order(p1)], design[c(6, 8), -c("dob", "eol", "additional")])

mn_small = mut("null")$prime(oibig$search_space)
sb_small = sel("best")$prime(oibig$search_space)

mn = mut("null")$prime(p)
rn = rec("null")$prime(p)
mn_bud = mut("null")$prime(p_nobudget)
rn_bud = rec("null")$prime(p_nobudget)
sb = sel("best")$prime(p)

# add 0.1, 0.2, ... to column p1
getrec = function(nin = 1, nout = nin) {
  RecombinatorDebug$new(function(n, v, p) {
    res = if (n == "p1") v + seq_along(v) / 100 else v
    res[seq_len(nout)]
  }, n_indivs_in = nin, n_indivs_out = nout)$prime(p)
}


# add 1, 2, 3.. to column p1
mutator = MutatorDebug$new(function(n, v, p) {
  if (n == "p1") v + seq_along(v) / 10 else v
})$prime(p)



# any one operator gives info about additional component
expect_equal(mies_generate_offspring(oibig, 1, parent_selector = sb), design[8, -c("dob", "eol")])
expect_equal(mies_generate_offspring(oibig, 1, parent_selector = sb, mutator = mn), design[8, -c("dob", "eol")])
expect_equal(mies_generate_offspring(oibig, 1, mutator = mn), design[8, -c("dob", "eol")])
expect_equal(mies_generate_offspring(oibig, 1, mutator = mn, recombinator = rn), design[8, -c("dob", "eol")])
expect_equal(mies_generate_offspring(oibig, 1, recombinator = rn), design[8, -c("dob", "eol")])
expect_equal(mies_generate_offspring(oibig, 1, recombinator = rn, parent_selector = sb), design[8, -c("dob", "eol")])
expect_equal(mies_generate_offspring(oibig, 1, mutator = mn, recombinator = rn, parent_selector = sb), design[8, -c("dob", "eol")])

# presence of budget parameter handled correctly
expect_equal(mies_generate_offspring(oibig, 1, parent_selector = sb, budget_id = "bud"), design[8, -c("dob", "eol", "bud")])
expect_equal(mies_generate_offspring(oibig, 1, parent_selector = sb, mutator = mn_bud, budget_id = "bud"), design[8, -c("dob", "eol", "bud")])
expect_equal(mies_generate_offspring(oibig, 1, mutator = mn_bud, budget_id = "bud"), design[8, -c("dob", "eol", "bud")])
expect_equal(mies_generate_offspring(oibig, 1, mutator = mn_bud, recombinator = rn_bud, budget_id = "bud"), design[8, -c("dob", "eol", "bud")])
expect_equal(mies_generate_offspring(oibig, 1, recombinator = rn_bud, budget_id = "bud"), design[8, -c("dob", "eol", "bud")])
expect_equal(mies_generate_offspring(oibig, 1, recombinator = rn_bud, parent_selector = sb, budget_id = "bud"), design[8, -c("dob", "eol", "bud")])
expect_equal(mies_generate_offspring(oibig, 1, mutator = mn_bud, recombinator = rn_bud, parent_selector = sb, budget_id = "bud"), design[8, -c("dob", "eol", "bud")])

# disagreement about space
expect_error(mies_generate_offspring(oibig, 1, parent_selector = sb_small, mutator = mn), "Must be (equal to set .* but is|a permutation of set .* but has extra)")
expect_error(mies_generate_offspring(oibig, 1, parent_selector = sb, mutator = mn_small), "Must be (equal to set .* but is|a set equal to .* but is missing)")
expect_error(mies_generate_offspring(oibig, 1, recombinator = rn, mutator = mn_small), "Must be (equal to set .* but is|a permutation of set .* but has extra)")
expect_error(mies_generate_offspring(oibig, 1, recombinator = rn, budget_id = "bud"), "Must be (equal to set|a permutation of set).*'bud'")
expect_error(mies_generate_offspring(oibig, 1, mutator = mn, budget_id = "bud"), "Must be (equal to set|a permutation of set).*'bud'")

# mutator
expect_equal(mies_generate_offspring(oibig, 3, mutator = mutator)$p1 %% 1, c(0.1, 0.2, 0.3))
expect_equal(mies_generate_offspring(oibig, 3, mutator = mutator)[order(p1)][, -"p1", with = FALSE], design[c(9, 6, 8), -c("dob", "eol", "p1")])

# recombinator
expect_equal(mies_generate_offspring(oibig, 3, recombinator = getrec())$p1 %% 1, c(0.01, 0.01, 0.01))
expect_equal(mies_generate_offspring(oibig, 3, recombinator = getrec(2))$p1 %% 1, c(0.01, 0.02, 0.01))
expect_equal(mies_generate_offspring(oibig, 3, recombinator = getrec(3))$p1 %% 1, c(0.01, 0.02, 0.03))
expect_equal(mies_generate_offspring(oibig, 3, recombinator = getrec(nin = 3, nout = 2))$p1 %% 1, c(0.01, 0.02, 0.01))
expect_equal(mies_generate_offspring(oibig, 3, recombinator = getrec(nin = 3, nout = 1))$p1 %% 1, c(0.01, 0.01, 0.01))
expect_equal(mies_generate_offspring(oibig, 3, recombinator = getrec())[order(p1)][, -"p1", with = FALSE], design[c(9, 6, 8), -c("dob", "eol", "p1")])
expect_equal(mies_generate_offspring(oibig, 4, recombinator = getrec(2))[order(p1)][, -"p1", with = FALSE], design[c(5, 9, 6, 8), -c("dob", "eol", "p1")])
expect_equal(mies_generate_offspring(oibig, 3, recombinator = getrec(3))[order(p1)][, -"p1", with = FALSE], design[c(9, 6, 8), -c("dob", "eol", "p1")])

set.seed(1)
offspring = mies_generate_offspring(oibig, 1, recombinator = getrec(nin = 2, nout = 1))[order(p1)][, -"p1", with = FALSE]
expect_true(isTRUE(all.equal(offspring, design[8, -c("dob", "eol", "p1")])) || isTRUE(all.equal(offspring, design[6, -c("dob", "eol", "p1")])))

# mutator + recombinator
expect_equal(mies_generate_offspring(oibig, 3, mutator = mutator, recombinator = getrec())$p1 %% 1, c(0.11, 0.21, 0.31))
expect_equal(mies_generate_offspring(oibig, 3, mutator = mutator, recombinator = getrec(2))$p1 %% 1, c(0.11, 0.22, 0.31))
expect_equal(mies_generate_offspring(oibig, 3, mutator = mutator, recombinator = getrec(2, 1))$p1 %% 1, c(0.11, 0.21, 0.31))
expect_equal(mies_generate_offspring(oibig, 3, mutator = mutator, recombinator = getrec())[order(p1)][, -"p1", with = FALSE], design[c(9, 6, 8), -c("dob", "eol", "p1")])
expect_equal(mies_generate_offspring(oibig, 4, mutator = mutator, recombinator = getrec(2))[order(p1)][, -"p1", with = FALSE], design[c(5, 9, 6, 8), -c("dob", "eol", "p1")])


# error message advises initialization
oibig$clear()
oibig$eval_batch(copy(design)[, eol := NULL])
expect_error(mies_generate_offspring(oibig, 10), "No alive individuals. Need to run mies_init_population")
