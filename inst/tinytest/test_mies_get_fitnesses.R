
source("setup.R", local = TRUE)

oibigmin = as_oi(get_objective_passthrough("minimize", FALSE, "bud"))
oibigmax = as_oi(get_objective_passthrough("maximize", FALSE, "bud"))
oibigmultimin = as_oi(get_objective_passthrough(c("minimize", "minimize"), FALSE, "bud"))
oibigmultimax = as_oi(get_objective_passthrough(c("maximize", "maximize"), FALSE, "bud"))
oibigmultiboth = as_oi(get_objective_passthrough(c("minimize", "maximize"), FALSE, "bud"))

design = cbind(generate_design_random(oibigmin$search_space, 9)$data[, bud := c(1, 1, 1, 3, 3, 7, 5, 5, 9)],
  data.table(additional = 1:9, dob = rep(1:3, each = 3), eol = rep(c(3, NA, NA), 3))
)

designmultiobj = cbind(generate_design_random(oibigmultiboth$search_space, 9)$data[, bud := c(1, 1, 1, 3, 3, 7, 5, 5, 9)],
  data.table(additional = 1:9, dob = rep(1:3, each = 3), eol = rep(c(3, NA, NA), 3))
)

singlefitness = as.matrix(design[, .(pout1 = p1)])
multifitness = as.matrix(designmultiobj[, .(pout1 = p1, pout2 = p2)])

oibigmin$eval_batch(design)
expect_equal(mies_get_fitnesses(oibigmin, 2:4), singlefitness[2:4, , drop = FALSE] * -1)
expect_equal(mies_get_fitnesses(oibigmin, 2), singlefitness[2, , drop = FALSE] * -1)

oibigmax$eval_batch(design)
expect_equal(mies_get_fitnesses(oibigmax, 2:4), singlefitness[2:4, , drop = FALSE])
expect_equal(mies_get_fitnesses(oibigmax, 2), singlefitness[2, , drop = FALSE])

oibigmax$eval_batch(design)
expect_equal(mies_get_fitnesses(oibigmax, 2:4), singlefitness[2:4, , drop = FALSE])
expect_equal(mies_get_fitnesses(oibigmax, 2), singlefitness[2, , drop = FALSE])

oibigmultimin$eval_batch(designmultiobj)
expect_equal(mies_get_fitnesses(oibigmultimin, 2:4), multifitness[2:4, , drop = FALSE] * -1)
expect_equal(mies_get_fitnesses(oibigmultimin, 2), multifitness[2, , drop = FALSE] * -1)

oibigmultimax$eval_batch(designmultiobj)
expect_equal(mies_get_fitnesses(oibigmultimax, 2:4), multifitness[2:4, , drop = FALSE])
expect_equal(mies_get_fitnesses(oibigmultimax, 2), multifitness[2, , drop = FALSE])

oibigmultiboth$eval_batch(designmultiobj)
expect_equal(mies_get_fitnesses(oibigmultiboth, 2:4), cbind(multifitness[2:4, 1, drop = FALSE] * -1, multifitness[2:4, 2, drop = FALSE]))
expect_equal(mies_get_fitnesses(oibigmultiboth, 2), cbind(multifitness[2, 1, drop = FALSE] * -1, multifitness[2, 2, drop = FALSE]))

