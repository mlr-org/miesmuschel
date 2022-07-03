
source("setup.R", local = TRUE)


oibig = as_oi(get_objective_passthrough("maximize", FALSE, "bud"))
oibigmin = as_oi(get_objective_passthrough("minimize", FALSE, "bud"))
oibigmulti = as_oi(get_objective_passthrough(c("minimize", "maximize"), FALSE, "bud"))

set.seed(1)

oibig$eval_batch(generate_design_random(oibig$search_space, 100)$data[, `:=`(additional = 1:100, dob = rep(1:4, each = 25), eol = rep(c(4, NA, NA, NA), 25))])
oibigmin$eval_batch(generate_design_random(oibig$search_space, 100)$data[, `:=`(dob = rep(1:4, each = 25), eol = rep(c(4, NA, NA, NA), 25))])
oibigmulti$eval_batch(generate_design_random(oibigmulti$search_space, 100)$data[, `:=`(dob = rep(1:4, each = 25), eol = rep(c(4, NA, NA, NA), 25))])

p = oibig$search_space$clone(deep = TRUE)$add(ps(additional = p_int(1, 100)))
p_nobudget = as_oi(get_objective_passthrough("maximize"))$search_space$clone(deep = TRUE)$add(ps(additional = p_int(1, 100)))


individuals = generate_design_random(oibig$search_space, 21)$data[, `:=`(additional = 1:21, p1 = c(1:10, 1:10, 1))]
individuals_min = generate_design_random(oibig$search_space, 21)$data[, `:=`(p1 = c(1:10, 1:10, 1))]
individuals_multi = generate_design_random(oibigmulti$search_space, 21)$data

oibig_clone = oibig$clone(deep = TRUE)

# without filtor
expect_error(mies_filter_offspring(oibig, individuals, 3), "filtor must be given when individuals contain additional components")

expect_equal(mies_filter_offspring(oibigmin, as.data.table(individuals_min), 0), individuals_min[0])
expect_equal(mies_filter_offspring(oibigmin, individuals_min, 0, get_indivs = FALSE), integer(0))

expect_equal(mies_filter_offspring(oibigmin, individuals_min, 3), individuals_min[1:3])
expect_equal(mies_filter_offspring(oibigmin, individuals_min, 3, get_indivs = FALSE), 1:3)

# NULL filtor
fn = FiltorNull$new()

fn$prime(p)
expect_equal(mies_filter_offspring(oibig, individuals, 3, fn), individuals[1:3])
expect_equal(mies_filter_offspring(oibig, individuals, 3, fn, get_indivs = FALSE), 1:3)
# error when not primed correctly
expect_error(mies_filter_offspring(oibigmin, individuals_min, 1, fn), "Must be (equal to set .* but is|a permutation of set .* but has extra)")

# FiltorSurrogateProgressive
library("mlr3learners")
fs = FiltorSurrogateProgressive$new(mlr3::lrn("regr.lm"))
fs$param_set$values$filter.pool_factor = 7
fs$prime(p)
expect_equal(mies_filter_offspring(oibig, individuals, 0, fs, get_indivs = FALSE), integer(0))
expect_equal(mies_filter_offspring(oibig, individuals, 1, fs, get_indivs = FALSE), 7)
expect_equal(mies_filter_offspring(oibig, individuals, 2, fs, get_indivs = FALSE), c(10, 9))
expect_subset(mies_filter_offspring(oibig, individuals, 3, fs, get_indivs = FALSE), c(20, 10, 9, 19))

# minimization returns indivs with smallest instead of largest p1
fs$prime(oibigmin$search_space)
expect_equal(mies_filter_offspring(oibigmin, individuals_min, 0, fs, get_indivs = FALSE), integer(0))
expect_equal(mies_filter_offspring(oibigmin, individuals_min, 1, fs, get_indivs = FALSE), 1)
expect_set_equal(mies_filter_offspring(oibigmin, individuals_min, 2, fs, get_indivs = FALSE), c(1, 11))
expect_set_equal(mies_filter_offspring(oibigmin, individuals_min, 3, fs, get_indivs = FALSE), c(1, 11, 21))


# budget
individuals_nobudget = copy(individuals)[, bud := NULL]
fs$prime(p_nobudget)
expect_error(mies_filter_offspring(oibig, individuals_nobudget, 0, fs, budget_id = "bud"), ".*bud.* but is .*")

fs$prime(p)
expect_error(mies_filter_offspring(oibig, individuals_nobudget, 0, fs, budget_id = "bud2"), "budget_id.*failed: Must be.*'bud'.* but is .*bud2")

expect_equal(mies_filter_offspring(oibig, individuals_nobudget, 0, fs, budget_id = "bud", get_indivs = FALSE), integer(0))

expect_subset(mies_filter_offspring(oibig, individuals_nobudget, 3, fs, budget_id = "bud", get_indivs = FALSE), c(10, 20, 9, 19))


fdp_record = new.env()
fdp = FiltorDebug$new(
  function(v, k, f, n, p) {
    fdp_record$v = v
    fdp_record$k = k
    fdp_record$f = f
    seq_len(n)
  },
  function(o, p) o
)

fdp$prime(p)
expect_equal(mies_filter_offspring(oibig, individuals_nobudget, 3, fdp, budget_id = "bud", get_indivs = FALSE), 1:3)
expect_equal(mies_filter_offspring(oibig, individuals_nobudget, 3, fdp, budget_id = "bud", get_indivs = TRUE), individuals_nobudget[1:3])  # budget not included in return

# no fidelity_schedule: given maximum budget found in data
expect_equal(fdp_record$f, matrix(oibig$archive$data$pout1, ncol = 1, dimnames = list(NULL, "pout1")))
expect_equal(fdp_record$k, oibig$archive$data[, p$ids(), with = FALSE], ignore.col.order = TRUE)
expect_equal(fdp_record$v, cbind(individuals_nobudget, bud = max(oibig$archive$data$bud)), ignore.col.order = TRUE)



# with given fidelity: use that fidelity

expect_equal(mies_filter_offspring(oibig, individuals_nobudget, 3, fdp, budget_id = "bud", fidelity = 4, get_indivs = FALSE), 1:3)
expect_equal(fdp_record$f, matrix(oibig$archive$data$pout1, ncol = 1, dimnames = list(NULL, "pout1")))
expect_equal(fdp_record$k, oibig$archive$data[, p$ids(), with = FALSE], ignore.col.order = TRUE)
expect_equal(fdp_record$v, cbind(individuals_nobudget, bud = 4), ignore.col.order = TRUE)

# multicrit
fdp$prime(oibigmulti$search_space)
expect_equal(mies_filter_offspring(oibigmulti, individuals_multi, 3, fdp, get_indivs = FALSE), 1:3)
expect_equal(fdp_record$f, as.matrix(oibigmulti$archive$data[, .(pout1 = -pout1, pout2 = pout2)]))
expect_equal(fdp_record$k, oibigmulti$archive$data[, oibigmulti$search_space$ids(), with = FALSE], ignore.col.order = TRUE)
expect_equal(fdp_record$v, individuals_multi)



# reject empty OptimInstance
oibig_clear = oibig$clone(deep = TRUE)
oibig_clear$clear()
expect_error(mies_filter_offspring(oibig_clear, individuals_nobudget, 3, fdp, budget_id = "bud", get_indivs = FALSE),
  "mies_filter_offspring does not work with empty OptimInstance.")


expect_equal(oibig_clone, oibig)
