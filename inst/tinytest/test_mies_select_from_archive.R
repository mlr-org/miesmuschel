
source("setup.R", local = TRUE)


oibig = as_oi(get_objective_passthrough("maximize", FALSE, "bud"))
oibigmin = as_oi(get_objective_passthrough("minimize", FALSE, "bud"))
oibigmulti = as_oi(get_objective_passthrough(c("minimize", "maximize"), FALSE, "bud"))

design = cbind(generate_design_random(oibig$search_space, 9)$data[, p1 := c(1, 3, 5, 7, 6, 4, 2, 0, 0)],
  data.table(additional = 1:9, dob = rep(1:3, each = 3), eol = rep(c(3, NA, NA), 3))
)

designmultiobj = cbind(generate_design_random(oibigmulti$search_space, 9)$data[, bud := c(1, 1, 1, 3, 3, 7, 5, 5, 9)],
  data.table(additional = 1:9, dob = rep(1:3, each = 3), eol = rep(c(3, NA, NA), 3))
)

oibig$eval_batch(design)
oibigmin$eval_batch(design)
oibigmulti$eval_batch(designmultiobj)

# doesn't recognize additional component
expect_equal(mies_select_from_archive(oibig, 1), design[4, -c("dob", "eol", "additional"), which = FALSE])
expect_equal(mies_select_from_archive(oibig, 1, get_indivs = FALSE), 4)

sb = sel("best", shuffle_selection = FALSE)$prime(miesmuschel:::ps_union(list(oibig$search_space$clone(deep = TRUE), ps(additional = p_int(1, 9)))))
sr = sel("random")$prime(miesmuschel:::ps_union(list(oibig$search_space$clone(deep = TRUE), ps(additional = p_int(1, 9)))))
srm = sel("random")$prime(miesmuschel:::ps_union(list(oibigmulti$search_space$clone(deep = TRUE), ps(additional = p_int(1, 9)))))

expect_equal(mies_select_from_archive(oibig, 1, selector = sb), design[4, -c("dob", "eol"), which = FALSE])

expect_equal(mies_select_from_archive(oibig, 1, 1:3, selector = sb), design[3, -c("dob", "eol"), which = FALSE])
expect_equal(mies_select_from_archive(oibig, 1, 1:3, selector = sb, get_indivs = FALSE), 3)

expect_error(mies_select_from_archive(oibig, 1, 1:3, selector = sel("best", shuffle_selection = FALSE)$prime(ps(p1 = p_dbl()))),
  "Must be a subset of")


expect_equal(mies_select_from_archive(oibig, 6, 1:3, selector = sb), design[c(3:1, 3:1), -c("dob", "eol"), which = FALSE])
expect_equal(mies_select_from_archive(oibig, 6, 1:3, selector = sb, get_indivs = FALSE), c(3:1, 3:1))

expect_equal(mies_select_from_archive(oibig, 0, integer(0), selector = sb, get_indivs = FALSE), integer(0))
expect_equal(mies_select_from_archive(oibig, 0, integer(0), selector = sb, get_indivs = TRUE), design[integer(0), -c("dob", "eol"), which = FALSE])

expect_error(mies_select_from_archive(oibig, 1, integer(0), selector = sb, get_indivs = FALSE), "Must have at least 1 row")

expect_equal(mies_select_from_archive(oibigmin, 6, 1:3, selector = sb), design[c(1:3, 1:3), -c("dob", "eol"), which = FALSE])

expect_equal(mies_select_from_archive(oibigmin, 6, 1:8, selector = sb), design[c(8, 1, 7, 2, 6, 3), -c("dob", "eol"), which = FALSE])

set.seed(1)
sels = mies_select_from_archive(oibig, 4, 1:8, selector = sr, get_indivs = FALSE)
set.seed(1)
selm = mies_select_from_archive(oibigmulti, 4, 1:8, selector = srm, get_indivs = FALSE)
expect_equal(sels, selm)

set.seed(1)
expect_equal(mies_select_from_archive(oibigmulti, 4, 1:8, selector = srm, get_indivs = TRUE),
  designmultiobj[selm, -c("dob", "eol"), which = FALSE])

