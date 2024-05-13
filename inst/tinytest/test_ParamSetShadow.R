
source("setup.R", local = TRUE)

# basics

p = ps(x = p_dbl(-1, 1, tags = "test2"), y = p_lgl(), z = p_fct(c("a", "b", "c")),
  a = p_dbl(-2, 2, tags = "test"), b = p_lgl(), c = p_fct(c("x", "y", "z")))
p$values = list(y = TRUE, b = FALSE)
pshadow = ParamSetShadow$new(p, c("x", "y", "z"))

ps_compare = ps(a = p_dbl(-2, 2, tags = "test"), b = p_lgl(), c = p_fct(c("x", "y", "z")))

ps_compare$values = list(b = FALSE)

expect_equal(pshadow$params, ps_compare$params)

expect_read_only(pshadow, c("params", "params_unid", "deps", "origin"))

# object properties
expect_equal(pshadow$values, list(b = FALSE))
# expect_equal(pshadow$set_id, "")
expect_equal(pshadow$has_deps, FALSE)
expect_equal(pshadow$has_trafo, FALSE)
expect_equal(pshadow$is_categ, c(a = FALSE, b = TRUE, c = TRUE))
expect_equal(pshadow$is_number, !c(a = FALSE, b = TRUE, c = TRUE))
expect_equal(pshadow$tags, list(a = "test", b = character(0), c = character(0)))

# reference to origin is kept
expect_identical(pshadow$origin, p)

# values propagate
p$values = list(x = 1, a = 2)
expect_equal(pshadow$values, list(a = 2))
pshadow$values$a = -0.5
expect_equal(p$values, list(x = 1, a = -0.5))
expect_error({pshadow$values$x = -0.5}, "'x' not available")

# printing
expect_stdout(print(pshadow), "ParamSetShadow.* a .* b .* c ")

# $add DEPRECATED
# expect_error(pshadow$add(ps(x = p_dbl(-2, 2))), "Must have unique names|duplicated name")
#
# pshadow$add(ps(zz = p_dbl()))
# expect_equal(p$params$zz, ps(zz = p_dbl())$params$zz)

# $subset
if (miesmuschel:::paradox_s3) {
  expect_equal(pshadow$subset(c("a", "b")), ps(a = p_dbl(-2, 2, tags = "test", init = -0.5), b = p_lgl()),
    check.attributes = FALSE)  # ignore indices of .tag-data.table
} else {
  pshadow$subset(c("a", "b"))
  expect_equal(p$params, ps(a = p_dbl(-2, 2, tags = "test"), b = p_lgl(),
    x = p_dbl(-1, 1, tags = "test2"), y = p_lgl(), z = p_fct(c("a", "b", "c")))$params)
}

if (miesmuschel:::paradox_s3) {
  cond_equal_true = CondEqual(TRUE)
} else {
  cond_equal_true = CondEqual$new(TRUE)
}

# deps
pshadow$add_dep("a", "b", cond_equal_true)
expect_data_table(pshadow$deps, any.missing = FALSE, nrows = 1, ncols = 3)
expect_names(colnames(pshadow$deps), identical.to = c("id", "on", "cond"))
expect_equal(pshadow$deps$id, "a")
expect_equal(pshadow$deps$on, "b")
expect_equal(pshadow$deps$cond, list(cond_equal_true))

expect_equal(pshadow$deps, p$deps)

expect_error(pshadow$add_dep("a", "y", cond_equal_true), "Must be element of .* but is 'y'")
expect_error(pshadow$add_dep("x", "b", cond_equal_true), "Must be element of .* but is 'x'")

# adding dep to origin doesn't change pshadow
p$add_dep("x", "y", cond_equal_true)
expect_data_table(pshadow$deps, any.missing = FALSE, nrows = 1, ncols = 3)
expect_names(colnames(pshadow$deps), identical.to = c("id", "on", "cond"))
expect_equal(pshadow$deps$id, "a")
expect_equal(pshadow$deps$on, "b")
expect_equal(pshadow$deps$cond, list(cond_equal_true))

expect_data_table(p$deps, any.missing = FALSE, nrows = 2, ncols = 3)
expect_equal(p$deps$id, c("a", "x"))
expect_equal(p$deps$on, c("b", "y"))
expect_identical(pshadow$origin, p)  # but they still refer to each other.

# creating PSS across dependency bounds is prohibited

expect_error(ParamSetShadow$new(p, "a"), "Params a have dependencies that reach across shadow bounds")
expect_error(ParamSetShadow$new(p, "b"), "Params a have dependencies that reach across shadow bounds")

# but presence of dependencies does not prevent shadowing along bounds.
if (miesmuschel:::params_s3) {
  # we don't do subset in-place any more, so original paramset still has 'c' parameter
  ps_compare = ps(x = p_dbl(-1, 1, tags = "test2"), y = p_lgl(),
    a = p_dbl(-2, 2, tags = "test"), b = p_lgl(), c = p_fct(c("x", "y", "z")))
  ps_compare_2 = ps(a = p_dbl(-2, 2, tags = "test"), b = p_lgl(), c = p_fct(c("x", "y", "z")))
  ps_compare$values = list(x = 1, a = -0.5)
  ps_compare_2$values = list(a = -0.5)
  ps_compare$add_dep("x", "y", cond_equal_true)
  ps_compare$add_dep("a", "b", cond_equal_true)
  ps_compare_2$add_dep("a", "b", cond_equal_true)
} else {
  ps_compare = ps(a = p_dbl(-2, 2, tags = "test"), b = p_lgl(),
      x = p_dbl(-1, 1, tags = "test2"), y = p_lgl())
  ps_compare_2 = ps(a = p_dbl(-2, 2, tags = "test"), b = p_lgl())
  ps_compare$values = list(x = 1, a = -0.5)
  ps_compare_2$values = list(a = -0.5)
}

expect_equal(ParamSetShadow$new(p, "z")$params, ps_compare$params)

expect_equal(ParamSetShadow$new(p, c("x", "y", "z"))$params,
  ps_compare_2$params)

expect_equal(ParamSetShadow$new(p, c("x", "y", "z"))$deps, pshadow$deps)

if (miesmuschel:::params_s3) {
  pshadow$extra_trafo = function(x, param_set) {
    list(x = x$a + x$b)
  }
  expect_true(pshadow$has_trafo)

  expect_equal(generate_design_grid(pshadow, 2)$transpose(), rep(list(list(x = 0.5), list(x = integer(0))), each = 3))

} else {
  pshadow$trafo = function(x, param_set) {
    list(x = x$a + x$b)
  }
  expect_true(pshadow$has_trafo)

  expect_equal(generate_design_grid(pshadow, 2)$transpose(), list(list(x = 0.5), list(x = integer(0))))

}
