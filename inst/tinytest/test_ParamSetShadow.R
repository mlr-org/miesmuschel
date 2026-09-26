
source("setup.R", local = TRUE)

# package bridge

native_generator = new.env(parent = emptyenv())
legacy_generator = R6::R6Class("ParamSetShadow",
  public = list(initialize = function(...) NULL)
)

expect_false(miesmuschel:::.paradox_has_owned_shadow(
  numeric_version("1.1.0"),
  "ParamSetShadow"
))
expect_false(miesmuschel:::.paradox_has_owned_shadow(
  numeric_version("2.0.0"),
  character()
))
expect_true(miesmuschel:::.paradox_has_owned_shadow(
  numeric_version("2.0.0"),
  "ParamSetShadow"
))

# with paradox 2 the bridge binds paradox's generator
bridge_namespace = new.env(parent = emptyenv())
bridge_namespace$ParamSetShadow = NULL
expect_true(miesmuschel:::.install_param_set_shadow_bridge(
  bridge_namespace,
  paradox_version = numeric_version("2.0.0"),
  paradox_exports = "ParamSetShadow",
  get_exported_value = function(package, name) {
    expect_identical(package, "paradox")
    expect_identical(name, "ParamSetShadow")
    native_generator
  },
  legacy_factory = function() stop("legacy generator was constructed")
))
expect_identical(bridge_namespace$ParamSetShadow, native_generator)

# with paradox 1 it binds the local implementation and leanifies it, which
# populates the historical .__ParamSetShadow__* names with the
# implementation's own method bodies
legacy_namespace = new.env(parent = emptyenv())
legacy_namespace$ParamSetShadow = NULL
expect_false(miesmuschel:::.install_param_set_shadow_bridge(
  legacy_namespace,
  paradox_version = numeric_version("1.1.0"),
  paradox_exports = character(),
  get_exported_value = function(package, name) stop("native generator was requested"),
  legacy_factory = function() legacy_generator
))
expect_identical(legacy_namespace$ParamSetShadow, legacy_generator)
expect_true(is.function(legacy_namespace$.__ParamSetShadow__initialize))

# The load-time decision must not depend on the paradox version under which a
# source or binary package happened to be built.
missing_export_namespace = new.env(parent = emptyenv())
missing_export_namespace$ParamSetShadow = NULL
expect_error(miesmuschel:::.install_param_set_shadow_bridge(
  missing_export_namespace,
  paradox_version = numeric_version("2.0.0"),
  paradox_exports = character(),
  legacy_factory = function() legacy_generator
), "does not export ParamSetShadow")
expect_null(missing_export_namespace$ParamSetShadow)

expect_true("ParamSetShadow" %in% getNamespaceExports("miesmuschel"))
if ("ParamSetShadow" %in% getNamespaceExports("paradox")) {
  expect_identical(
    miesmuschel::ParamSetShadow,
    getExportedValue("paradox", "ParamSetShadow")
  )
}

# serialized-object migration bridge

legacy_origin = ps(x = p_dbl(0, 1), hidden = p_lgl())
legacy_private = new.env(parent = emptyenv())
legacy_private$.set = legacy_origin
legacy_private$.shadowed = "hidden"
legacy_private$.params = data.table(id = "x")
legacy_private$.tags = data.table(id = character(0), tag = character(0))
legacy_private$.extra_trafo = NULL
legacy_enclosure = new.env(parent = asNamespace("miesmuschel"))
legacy_shell = new.env(parent = emptyenv())
class(legacy_shell) = c("ParamSetShadow", "ParamSet", "R6")
legacy_enclosure$self = legacy_shell
legacy_enclosure$private = legacy_private
legacy_shell$.__enclos_env__ = legacy_enclosure

inspected = miesmuschel:::.inspect_legacy_param_set_shadow(legacy_shell)
expect_identical(inspected$state, list(shadowed = "hidden", tags = list(x = character(0))))
expect_identical(inspected$dependencies, list(origin = legacy_origin))

# tags set on the legacy shadow itself are carried along; an extra_trafo that
# differs from the origin's refuses the upgrade instead of being dropped
tagged_private = new.env(parent = emptyenv())
tagged_private$.set = legacy_origin
tagged_private$.shadowed = "hidden"
tagged_private$.params = data.table(id = "x")
tagged_private$.tags = data.table(id = c("x", "x"), tag = c("custom", "another"))
tagged_private$.extra_trafo = NULL
tagged_enclosure = new.env(parent = asNamespace("miesmuschel"))
tagged_enclosure$self = legacy_shell
tagged_enclosure$private = tagged_private
legacy_shell$.__enclos_env__ = tagged_enclosure
tagged = miesmuschel:::.inspect_legacy_param_set_shadow(legacy_shell)
expect_identical(tagged$state$tags, list(x = c("another", "custom")))

tagged_private$.extra_trafo = function(x, param_set) x
expect_error(
  miesmuschel:::.inspect_legacy_param_set_shadow(legacy_shell),
  "extra_trafo.*differs from its origin"
)
tagged_private$.extra_trafo = NULL
legacy_shell$.__enclos_env__ = legacy_enclosure

forced = new.env(parent = emptyenv())
forced$value = FALSE
delayed_private = new.env(parent = emptyenv())
delayedAssign(".set", {
  forced$value = TRUE
  legacy_origin
}, assign.env = delayed_private)
delayed_private$.shadowed = "hidden"
delayed_enclosure = new.env(parent = asNamespace("miesmuschel"))
delayed_enclosure$self = legacy_shell
delayed_enclosure$private = delayed_private
legacy_shell$.__enclos_env__ = delayed_enclosure
expect_error(
  miesmuschel:::.inspect_legacy_param_set_shadow(legacy_shell),
  "Delayed legacy ParamSetShadow binding"
)
expect_false(forced$value)
legacy_shell$.__enclos_env__ = legacy_enclosure

if ("ParamSetShadow" %in% getNamespaceExports("paradox")) {
  rebuilt = miesmuschel:::.rebuild_legacy_param_set_shadow(
    ps(x = p_dbl(0, 1)),
    inspected$state,
    inspected$dependencies
  )
  expect_identical(rebuilt$origin, legacy_origin)
  expect_identical(rebuilt$ids(), "x")
  expect_identical(rebuilt$tags, list(x = character(0)))

  # legacy shadow-local tags become the rebuilt view's own answer and leave
  # the origin untouched
  rebuilt_tagged = miesmuschel:::.rebuild_legacy_param_set_shadow(
    NULL,
    tagged$state,
    tagged$dependencies
  )
  expect_identical(rebuilt_tagged$tags, list(x = c("another", "custom")))
  expect_identical(legacy_origin$tags$x, character(0))

  old_action = options(paradox.legacy_object_action = "error")
  on.exit(options(old_action), add = TRUE)
  expect_error(
    miesmuschel:::.__ParamSetShadow__values(
      legacy_shell, legacy_private, NULL
    ),
    "upgrade_paradox_object_graph"
  )

  # a current object passes straight through the gateways, so a stub taken
  # from an object before it was upgraded keeps working afterwards
  current = ParamSetShadow$new(ps(x = p_dbl(0, 1), hidden = p_lgl()), "hidden")
  stub = function(rhs) {
    miesmuschel:::.__ParamSetShadow__values(
      self = current, private = NULL, super = NULL, rhs = rhs
    )
  }
  expect_identical(stub(), current$values)
  stub(list(x = 0.25))
  expect_identical(current$values, list(x = 0.25))
  expect_identical(
    miesmuschel:::.__ParamSetShadow__origin(current, NULL, NULL),
    current$origin
  )
  expect_true(miesmuschel:::.__ParamSetShadow__test_constraint(
    current, NULL, NULL, x = list(x = 0.5)
  ))
  cloned = miesmuschel:::.__ParamSetShadow__clone(current, NULL, NULL, deep = TRUE)
  expect_identical(cloned$values, list(x = 0.25))
  expect_error(
    miesmuschel:::.__ParamSetShadow__params_unid(current, NULL, NULL),
    "retired"
  )
  expect_error(
    miesmuschel:::.__ParamSetShadow__initialize(
      current, NULL, NULL, current$origin, "hidden"
    ),
    "cannot be initialized again"
  )
} else {
  legacy_instance = ParamSetShadow$new(legacy_origin, "hidden")
  private = legacy_instance$.__enclos_env__$private
  expect_true(isNamespace(parent.env(legacy_instance$.__enclos_env__)))
  expect_true(grepl(
    ".__ParamSetShadow__values",
    paste(
      deparse(body(activeBindingFunction("values", legacy_instance))),
      collapse = ""
    ),
    fixed = TRUE
  ))
  expect_identical(
    miesmuschel:::.__ParamSetShadow__values(
      legacy_instance, private, NULL
    ),
    legacy_instance$values
  )
}

# basics

p = ps(x = p_dbl(-1, 1, tags = "test2"), y = p_lgl(), z = p_fct(c("a", "b", "c")),
  a = p_dbl(-2, 2, tags = "test"), b = p_lgl(), c = p_fct(c("x", "y", "z")))
p$values = list(y = TRUE, b = FALSE)
pshadow = ParamSetShadow$new(p, c("x", "y", "z"))

ps_compare = ps(a = p_dbl(-2, 2, tags = "test"), b = p_lgl(), c = p_fct(c("x", "y", "z")))

ps_compare$values = list(b = FALSE)

expect_equal(pshadow$params, ps_compare$params)

read_only_fields = c("params", "deps", "origin")
if (!miesmuschel:::.paradox_has_owned_shadow()) {
  read_only_fields = c(read_only_fields, "params_unid")
}
expect_read_only(pshadow, read_only_fields)

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
}

if (miesmuschel:::paradox_s3) {
  cond_equal_true = CondEqual(TRUE)
} else {
  cond_equal_true = CondEqual$new(TRUE)
}

# dormant dependent values

dormant_origin = ps(
  gate = p_lgl(),
  child = p_int(),
  hidden = p_int()
)
dormant_origin$add_dep("child", "gate", cond_equal_true)
dormant_origin$values = list(hidden = 9L)
dormant_shadow = ParamSetShadow$new(dormant_origin, "hidden")

if (miesmuschel:::.paradox_has_owned_shadow()) {
  dormant_shadow$values = list(gate = FALSE, child = 1L)
  expect_identical(
    dormant_shadow$values,
    list(gate = FALSE, child = 1L)
  )
  expect_identical(dormant_shadow$get_values(), list(gate = FALSE))

  dormant_shadow$values$gate = TRUE
  expect_identical(
    dormant_shadow$get_values(),
    list(gate = TRUE, child = 1L)
  )

  dormant_shadow$values$gate = FALSE
  expect_identical(
    dormant_shadow$values,
    list(gate = FALSE, child = 1L)
  )
  expect_identical(dormant_shadow$get_values(), list(gate = FALSE))
  expect_identical(dormant_shadow$origin$values$hidden, 9L)
} else {
  before = dormant_shadow$origin$values
  expect_error(
    {
      dormant_shadow$values = list(gate = FALSE, child = 1L)
    },
    "child.*can only be set"
  )
  expect_identical(dormant_shadow$origin$values, before)
  expect_identical(length(dormant_shadow$values), 0L)
}

# deps
pshadow$add_dep("a", "b", cond_equal_true)
expect_data_table(pshadow$deps, any.missing = FALSE, nrows = 1, ncols = 3)
expect_names(colnames(pshadow$deps), identical.to = c("id", "on", "cond"))
expect_equal(pshadow$deps$id, "a")
expect_equal(pshadow$deps$on, "b")
expect_equal(pshadow$deps$cond, list(cond_equal_true))

expect_equal(pshadow$deps, p$deps)

shadow_dependency_error = function(legacy, owned) {
  if (miesmuschel:::.paradox_has_owned_shadow()) {
    owned
  } else {
    legacy
  }
}
expect_error(pshadow$add_dep("a", "y", cond_equal_true),
  shadow_dependency_error(
    "Must be element of .* but is 'y'",
    "Dependency of visible 'a' on hidden 'y' crosses the ParamSetShadow boundary"
  ))
expect_error(pshadow$add_dep("x", "b", cond_equal_true),
  shadow_dependency_error(
    "Must be element of .* but is 'x'",
    "'x' is hidden by this ParamSetShadow"
  ))

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

expect_error(ParamSetShadow$new(p, "a"), shadow_dependency_error(
  "Params a have dependencies that reach across shadow bounds",
  "Dependency of hidden 'a' on visible 'b' crosses the ParamSetShadow boundary"
))
expect_error(ParamSetShadow$new(p, "b"), shadow_dependency_error(
  "Params a have dependencies that reach across shadow bounds",
  "Dependency of visible 'a' on hidden 'b' crosses the ParamSetShadow boundary"
))

ps_compare = ps(x = p_dbl(-1, 1, tags = "test2"), y = p_lgl(),
  a = p_dbl(-2, 2, tags = "test"), b = p_lgl(), c = p_fct(c("x", "y", "z")))
ps_compare$values = list(x = 1, a = -0.5)
# add deps after setting values...
ps_compare$add_dep("x", "y", cond_equal_true)
ps_compare$add_dep("a", "b", cond_equal_true)

ps_compare_2 = ps(a = p_dbl(-2, 2, tags = "test"), b = p_lgl(), c = p_fct(c("x", "y", "z")))
ps_compare_2$values = list(a = -0.5)
ps_compare_2$add_dep("a", "b", cond_equal_true)

expect_equal(ParamSetShadow$new(p, "z")$params, ps_compare$params)

expect_equal(ParamSetShadow$new(p, c("x", "y", "z"))$params,
  ps_compare_2$params)

expect_equal(ParamSetShadow$new(p, c("x", "y", "z"))$deps, pshadow$deps)

if (miesmuschel:::paradox_s3) {
  pshadow$extra_trafo = function(x, param_set) {
    list(x = x$a + x$b)
  }
} else {
  pshadow$trafo = function(x, param_set) {
    list(x = x$a + x$b)
  }
}
expect_true(pshadow$has_trafo)

expect_equal(generate_design_grid(pshadow, 2)$transpose(), rep(list(list(x = 0.5), list(x = integer(0))), each = 3))
