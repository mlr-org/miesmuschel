.paradox_has_owned_shadow = function(
    paradox_version = utils::packageVersion("paradox"),
    paradox_exports = getNamespaceExports("paradox")) {
  isTRUE(paradox_version >= numeric_version("2.0.0")) &&
    "ParamSetShadow" %in% paradox_exports
}

.install_param_set_shadow_bridge = function(namespace,
    paradox_version = utils::packageVersion("paradox"),
    paradox_exports = getNamespaceExports("paradox"),
    get_exported_value = getExportedValue,
    legacy_factory = .make_legacy_param_set_shadow) {
  if (isTRUE(paradox_version >= numeric_version("2.0.0")) &&
      !"ParamSetShadow" %in% paradox_exports) {
    stop("paradox >= 2.0.0 does not export ParamSetShadow")
  }
  use_paradox = .paradox_has_owned_shadow(paradox_version, paradox_exports)
  generator = if (use_paradox) {
    get_exported_value("paradox", "ParamSetShadow")
  } else {
    legacy_factory()
  }
  if (!use_paradox) {
    .leanify_legacy_param_set_shadow(generator, namespace)
  }

  was_locked = bindingIsLocked("ParamSetShadow", namespace)
  if (was_locked) unlockBinding("ParamSetShadow", namespace)
  on.exit(if (was_locked) lockBinding("ParamSetShadow", namespace), add = TRUE)
  assign("ParamSetShadow", generator, envir = namespace)
  invisible(use_paradox)
}

#' @title ParamSetShadow
#'
#' @description
#' Wraps another [`ParamSet`][paradox::ParamSet] and shadows out a subset of its [`Domain`][paradox::Domain]s.
#' The original [`ParamSet`][paradox::ParamSet] can still be accessed through the `$origin` field;
#' otherwise, the `ParamSetShadow` behaves like a [`ParamSet`][paradox::ParamSet] where the shadowed
#' [`Domain`][paradox::Domain]s are not present.
#'
#' With paradox 2.0.0 or newer, this export is the exact
#' `paradox::ParamSetShadow` generator. The local implementation below is
#' retained only so an installed miesmuschel artifact can still be loaded with
#' paradox 1.x.
#'
#' @param set ([`ParamSet`][paradox::ParamSet])\cr
#'   [`ParamSet`][paradox::ParamSet] to wrap.
#' @param shadowed (`character`)\cr
#'   Ids of [`Domain`][paradox::Domain]s to shadow from `sets`, must be a subset of `set$ids()`.
#' @examples
#' p1 = ps(x = p_dbl(0, 1), y = p_lgl())
#' p1$values = list(x = 0.5, y = TRUE)
#' print(p1)
#'
#' p2 = ParamSetShadow$new(p1, "x")
#' print(p2$values)
#'
#' p2$values$y = FALSE
#' print(p2)
#'
#' print(p2$origin$values)
#' @export
# Populated by .onLoad(). Do not serialize Paradox's generator in this
# namespace: leanification would otherwise rewrite its package-owned methods.
ParamSetShadow = NULL

.make_legacy_param_set_shadow = function() {
  R6Class("ParamSetShadow", inherit = ParamSet,
  parent_env = asNamespace("miesmuschel"),
  public = list(
    #' @description
    #' Initialize the `ParamSetShadow` object.
    initialize = function(set, shadowed) {
      private$.set = assert_r6(set, "ParamSet")
      private$.shadowed = assert_subset_character(shadowed, set$ids())
      id = on = NULL
      baddeps = set$deps[(id %in% private$.shadowed) != (on %in% private$.shadowed), id]
      if (length(baddeps)) {
        stopf("Params %s have dependencies that reach across shadow bounds", str_collapse(baddeps))
      }
      if (paradox_s3) {
        .tags = .trafo = NULL  # for static checks
        paramtbl = set$params[!shadowed, on = "id"]
        private$.tags = paramtbl[, .(tag = unlist(.tags)), keyby = "id"]
        private$.trafos = setkeyv(paramtbl[!map_lgl(.trafo, is.null), .(id, trafo = .trafo)], "id")
        set(paramtbl, , grep("^\\.", colnames(paramtbl), value = TRUE), NULL)
        setindexv(paramtbl, c("id", "cls", "grouping"))
        private$.params = paramtbl
        private$.extra_trafo = set$extra_trafo
      }
    },

    #' @description
    #' Checks underlying [`ParamSet`][paradox::ParamSet]'s constraint.
    #' It uses the underlying `$values` for shadowed values.
    #'
    #' @param x (named `list`) values to test
    #' @param ... Further arguments passed to [`ParamSet`][paradox::ParamSet]'s `$test_constraint()` function.
    #' @return `logical(1)`.
    test_constraint = function(x, ...) {
      assert_list(x, names = "unique")
      if (length(x)) assert_names(names(x), disjunct.from = private$.shadowed)
      values_underlying = private$.set$values
      values_underlying = values_underlying[intersect(names(values_underlying), private$.shadowed)]
      private$.set$test_constraint(c(x, values_underlying), ...)
    },
    #' @description
    #' Adds a dependency to the unterlying [`ParamSet`][paradox::ParamSet].
    #'
    #' @param id (`character(1)`)
    #' @param on (`character(1)`)
    #' @param cond ([`Condition`][paradox::Condition])
    #' @param allow_dangling_dependencies (`logical(1)`): Whether to allow dependencies on parameters that are not present.
    #' @param ... Further arguments passed to [`ParamSet`][paradox::ParamSet]'s `$add_dep()` function.
    #' @return `invisible(self)`.
    add_dep = function(id, on, cond,  allow_dangling_dependencies = FALSE, ...) {
      ids = self$ids()
      assert_choice(id, ids)
      if (!allow_dangling_dependencies) assert_choice(on, ids) else assert_string(on)
      if (paradox_s3) {
        private$.set$add_dep(id = id, on = on, cond = cond, allow_dangling_dependencies = allow_dangling_dependencies, ...)
      } else {
        private$.set$add_dep(id = id, on = on, cond = cond, ...)
      }
      invisible(self)
    }
  ),
  active = list(
    constraint = function(f) {
      if (!missing(f)) {
        stop("ParamSetShadow does not allow setting constraint.")
      } else {
        constraint = private$.set$constraint
        if (is.null(constraint)) return(NULL)
        # we give constraint() the underlying ParamSet and construct 'values_underlying' on the fly
        # this is so that changing values gets the correct result even when the underlying PS's values change.
        set = private$.set
        shadowed = private$.shadowed
        crate(function(x) {
          assert_list(x, names = "unique")
          values_underlying = set$values
          values_underlying = values_underlying[intersect(names(values_underlying), shadowed)]
          x[names(values_underlying)] = values_underlying
          constraint(x)
        }, constraint, set, shadowed)
      }
    },
    #' @field params (named `list()`)\cr
    #' Table of rows identifying the contained [`Domain`][paradox::Domain]s
    params = function(rhs) {
      if (!missing(rhs)) {
        stop("params is read-only.")
      }
      if (paradox_s3) return(super$params)  # TODO this function can go altogether with new paradox
      params = private$.set$params
      params[private$.shadowed] = NULL
      params
    },

    #' @field params_unid (named `list` of `Param`)
    #' List of `Param` that are members of the wrapped [`ParamSet`][paradox::ParamSet] with the
    #' shadowed `Param`s removed. This is a field mostly for internal usage that has the
    #' `$id`s set to invalid values but avoids cloning overhead.\cr
    #' Available only in the paradox 1.x compatibility implementation.
    params_unid = function(rhs) {
      if (!missing(rhs)) {
        stop("params_unid is read-only.")
      }
      if (paradox_s3) return(super$params())  # TODO this function can go altogether with new paradox
      params = private$.set$params_unid
      params[private$.shadowed] = NULL
      params
    },
    #' @field deps ([`data.table`][data.table::data.table])\cr
    #' Table of dependencies, as in [`ParamSet`][paradox::ParamSet]. The dependencies that are related to shadowed
    #' parameters are not exposed. This [`data.table`][data.table::data.table] should be seen as read-only and not
    #' modified in-place; instead, the `$origin`'s `$deps` should be modified.
    deps = function(rhs) {
      if (!missing(rhs)) {
        stop("deps is read-only.")
      }
      id = on = NULL
      private$.set$deps[!id %in% private$.shadowed & !on %in% private$.shadowed, ]
    },
    #' @field values (named `list`)\cr
    #' List of values, as in [`ParamSet`][paradox::ParamSet], with the shadowed values removed.
    values = function(rhs) {
      if (!missing(rhs)) {
        assert_list(rhs)
        self$assert(rhs)
        all_values = private$.set$values
        all_values = all_values[intersect(names(all_values), private$.shadowed)]
        all_values = c(all_values, rhs)
        private$.set$values = all_values
      }
      values = private$.set$values
      values[private$.shadowed] = NULL
      values
    },
    #' @field set_id ([`data.table`][data.table::data.table])\cr
    #' Id of the wrapped [`ParamSet`][paradox::ParamSet]. Changing this value will also change the wrapped [`ParamSet`][paradox::ParamSet]'s `$set_id` accordingly.
    set_id = function(v) {
      if (paradox_s3) {
        if (!missing(v)) stop("setting $set_id no longer supported!")
        warning("$set_id is deprecated!")
        return(NULL)
      }
      if (!missing(v)) {
        private$.set$set_id = v
      }
      private$.set$set_id
    },
    #' @field origin ([`ParamSet`][paradox::ParamSet])\cr
    #' [`ParamSet`][paradox::ParamSet] being wrapped. This object can be modified by reference to influence the `ParamSetShadow` object itself.
    origin = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.set)) {
        stop("origin is read-only.")
      }
      private$.set
    }
  ),
  private = list(
    .set = NULL,
    .shadowed = NULL,
    .extra_trafo = NULL
    )
  )
}

.legacy_shadow_binding = function(owner, name) {
  if (!is.environment(owner) ||
      !exists(name, envir = owner, inherits = FALSE) ||
      bindingIsActive(name, owner)) {
    stop(sprintf("Malformed legacy ParamSetShadow binding `%s`", name))
  }
  value = eval(call("substitute", as.name(name), owner), envir = baseenv())
  if (is.language(value) || is.symbol(value)) {
    stop(sprintf("Delayed legacy ParamSetShadow binding `%s` is unsupported", name))
  }
  value
}

.inspect_legacy_param_set_shadow = function(x) {
  if (!is.environment(x) ||
      !identical(attr(x, "class", exact = TRUE),
        c("ParamSetShadow", "ParamSet", "R6"))) {
    stop("Malformed legacy miesmuschel ParamSetShadow")
  }
  enclosure = .legacy_shadow_binding(x, ".__enclos_env__")
  private = .legacy_shadow_binding(enclosure, "private")
  origin = .legacy_shadow_binding(private, ".set")
  shadowed = .legacy_shadow_binding(private, ".shadowed")
  if (!is.environment(enclosure) || !is.environment(private) ||
      !is.environment(origin) || !inherits(origin, "ParamSet") ||
      !is.character(shadowed) || anyNA(shadowed)) {
    stop("Malformed legacy miesmuschel ParamSetShadow state")
  }
  list(
    state = list(shadowed = shadowed),
    dependencies = list(origin = origin)
  )
}

.rebuild_legacy_param_set_shadow = function(base, state, dependencies) {
  paradox::ParamSetShadow$new(dependencies$origin, state$shadowed)
}

.register_paradox_shadow_upgrader = function() {
  if (!"register_paradox_object_upgrader" %in%
      getNamespaceExports("paradox")) {
    return(invisible(FALSE))
  }
  getExportedValue("paradox", "register_paradox_object_upgrader")(
    owner_package = "miesmuschel",
    legacy_class = c("ParamSetShadow", "ParamSet", "R6"),
    migration_kind = "replacement",
    inspector = ".inspect_legacy_param_set_shadow",
    rebuilder = ".rebuild_legacy_param_set_shadow",
    retired_bindings = c("params_unid", "set_id")
  )
  invisible(TRUE)
}

.legacy_shadow_target_names = paste0(
  ".__ParamSetShadow__",
  c(
    "add_dep", "clone", "constraint", "deps", "initialize", "origin",
    "params", "params_unid", "set_id", "test_constraint", "values"
  )
)

.leanify_legacy_param_set_shadow = function(generator, namespace) {
  if (!R6::is.R6Class(generator)) return(invisible(FALSE))
  locked = vapply(
    .legacy_shadow_target_names,
    bindingIsLocked,
    logical(1L),
    env = namespace
  )
  for (name in .legacy_shadow_target_names[locked]) {
    unlockBinding(name, namespace)
  }
  on.exit({
    for (name in .legacy_shadow_target_names[locked]) {
      lockBinding(name, namespace)
    }
  }, add = TRUE)
  mlr3misc::leanify_r6(generator, namespace)
  invisible(TRUE)
}

.legacy_shadow_graph_api = function() {
  "upgrade_paradox_object_graph" %in% getNamespaceExports("paradox")
}

.upgrade_legacy_shadow_first_use = function(self, target) {
  if (!.legacy_shadow_graph_api()) return(FALSE)
  action = getOption("paradox.legacy_object_action", "error")
  if (!identical(action, "upgrade")) {
    stop(
      sprintf(
        paste0(
          "A serialized Paradox 1 object tried to call ",
          "`miesmuschel::%s`. Upgrade the containing object with ",
          "`upgrade_paradox_object_graph(x)`. To perform this migration ",
          "silently on first use, set ",
          "`options(paradox.legacy_object_action = \"upgrade\")`."
        ),
        target
      ),
      call. = FALSE
    )
  }
  getExportedValue("paradox", "upgrade_paradox_object_graph")(self)
  TRUE
}

.legacy_shadow_function = function(member, kind, frame) {
  generator = .make_legacy_param_set_shadow()
  method = switch(
    kind,
    public = generator$public_methods[[member]],
    active = generator$active[[member]]
  )
  if (!is.function(method)) {
    stop(sprintf("Missing legacy ParamSetShadow member `%s`", member))
  }
  environment(method) = frame
  method
}

.replay_shadow_active = function(self, member, supplied, value = NULL) {
  if (!exists(member, envir = self, inherits = FALSE) ||
      !bindingIsActive(member, self)) {
    stop(sprintf(
      "The legacy ParamSetShadow binding `%s` was retired by Paradox 2",
      member
    ))
  }
  binding = activeBindingFunction(member, self)
  if (supplied) binding(value) else binding()
}

# Historical miesmuschel releases leanified their local ParamSetShadow at
# package load. Serialized stubs therefore resolve these exact namespace names
# before any inherited Paradox method can notice the old shell. On Paradox 2
# they are cold migration gateways; on Paradox 1 they retain the old behavior.
.__ParamSetShadow__initialize = function(
    self, private, super, set, shadowed) {
  if (!.upgrade_legacy_shadow_first_use(
      self, ".__ParamSetShadow__initialize")) {
    legacy = .legacy_shadow_function("initialize", "public", environment())
    return(legacy(set, shadowed))
  }
  self$initialize(set, shadowed)
}

.__ParamSetShadow__test_constraint = function(
    self, private, super, x, ...) {
  if (!.upgrade_legacy_shadow_first_use(
      self, ".__ParamSetShadow__test_constraint")) {
    legacy = .legacy_shadow_function(
      "test_constraint", "public", environment()
    )
    return(legacy(x, ...))
  }
  self$test_constraint(x, ...)
}

.__ParamSetShadow__add_dep = function(
    self, private, super, id, on, cond,
    allow_dangling_dependencies = FALSE, ...) {
  if (!.upgrade_legacy_shadow_first_use(
      self, ".__ParamSetShadow__add_dep")) {
    legacy = .legacy_shadow_function("add_dep", "public", environment())
    return(legacy(
      id, on, cond,
      allow_dangling_dependencies = allow_dangling_dependencies,
      ...
    ))
  }
  self$add_dep(
    id, on, cond,
    allow_dangling_dependencies = allow_dangling_dependencies,
    ...
  )
}

.__ParamSetShadow__clone = function(self, private, super, deep = FALSE) {
  if (!.upgrade_legacy_shadow_first_use(self, ".__ParamSetShadow__clone")) {
    legacy = .legacy_shadow_function("clone", "public", environment())
    return(legacy(deep = deep))
  }
  self$clone(deep = deep)
}

.__ParamSetShadow__constraint = function(self, private, super, f) {
  if (!.upgrade_legacy_shadow_first_use(
      self, ".__ParamSetShadow__constraint")) {
    legacy = .legacy_shadow_function("constraint", "active", environment())
    if (missing(f)) return(legacy())
    return(legacy(f))
  }
  .replay_shadow_active(self, "constraint", !missing(f),
    if (!missing(f)) f)
}

.__ParamSetShadow__deps = function(self, private, super, rhs) {
  if (!.upgrade_legacy_shadow_first_use(self, ".__ParamSetShadow__deps")) {
    legacy = .legacy_shadow_function("deps", "active", environment())
    if (missing(rhs)) return(legacy())
    return(legacy(rhs))
  }
  .replay_shadow_active(self, "deps", !missing(rhs),
    if (!missing(rhs)) rhs)
}

.__ParamSetShadow__origin = function(self, private, super, rhs) {
  if (!.upgrade_legacy_shadow_first_use(self, ".__ParamSetShadow__origin")) {
    legacy = .legacy_shadow_function("origin", "active", environment())
    if (missing(rhs)) return(legacy())
    return(legacy(rhs))
  }
  .replay_shadow_active(self, "origin", !missing(rhs),
    if (!missing(rhs)) rhs)
}

.__ParamSetShadow__params = function(self, private, super, rhs) {
  if (!.upgrade_legacy_shadow_first_use(self, ".__ParamSetShadow__params")) {
    legacy = .legacy_shadow_function("params", "active", environment())
    if (missing(rhs)) return(legacy())
    return(legacy(rhs))
  }
  .replay_shadow_active(self, "params", !missing(rhs),
    if (!missing(rhs)) rhs)
}

.__ParamSetShadow__params_unid = function(self, private, super, rhs) {
  if (!.upgrade_legacy_shadow_first_use(
      self, ".__ParamSetShadow__params_unid")) {
    legacy = .legacy_shadow_function("params_unid", "active", environment())
    if (missing(rhs)) return(legacy())
    return(legacy(rhs))
  }
  .replay_shadow_active(self, "params_unid", !missing(rhs),
    if (!missing(rhs)) rhs)
}

.__ParamSetShadow__set_id = function(self, private, super, v) {
  if (!.upgrade_legacy_shadow_first_use(self, ".__ParamSetShadow__set_id")) {
    legacy = .legacy_shadow_function("set_id", "active", environment())
    if (missing(v)) return(legacy())
    return(legacy(v))
  }
  .replay_shadow_active(self, "set_id", !missing(v),
    if (!missing(v)) v)
}

.__ParamSetShadow__values = function(self, private, super, rhs) {
  if (!.upgrade_legacy_shadow_first_use(self, ".__ParamSetShadow__values")) {
    legacy = .legacy_shadow_function("values", "active", environment())
    if (missing(rhs)) return(legacy())
    return(legacy(rhs))
  }
  .replay_shadow_active(self, "values", !missing(rhs),
    if (!missing(rhs)) rhs)
}
