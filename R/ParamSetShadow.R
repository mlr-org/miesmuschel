#' @title ParamSetShadow
#'
#' @description
#' Wraps another [`ParamSet`][paradox::ParamSet] and shadows out a subset of its [`Domain`][paradox::Domain]s.
#' The original [`ParamSet`][paradox::ParamSet] can still be accessed through the `$origin` field;
#' otherwise, the `ParamSetShadow` behaves like a [`ParamSet`][paradox::ParamSet] where the shadowed
#' [`Domain`][paradox::Domain]s are not present.
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
ParamSetShadow = R6Class("ParamSetShadow", inherit = ParamSet,
  public = list(
    #' @description
    #' Initialize the `ParamSetShadow` object.
    initialize = function(set, shadowed) {
      private$.set = assert_r6(set, "ParamSet")
      private$.shadowed = assert_subset(shadowed, set$ids())
      id = on = NULL
      baddeps = set$deps[(id %in% private$.shadowed) != (on %in% private$.shadowed), id]
      if (length(baddeps)) {
        stopf("Params %s have dependencies that reach across shadow bounds", str_collapse(baddeps))
      }
      if (paradox_s3) {
        paramtbl = set$params[!shadowed, on = "id"]
        private$.tags = paramtbl[, .(tag = unlist(.tags)), keyby = "id"]
        private$.trafos = setkeyv(paramtbl[!map_lgl(.trafo, is.null), .(id, trafo = .trafo)], "id")
        set(paramtbl, , setdiff(colnames(paramtbl), colnames(set$.__enclos_env__$private$.params)), NULL)
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
      private$.set$add_dep(id = id, on = on, cond = cond, allow_dangling_dependencies = allow_dangling_dependencies, ...)
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
    #' Table of rows identifying the contained [`Domain`]s
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
    #' Deprecated by the upcoming `paradox` package update and will be removed in the future.
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

