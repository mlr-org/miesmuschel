#' @title ParamSetShadow
#'
#' @description
#' Wraps another [`ParamSet`][paradox::ParamSet] and shadows out a subset of its [`Param`][paradox::Param]s.
#' The original [`ParamSet`][paradox::ParamSet] can still be accessed through the `$origin` field;
#' otherwise, the `ParamSetShadow` behaves like a [`ParamSet`][paradox::ParamSet] where the shadowed
#' [`Param`][paradox::Param]s are not present.
#'
#' @param set ([`ParamSet`][paradox::ParamSet])\cr
#'   [`ParamSet`][paradox::ParamSet] to wrap.
#' @param shadowed (`character`)\cr
#'   Ids of [`Param`][paradox::Param]s to shadow from `sets`, must be a subset of `set$ids()`.
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
    },
    #' @description
    #' Adds a single param or another set to this set, all params are cloned.
    #'
    #' This calls the underlying [`ParamSet`][paradox::ParamSet]'s `$add()` function.
    #'
    #' [`Param`][paradox::Param] with ids that also occur in the underlying  [`ParamSet`][paradox::ParamSet]
    #' but are shadowed can *not* be added and instead will result in an error.
    #'
    #' @param p ([`Param`][paradox::Param] | [`ParamSet`][paradox::ParamSet])
    #' @return `invisible(self)`.
    add = function(p) {
      private$.set$add(p)
      invisible(self)
    },
    #' @description
    #' Reduces the parameters to the ones of passed ids.
    #'
    #' This calls the underlying [`ParamSet`][paradox::ParamSet]'s `$subset()` function.
    #'
    #' @param ids (`character`)
    #' @return `invisible(self)`.
    subset = function(ids) {
      assert_subset(ids, names(self$params_unid))
      private$.set$subset(c(ids, private$.shadowed))
      invisible(self)
    },
    #' @description
    #' Adds a dependency to the unterlying [`ParamSet`][paradox::ParamSet].
    #'
    #' @param id (`character(1)`)
    #' @param on (`character(1)`)
    #' @param cond ([`Condition`][paradox::Condition])
    #' @return `invisible(self)`.
    add_dep = function(id, on, cond) {
      ids = names(self$params_unid)
      assert_choice(id, ids)
      assert_choice(on, ids)
      private$.set$add_dep(id, on, cond)
      invisible(self)
    }
  ),
  active = list(
    #' @field params (named `list` of [`Param`][paradox::Param])
    #' List of [`Param`][paradox::Param] that are members of the wrapped [`ParamSet`][paradox::ParamSet] with the
    #' shadowed [`Param`][paradox::Param]s removed.
    params = function(rhs) {
      if (!missing(rhs)) {
        stop("params is read-only.")
      }
      params = private$.set$params
      params[private$.shadowed] = NULL
      params
    },
    #' @field params_unid (named `list` of [`Param`][paradox::Param])
    #' List of [`Param`][paradox::Param] that are members of the wrapped [`ParamSet`][paradox::ParamSet] with the
    #' shadowed [`Param`][paradox::Param]s removed. This is a field mostly for internal usage that has the
    #' `$id`s set to invalid values but avoids cloning overhead.
    params_unid = function(rhs) {
      if (!missing(rhs)) {
        stop("params is read-only.")
      }
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
        stop("deps is read-only")
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
    .shadowed = NULL
  )
)

