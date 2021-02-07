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
ParamSetShadow = R6Class("ParamSetShadow", inherit = ParamSet,
  public = list(
    #' @description
    #' Initialize the `ParamSetShadow` object.
    initialize = function(set, shadowed) {
      private$.set = assert_r6(set, "ParamSet")
      private$.shadowed = assert_subset(shadowed, set$ids())
    },
    #' @description
    #' Implemented for compatibility with [`ParamSet`][paradox::ParamSet] superclass. However, this function is
    #' not implemented and will give an error.
    #' @param p not used.
    #' @return nothing.
    add = function(p) stop("Not Allowed."),
    #' @description
    #' Implemented for compatibility with [`ParamSet`][paradox::ParamSet] superclass. However, this function is
    #' not implemented and will give an error.
    #' @param p not used.
    #' @return nothing.
    subset = function(p) stop("Not Allowed."),
    #' @description
    #' \pkg{checkmate}-like check-function for values.
    #'
    #'
    #' Handles dependencies of the shadowed [`ParamSet`][paradox::ParamSet] correctly even for dependencies
    #' that reach to shadowed values, by inserting these shadowed values into `xs`.
    #'
    #' @param xs (named `list`)\cr
    #'   Value to be checked.
    #' @return `TRUE` if successful, otherwise a string describing the error.
    check = function(xs) {
      # We insert the $values of the shadowed ParamSet into xs, so that dependency checks work the way they should.
      # For this, we have to first make sure that xs is a named list that doesn't contain any shadowed values; then
      # we can delegate to the wrapped `ParamSet`.
      ok = check_list(xs, names = "unique")
      if (!isTRUE(ok)) return(ok)
      if (any(names(rhs) %in% private$.shadowed)) {
        return(sprintf("Params %s are shadowed and must not be present in values.", str_collapse(intersect(names(rhs), private$.shadowed), quote = '"')))
      }
      shadow_vals = private$.set$values
      shadow_vals = shadow_vals[intersect(names(shadow_vals), private$.shadowed)]

      private$.set$check(insert_named(xs, shadow_vals))
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
      rbind(private$.set$deps[!id %in% private$.shadowed & !on %in% private$.shadowed, ],
        private$.deps)
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

