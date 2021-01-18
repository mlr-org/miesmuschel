



#' @title ParamSetShadow
#'
#' @description
#' Expose only some of a ParamSet's Parameters
ParamSetShadow = R6Class("ParamSetShadow", inherit = ParamSet,
  public = list(
    initialize = function(set, shadowed) {
      private$.set = assert_r6(set, "ParamSet")
      private$.shadowed = assert_subset(shadowed, set$ids())

    },
    add = function(p) stop("Not Allowed."),
    subset = function(p) stop("Not Allowed."),
    check = function(xs) private$.set$check(xs)
  ),
  active = list(
    params = function(rhs) {
      if (!missing(rhs)) {
        stop("params is read-only.")
      }
      params = private$.set$params
      params[private$.shadowed] = NULL
      params
    },
    deps = function(rhs) {
      if (!missing(rhs)) {
        stop("deps is read-only")
      }
      rbind(private$.set$deps[!id %in% private$.shadowed & !on %in% private$.shadowed, ],
        private$.deps)
    },
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

