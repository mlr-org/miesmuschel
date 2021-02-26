#' @title Random Selector
#'
#' @include Selector.R
#'
#' @name dict_selectors_random
#'
#' @description
#' Random selector that disregards fitness and individual values and selects individuals randomly. Depending on the hyperparameter `replace`,
#' it samples with or without replacement.
#'
#' @section Hyperparameters:
#' * `replace` :: `logical(1)`\cr
#'   Whether to sample individuals with (`TRUE`) or without (`FALSE`) replacement. When sampling is done without replacement, then
#'   `n_select` must be less or equal the number of rows in `values` when calling `$operate()`. Initialized to `FALSE`.
#'
#' @templateVar id random
#' @template autoinfo_prepare_sel
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family selectors
#' @export
SelectorRandom = R6Class("SelectorRandom",
  inherit = Selector,
  public = list(
    #' @description
    #' Initialize the `SelectorRandom` object.
    initialize = function() {
      param_set = ps(replace = p_lgl(tags = "required"))
      param_set$values = list(replace = FALSE)
      super$initialize(c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set)
    }
  ),
  private = list(
    .select = function(values, fitnesses, n_select) {
      params = self$param_set$get_values()
      sample(nrow(values), n_select, replace = params$replace)
    }
  )
)
dict_selectors$add("random", SelectorRandom)
