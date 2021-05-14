#' @title Random Selector
#'
#' @include Selector.R
#'
#' @name dict_selectors_random
#'
#' @description
#' Random selector that disregards fitness and individual values and selects individuals randomly. Depending on the configuration parameter `replace`,
#' it samples with or without replacement.
#'
#' @section Configuration Parameters:
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
#' @examples
#' set.seed(1)
#' sr = sel("random")
#' p = ps(x = p_dbl(-5, 5))
#' # dummy data; note that SelectorRandom does not depend on data content
#' data = data.frame(x = rep(0, 5))
#' fitnesses = c(1, 5, 2, 3, 0)
#'
#' sr$prime(p)
#'
#' sr$operate(data, fitnesses, 2)
#' sr$operate(data, fitnesses, 2)
#' sr$operate(data, fitnesses, 2)
#'
#' sr$operate(data, fitnesses, 4)
#' sr$operate(data, fitnesses, 4)
#' sr$operate(data, fitnesses, 4)
#' @export
SelectorRandom = R6Class("SelectorRandom",
  inherit = Selector,
  public = list(
    #' @description
    #' Initialize the `SelectorRandom` object.
    initialize = function() {
      param_set = ps(replace = p_lgl(tags = "required"))
      param_set$values = list(replace = FALSE)
      super$initialize(param_set = param_set, dict_entry = "random")
    }
  ),
  private = list(
    .select = function(values, fitnesses, n_select, context) {
      params = self$param_set$get_values(context = context)
      sample(nrow(values), n_select, replace = params$replace)
    }
  )
)
dict_selectors$add("random", SelectorRandom)
