#' @title Best Value Selector
#'
#' @include Selector.R
#'
#' @name dict_selectors_best
#'
#' @description
#' [`Selector`] that selects the top `n_select` individuals based on the  fitness value. When `n_select` is larger than the number
#' of individuals, the selection wraps around: All `nrow(values)` individuals are selected at least `floor(nrow(values) / n_select)`
#' times, with the top `nrow(values) %% n_select` individuals being selected one more time.
#'
#' @section Configuration Parameters:
#' This operator has no configuration parameters.
#'
#' @templateVar id best
#' @template autoinfo_prepare_sel
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family selectors
#' @examples
#' sb = sel("best")
#' p = ps(x = p_dbl(-5, 5))
#' # dummy data; note that SelectorBest does not depend on data content
#' data = data.frame(x = rep(0, 5))
#' fitnesses = c(1, 5, 2, 3, 0)
#'
#' sb$prime(p)
#'
#' sb$operate(data, fitnesses, 2)
#'
#' sb$operate(data, fitnesses, 4)
#' @export
SelectorBest = R6Class("SelectorBest",
  inherit = SelectorScalar,
  public = list(
    #' @description
    #' Initialize the `SelectorBest` object.
    initialize = function(scalor = ScalorSingleObjective$new()) {
      super$initialize(scalor = scalor, dict_entry = "best")
    }
  ),
  private = list(
    .select_scalar = function(values, fitnesses, n_select) {
      order(fitnesses, decreasing = TRUE)[(seq_len(n_select) - 1) %% nrow(values) + 1]
    }
  )
)
dict_selectors$add("best", SelectorBest)
