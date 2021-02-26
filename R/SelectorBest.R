#' @title Best Value Selector
#'
#' @include Selector.R
#'
#' @name dict_selectors_best
#'
#' @description
#' Selector that selects the top `n_select` individuals based on the  fitness value. When `n_select` is larger than the number
#' of individuals, the selection wraps around: All `nrow(values)` individuals are selected at least `floor(nrow(values) / n_select)`
#' times, with the top `nrow(values) %% n_select` individuals being selected one more time.
#'
#' @section Hyperparameters:
#' This operator has no hyperparameters.
#'
#' @templateVar id best
#' @template autoinfo_prepare_sel
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family selectors
#' @export
SelectorBest = R6Class("SelectorBest",
  inherit = Selector,
  public = list(
    #' @description
    #' Initialize the `SelectorBest` object.
    initialize = function() {
      super$initialize(supported = "single-crit")
    }
  ),
  private = list(
    .select = function(values, fitnesses, n_select) {
      order(fitnesses, decreasing = TRUE)[(seq_len(n_select) - 1) %% nrow(values) + 1]
    }
  )
)
dict_selectors$add("best", SelectorBest)
