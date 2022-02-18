#' @title Null Selector
#'
#' @include Selector.R
#'
#' @name dict_selectors_null
#'
#' @description
#' Selector that disregards fitness and individual values and selects individuals by order in which they are given.
#'
#' @section Configuration Parameters:
#' This operator has no configuration parameters.
#'
#' @templateVar id null
#' @template autoinfo_prepare_sel
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family selectors
#' @examples
#' sn = sel("null")
#' p = ps(x = p_dbl(-5, 5))
#' # dummy data; note that SelectorNull does not depend on data content
#' data = data.frame(x = rep(0, 5))
#' fitnesses = c(1, 5, 2, 3, 0)
#'
#' sn$prime(p)
#'
#' sn$operate(data, fitnesses, 2)
#' sn$operate(data, fitnesses, 4)
#' sn$operate(data, fitnesses, 6)
#' @export
SelectorNull = R6Class("SelectorNull",
  inherit = Selector,
  public = list(
    #' @description
    #' Initialize the `SelectorNull` object.
    initialize = function() {
      super$initialize(dict_entry = "null")
    }
  ),
  private = list(
    .select = function(values, fitnesses, n_select) {
      (seq_len(n_select) - 1) %% nrow(values) + 1  # mod-operation to wrap around when n_select > nrow(values)
    }
  )
)
dict_selectors$add("null", SelectorNull)
