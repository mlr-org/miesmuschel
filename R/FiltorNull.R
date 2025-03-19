#' @title Null-Filtor
#'
#' @include Filtor.R
#'
#' @name dict_filtors_null
#'
#' @description
#' Null-filtor that does not perform filtering. Its `needed_input()` is always  the `output_size`, and `operate()` selects the first `n_filter` values from its input.
#'
#' Useful in particular with operator-wrappers such as [`FiltorProxy`], and to make filtering optional.
#'
#' @section Configuration Parameters:
#' This operator has no configuration parameters.
#'
#' @templateVar id null
#' @template autoinfo_prepare_ftr
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family filtors
#' @examples
#' fn = ftr("null")
#'
#' p = ps(x = p_dbl(-5, 5))
#' known_data = data.frame(x = as.numeric(1:5))
#' fitnesses = as.numeric(1:5)
#'
#' new_data = data.frame(x = c(2.5, 4.5))
#'
#' fn$prime(p)
#'
#' fn$needed_input(1)
#'
#' fn$operate(new_data, known_data, fitnesses, 1)
#'
#' @export
FiltorNull = R6Class("FiltorNull",
  inherit = Filtor,
  public = list(
    #' @description
    #' Initialize the `FiltorNull` object.
    initialize = function() {
      super$initialize(dict_entry = "null")
    }
  ),
  private = list(
    .filter = function(values, known_values, fitnesses, n_filter) {
      seq_len(n_filter)
    },
    .needed_input = function(output_size) output_size
  )
)
dict_filtors$add("null", FiltorNull)
