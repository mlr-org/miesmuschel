#' @title Null-Filtor
#'
#' @include Filtor.R
#'
#' @description
#' Does not filter.
#'
#' ` # TODO better docu `
#'
#' @family filtors
#' @export
FiltorNull = R6Class("FiltorNull",
  inherit = Filtor,
  public = list(
    initialize = function() {
      super$initialize()
    }
  ),
  private = list(
    .filter = function(values, known_values, fitnesses, n_filter) {
      seq_len(n_filter)
    },
    .needed_input = function(output_size) output_size
  )
)
