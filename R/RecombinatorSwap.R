#' @title Swap Recombinator
#'
#' @include Recombinator.R
#'
#' @name dict_recombinators_swap
#'
#' @description
#' Values between two individuals are exchanged. This is relatively useless as an operator by itself, but is used
#' in combination with [`RecombinatorCmpMaybe`] to get a recombinator that is crossing over individuals
#' uniformly at random. Because this is such a frequently-used operation, the [`RecombinatorCrossoverUniform`] pseudo-class
#' exists as a shortcut.
#'
#' @section Configuration Parameters:
#'   None.
#'
#' @templateVar id swap
#' @template autoinfo_prepare_rec
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family recombinators
#' @examples
#' set.seed(1)
#' rs = rec("swap")
#' p = ps(x = p_int(-5, 5), y = p_dbl(-5, 5), z = p_dbl(-5, 5))
#' data = data.frame(x = 0:5, y = 0:5, z = 0:5)
#'
#' rs$prime(p)
#' rs$operate(data)
#'
#' rx = rec("cmpmaybe", rec("swap"), p = 0.5)  # the same as 'rec("xounif")'
#' rx$prime(p)
#' rx$operate(data)
#'
#' @export
RecombinatorSwap = R6Class("RecombinatorSwap",
  inherit = RecombinatorPair,
  public = list(
    #' @description
    #' Initialize the `RecombinatorCrossoverSwap` object.
    #' @template param_keep_complement
    initialize = function(keep_complement = TRUE) {
      super$initialize(keep_complement, c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), dict_entry = "swap")
    }
  ),
  private = list(
    .recombine_pair = function(values) {
      values[2:1]
    }
  )
)
dict_recombinators$add("swap", RecombinatorSwap)
