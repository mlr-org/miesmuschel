#' @title Single Objective Scalor
#'
#' @include Scalor.R
#'
#' @name dict_scalors_single
#'
#' @description
#' [`Scalor`] that uses a single given objective, throwing an error in case it is used in a multi-objective problem.
#'
#' In contrast to [`ScalorOne`], this [`Scalor`] throws an error when more than one objective is present. When
#' this [`Scalor`] gets used as the default value, e.g. for a [`Selector`], then it
#' forces the user to make an explicit decision about what [`Scalor`] to use in a multi-objective setting.
#'
#' @section Configuration Parameters:
#'   No configuration parameters.
#'
#' @templateVar id single
#' @template autoinfo_prepare_scl
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family scalors
#' @examples
#' ss = scl("single")
#' p = ps(x = p_dbl(-5, 5))
#' # dummy data; note that ScalorOne does not depend on data content
#' data = data.frame(x = rep(0, 5))
#' fitnesses_so = c(1, 5, 2, 3, 0)
#' fitnesses_mo = matrix(c(1, 5, 2, 3, 0, 3, 1, 0, 10, 8), ncol = 2)
#'
#' ss$prime(p)
#'
#' ss$operate(data, fitnesses_so)
#'
#' try(ss$operate(data, fitnesses_mo))
#'
#' @export
ScalorSingleObjective = R6Class("ScalorSingleObjective",
  inherit = Scalor,
  public = list(
    #' @description
    #' Initialize the `ScalorSingleObjective` object.
    initialize = function() {
      super$initialize(supported = "single-crit", dict_entry = "single")
    }
  ),
  private = list(
    .scale = function(values, fitnesses) {
      if (ncol(fitnesses) != 1) stop("Used ScalorSingleObjective in a multi-objective setting. You must choose a different Scalor to use.")
      fitnesses[, 1]
    }
  )
)
dict_scalors$add("single", ScalorSingleObjective)
