#' @title Convex Combination Recombinator for Pairs
#'
#' @include Recombinator.R
#'
#' @name dict_recombinators_cvxpair
#'
#' @description
#' Numeric Values between various individuals are recombined via component-wise convex combination (or weighted mean). Exactly two
#' individuals are being recombined, and the `lambda` configuration parameter determines the relative weight of the first
#' individual in each pair for the first result, and the relative weight of the second indivudual for the complement, if
#' initialized with `keep_complement` set to `TRUE`.
#'
#' @section Configuration Parameters:
#' *  `lambda` :: `numeric`\cr
#'   Combination weight. If `keep_complement` is `TRUE`, then two individuals are returned for each pair of input individuals:
#'   one corresponding to `lambda * <1st individual> + (1-lambda) * <2nd individual>`, and one corresponding to
#'   `(1-lambda) * <1st individual> + lambda * <2nd individual>` (i.e. the complement). Otherwise, only the first of these two
#'   is generated.
#'   Must either be a scalar, or a vector with length equal to the number of
#'   components in the values being operated on. Must be between 0 and 1.\cr
#'   Initialized to 0.5.
#'
#' @templateVar id convex
#' @template autoinfo_prepare_rec
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family recombinators
#'
#' @examples
#' set.seed(1)
#' rcvx = rec("cvxpair")
#' p = ps(x = p_dbl(-5, 5), y = p_dbl(-5, 5), z = p_dbl(-5, 5))
#' data = data.frame(x = 0:5, y = 0:5, z = 0:5)
#'
#' rcvx$prime(p)
#' rcvx$operate(data)  # mean of groups of 2
#' # with the default value of lambda = 0.5, the default of
#' # keep_complement = TRUE means that pairs of equal values are generated;
#' # consider setting keep_complement = FALSE int that case.
#'
#' rcvx$param_set$values$lambda = 0.1
#' rcvx$operate(data)
#'
#' @export
RecombinatorConvexPair = R6Class("RecombinatorConvexPair",
  inherit = RecombinatorPair,
  public = list(
    #' @description
    #' Initialize the `RecombinatorConvexPair` object.
    #' @template param_keep_complement
    initialize = function(keep_complement = TRUE) {
      param_set = ps(
        lambda = p_vct(lower = 0, tags = "required")
      )
      param_set$values = list(lambda = 0.5)
      super$initialize(keep_complement = keep_complement, param_classes = "ParamDbl", param_set = param_set, dict_entry = "cvxpair")
    }
  ),
  private = list(
    .recombine = function(values) {
      lambda = self$param_set$get_values()$lambda
      if (!length(lambda) %in% c(1, ncol(values))) stop("lambda must have either length 1, or length of input.")
      lambda * values[1:2] + (1 - lambda) * values[2:1]
    }
  )
)
dict_recombinators$add("cvxpair", RecombinatorConvexPair)
