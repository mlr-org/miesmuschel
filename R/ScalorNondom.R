#' @title Nondominated Sorting Scalor
#'
#' @include Scalor.R
#'
#' @name dict_scalors_nondom
#'
#' @description
#' [`Scalor`] that returns a the rank of the pareto-front in nondominated sorting as scale. Higher ranks
#' indocate higher fitnesses and therefore "better" individuals.
#'
#' @section Configuration Parameters:
#' This operator has no configuration parameters.
#'
#' @templateVar id nondom
#' @template autoinfo_prepare_scl
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family scalors
#' @examples
#' so = scl("nondom")
#' p = ps(x = p_dbl(-5, 5))
#' # dummy data; note that ScalorNondom does not depend on data content
#' data = data.frame(x = rep(0, 5))
#' fitnesses = matrix(c(1, 5, 2, 3, 0, 3, 1, 0, 10, 8), ncol = 2)
#'
#' so$prime(p)
#'
#' so$operate(data, fitnesses)
#' @export
ScalorNondom = R6Class("ScalorNondom",
  inherit = Scalor,
  public = list(
    #' @description
    #' Initialize the `ScalorNondom` object.
    initialize = function() {
      param_set = ps(
        epsilon = p_vct(lower = 0, tags = "required"),
        scale_output = p_lgl(),
        jitter = p_lgl(),
        tiebreak = p_fct(c("crowding-dist", "hv-contrib", "domcount", "none")),


      super$initialize(dict_entry = "nondom")
    }
  ),
  private = list(
    .scale = function(values, fitnesses) {
      sorted = order_nondominated(fitnesses)$fronts
      max(sorted) + 1 - sorted  # want high front values for high fitnesses, so reverse ordering here
    }
  )
)
dict_scalors$add("nondom", ScalorNondom)

