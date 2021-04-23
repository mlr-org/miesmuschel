#' @title Scalor Counting Dominating Individuals
#'
#' @include Scalor.R
#'
#' @name dict_scalors_domcount
#'
#' @description
#' [`Scalor`] that returns a the number of (weakly, epsilon-) dominating individuals for each individuum.
#'
#' @section Configuration Parameters:
#'
#' * `epsilon`
#'
#' @templateVar id domcount
#' @template autoinfo_prepare_scl
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family scalors
#' @examples
#' # TODO
#' @export
ScalorDomcount = R6Class("ScalorDomcount",
  inherit = Scalor,
  public = list(
    #' @description
    #' Initialize the `ScalorNondom` object.
    initialize = function() {
      param_set = ps(
        epsilon = p_vct(lower = 0, tags = "required"),
        jitter = p_lgl(tags = "required"),
        scale_output = p_lgl(tags = "required"),
      )
      param_set$values = list(epsilon = 0, jitter = TRUE, scale_output = TRUE)
      super$initialize(param_set = param_set, dict_entry = "domcount")
    }
  ),
  private = list(
    .scale = function(values, fitnesses) {
      params = self$param_set$get_values()
      if (params$jitter) {
        fitnesses = fitnesses *
          (1 + runif(length(fitnesses)) * sqrt(.Machine$double.eps))
      }
      nadir = 0
      rnd = rank_nondominated(fitnesses, epsilon = params$epsilon)
      # TODO
    }
  )
)
dict_scalors$add("domcount", ScalorDomcount)

