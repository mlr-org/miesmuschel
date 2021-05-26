#' @title Scalor Counting Dominating Individuals
#'
#' @include Scalor.R
#'
#' @name dict_scalors_domcount
#'
#' @description
#' [`Scalor`] that returns a the number of (weakly, epsilon-) dominated or dominating individuals for each individuum.
#'
#' @section Configuration Parameters:
#'
#' * `output`
#' * `epsilon`
#' * `jitter`
#' * `scale_output`
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
        output = p_fct(c("count_dominated", "count_not_dominating"), tags = "required"),
        epsilon = p_vct(lower = 0, tags = "required"),
        jitter = p_lgl(tags = "required"),
        scale_output = p_lgl(tags = "required")
      )
      param_set$values = list(output = "count_not_dominating", epsilon = 0, jitter = TRUE, scale_output = TRUE)
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
      if (params$output == "number_dominated") {
        domcount = rank_nondominated(-fitnesses, epsilon = params$epsilon)$domcount
      } else {
        domcount = nrow(fitnesses) - rank_nondominated(fitnesses, epsilon = params$epsilon)$domcount
      }
      if (params$scale_output) {
        domcount / nrow(fitnesses)
      } else {
        domcount
      }
    }
  )
)
dict_scalors$add("domcount", ScalorDomcount)

