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
#' * `output` :: `character(1)`\cr
#'   What to count: individuals that are being dominated by the point under consideration(`"count_dominated"`),
#'   or individuals that do not dominate the point under consideration (`"count_not_dominating"`).
#'   In both cases, a larger output means the individual is "better", in some way, according to the fitness values.
#'   Initialized with `"count_not_dominating"`.
#' * `epsilon` :: `numeric`\cr
#'   Epsilon-value for non-dominance, as used by [`rank_nondominated`]. Initialized to `0`.
#' * `jitter` :: `logical(1)`\cr
#'   Whether to add random jitter to points, with magnitude `sqrt(.Machine$double.eps)` relative to fitness values.
#'   This is used to effectively break ties.
#' * `scale_output` :: `logical(1)`\cr
#'   Whether to scale output by the total numberof individuals, giving output between `0` and `1` (inclusive) when `TRUE`
#'   or integer outputs ranging from 0 and `nrow(fitnesses)` (inclusive) when `FALSE`. Initialized to `TRUE`.
#'
#' @templateVar id domcount
#' @template autoinfo_prepare_scl
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family scalors
#' @examples
#' p = ps(x = p_dbl(-5, 5))
#' data = data.frame(x = rep(0, 5))
#'
#' sd = scl("domcount")
#' sd$prime(p)
#'
#' (fitnesses = matrix(c(1, 5, 2, 3, 0, 3, 1, 0, 10, 8), ncol = 2))
#'
#' # to see the fitness matrix, use:
#' ## plot(fitnesses, pch = as.character(1:5))
#'
#' # note that for both 2 and 4, all points do not dominate them
#' # their value is therefore 1
#' sd$operate(data, fitnesses)
#'
#' sd$param_set$values$scale_output = FALSE
#' sd$operate(data, fitnesses)
#'
#' sd$param_set$values$output = "count_dominated"
#' # point 4 dominates three other points, point 2 only one other point.
#' sd$operate(data, fitnesses)
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
      super$initialize(param_set = param_set, packages = "stats", dict_entry = "domcount")
    }
  ),
  private = list(
    .scale = function(values, fitnesses) {
      params = self$param_set$get_values()
      if (params$jitter) {
        fitnesses = fitnesses *
          (1 + stats::runif(length(fitnesses)) * sqrt(.Machine$double.eps))
      }
      if (params$output == "count_dominated") {
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

