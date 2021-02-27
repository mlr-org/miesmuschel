#' @title Crossover Recombinator
#'
#' @include Recombinator.R
#'
#' @name dict_recombinators_xounif
#'
#' @description
#' Values between two individuals are exchanged with component-wise independent probability.
#'
#' @section Configuration Parameters:
#' * `p` :: `numeric(1)`\cr
#'   Component-wise probability with which to exchange values. Initialized to 0.5.
#'
#' @templateVar id xounif
#' @template autoinfo_prepare_rec
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family recombinators
#' @export
RecombinatorCrossoverUniform = R6Class("RecombinatorCrossoverUniform",
  inherit = Recombinator,
  public = list(
    #' @description
    #' Initialize the `RecombinatorCrossoverUniform` object.
    #' @param keep_complement (`logical(1)`)\cr
    #'   Whether the operation should keep both individuals that were crossed over (`TRUE`), or only the first and discard
    #'   the crossover complement (`FALSE`). Default `TRUE`
    initialize = function(keep_complement = TRUE) {
      param_set = ps(p = p_dbl(0, tags = "required"))
      param_set$values = list(p = 0.5)
      super$initialize(c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set, 2, if (keep_complement) 2 else 1)
    }
  ),
  private = list(
    .recombine = function(values) {
      params = self$param_set$get_values()
      index = lapply(sample(1:2, length(values), TRUE, c(1 - params$p, params$p)), function(x) c(x, 3 - x))
      setnames(values[, pmap(list(.SD, index), `[`)][seq_len(self$n_indivs_out)], names(values))
    }
  )
)
dict_recombinators$add("xounif", RecombinatorCrossoverUniform)
