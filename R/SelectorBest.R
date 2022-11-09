#' @title Best Value Selector
#'
#' @include Selector.R
#'
#' @name dict_selectors_best
#'
#' @description
#' [`Selector`] that selects the top `n_select` individuals based on the  fitness value, breaking ties randomly. When `n_select` is larger than the number
#' of individuals, the selection wraps around: All `nrow(values)` individuals are selected at least `floor(nrow(values) / n_select)`
#' times, with the top `nrow(values) %% n_select` individuals being selected one more time.
#'
#' @section Configuration Parameters:
#' @template confparam_shuffle_selection
#'
#' @templateVar id best
#' @template autoinfo_prepare_sel
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family selectors
#' @examples
#' sb = sel("best")
#' p = ps(x = p_dbl(-5, 5))
#' # dummy data; note that SelectorBest does not depend on data content
#' data = data.frame(x = rep(0, 5))
#' fitnesses = c(1, 5, 2, 3, 0)
#'
#' sb$prime(p)
#'
#' sb$operate(data, fitnesses, 2)
#'
#' sb$param_set$values$shuffle_selection = FALSE
#'
#' sb$operate(data, fitnesses, 4)
#' @export
SelectorBest = R6Class("SelectorBest",
  inherit = SelectorScalar,
  public = list(
    #' @description
    #' Initialize the `SelectorBest` object.
    #' @template param_scalor
    initialize = function(scalor = ScalorSingleObjective$new()) {
      super$initialize(scalor = scalor, is_deterministic = TRUE, dict_entry = "best", packages = "stats")
    }
  ),
  private = list(
    .select_scalar = function(values, fitnesses, n_select, group_size) {
      # runif() as second argument to break ties
      order(fitnesses, stats::runif(length(fitnesses)), decreasing = TRUE)[(seq_len(n_select) - 1) %% nrow(values) + 1]
    }
  )
)
dict_selectors$add("best", SelectorBest)

