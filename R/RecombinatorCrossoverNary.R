#' @title N-ary Crossover Recombinator
#'
#' @include Recombinator.R
#'
#' @name dict_recombinators_xonary
#'
#' @description
#' Values are chosen componentwise independently at random from multiple individuals.
#' The number of individuals must be determined during construction as `n_indivs_in`.
#'
#' The number of output individuals is always 1, i.e. `n_indivs_in` are used to create one output value. When using this
#' recombinator in a typical EA setting, e.g. with [`mies_generate_offspring`], it is therefore recommended to use a parent-selector
#' where the expected quality of selected parents does not depend on the number of parents selected when `n_indivs_in` is large:
#' [`sel("tournament")`][SelectorTournament] is preferred to [`sel("best")`][SelectorBest].
#'
#' @section Configuration Parameters:
#' *  `p` :: `numeric` | `matrix`\cr
#'   Sampling weights these are normalized to sum to 1 internally. Must either be a vector of length `n_indivs_in`, or a matrix with
#'   `n_indivs_in` rows and as many columns as there
#'   are components in the values being operated on. Must be non-negative, at least one value per column must be greater than zero, but it is not
#'   necessary that they sum to 1.\cr
#'   Initialized to `rep(1, n_indivs_in)`, i.e. uniform sampling from all individuals being operated on.
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
#' rxon = rec("xonary", n_indivs_in = 3)
#' p = ps(x = p_dbl(-5, 5), y = p_dbl(-5, 5), z = p_dbl(-5, 5))
#' data = data.frame(x = 0:5, y = 0:5, z = 0:5)
#'
#' rxon$prime(p)
#' rxon$operate(data)  # uniform sampling from groups of 3
#'
#' rxon = rec("xonary", 3, p = c(0, 1, 2))$prime(p)
#' # for groups of 3, take with probability 1/3 from 2nd and with probability 2/3 from 3rd row
#' rxon$operate(data)
#'
#' pmat = matrix(c(0, 1, 2, 1, 1, 1, 1, 0, 0), ncol = 3)
#' pmat
#'
#' rxon = rec("xonary", 3, p = pmat)$prime(p)
#' rxon$operate(data)  # componentwise different operation
#'
#' @export
RecombinatorCrossoverNary = R6Class("RecombinatorCrossoverNary",
  inherit = Recombinator,
  public = list(
    #' @description
    #' Initialize the `RecombinatorConvex` object.
    #' @template param_n_indivs_in
    initialize = function(n_indivs_in = 2) {
      param_set = ps(
        p = p_mtx(rows = n_indivs_in, lower = 0, tags = "required")
      )
      param_set$values = list(p = rep(1, n_indivs_in))
      super$initialize(param_set = param_set, n_indivs_in = n_indivs_in, n_indivs_out = 1, packages = "stats", dict_entry = "xonary")
    }
  ),
  private = list(
    .recombine = function(values) {
      p = self$param_set$get_values()$p

      if (is.matrix(p)) {
        choices = apply(p, 2, sample.int, n = nrow(values), size = 1, replace = TRUE)
      } else {
        choices = sample.int(nrow(values), ncol(values), replace = TRUE, prob = p)
      }
      setnames(setDT(Map(`[`, values, choices)), names(values))
    }
  )
)
dict_recombinators$add("xonary", RecombinatorCrossoverNary)
