#' @title Convex Combination Recombinator
#'
#' @include Recombinator.R
#'
#' @name dict_recombinators_convex
#'
#' @description
#' Numeric Values between various individuals are recombined via component-wise convex combination (or weighted mean). The number of individuals
#' over which the convex combination is taken must be determined during construction as `n_indivs_in`.
#'
#' The number of output individuals is always 1, i.e. `n_indivs_in` are used to create one output value. When using this
#' recombinator in a typical EA setting, e.g. with [`mies_generate_offspring`], it is therefore recommended to use a parent-selector
#' where the expected quality of selected parents does not depend on the number of parents selected when `n_indivs_in` is large:
#' [`sel("tournament")`][SelectorTournament] is preferred to [`sel("best")`][SelectorBest].
#'
#' @section Configuration Parameters:
#' *  `lambda` :: `numeric` | `matrix`\cr
#'   Combination weights. Must either be a vector of length `n_indivs_in`, or a matrix with `n_indivs_in` rows and as many columns as there
#'   are components in the values being operated on. Must be non-negative, at least one value per column must be greater than zero.\cr
#'   Initialized to `rep(1, n_indivs_in)`, i.e. equal weights to all individuals being operated on.
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
#' rcvx = rec("convex", n_indivs_in = 3)
#' p = ps(x = p_dbl(-5, 5), y = p_dbl(-5, 5), z = p_dbl(-5, 5))
#' data = data.frame(x = 0:5, y = 0:5, z = 0:5)
#'
#' rcvx$prime(p)
#' rcvx$operate(data)  # mean of groups of 3
#'
#' rcvx = rec("convex", 3, lambda = c(0, 1, 2))$prime(p)
#' rcvx$operate(data)  # for groups of 3, take 1/3 of 2nd and 2/3 of 3rd row
#'
#' lambda = matrix(c(0, 1, 2, 1, 1, 1, 1, 0, 0, ncol = 3)
#' lambda
#'
#' rcvx = rec("convex", 3, lambda = lambda)$prime(p)
#' rcvx$operate(data)  # componentwise different operation
#'
#' @export
RecombinatorConvex = R6Class("RecombinatorConvex",
  inherit = Recombinator,
  public = list(
    #' @description
    #' Initialize the `RecombinatorConvex` object.
    #' @template param_n_indivs_in
    initialize = function(n_indivs_in = 2) {
      param_set = ps(
        lambda = p_mtx(rows = n_indivs_in, lower = 0, tags = "required")
      )
      param_set$values = list(lambda = rep(1, n_indivs_in))
      super$initialize("ParamDbl", param_set = param_set, n_indivs_in = n_indivs_in, n_indivs_out = 1, packages = "stats", dict_entry = "convex")
    }
  ),
  private = list(
    .recombine = function(values) {
      lambda = self$param_set$get_values()$lambda

      if (is.matrix(lambda)) {
        reslist = setnames(Map(weighted.mean, values, as.data.frame(lambda)), names(values))
      } else {
        reslist = lapply(values, weighted.mean, w = lambda)
      }
      setDT(reslist)
    }
  )
)
dict_recombinators$add("convex", RecombinatorConvex)
