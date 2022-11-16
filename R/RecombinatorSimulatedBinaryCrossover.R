#' @title Simulated Binary Crossover Recombinator
#'
#' @include Recombinator.R
#'
#' @name dict_recombinators_sbx
#'
#' @description
#' Numeric Values between two individuals are recombined via component-wise independent simulated
#' binary crossover. See `r cite_bib("deb1995simulated")` for more details.
#'
#' This operator is applied to all components; It is common to apply the operator to only some randomly
#' chosen components, in which case the [`rec("cmpmaybe")`][RecombinatorCmpMaybe] operator should
#' be used; see examples.
#'
#' @section Configuration Parameters:
#' *  `n` :: `numeric`\cr
#'  Non-negative distribution index of the polynomial distribution for each component.
#'  Generally spoken, the higher `n`, the higher the probability of creating near parent values.
#'  This may either be a scalar in which case it is applied to all input components, or a vector,
#'  in which case it must have the length of the input components and applies to components in
#'  order in which they appear in the priming [`ParamSet`][paradox::ParamSet]. Initialized to 1.
#'
#' @templateVar id sbx
#' @template autoinfo_prepare_rec
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family recombinators
#'
#' @references
#' `r format_bib("deb1995simulated")`
#'
#' @examples
#' set.seed(1)
#' rsbx = rec("cmpmaybe", rec("sbx"), p = 0.5)
#' p = ps(x = p_dbl(-5, 5), y = p_dbl(-5, 5), z = p_dbl(-5, 5))
#' data = data.frame(x = 0:5, y = 0:5, z = 0:5)
#'
#' rsbx$prime(p)
#' rsbx$operate(data)
#'
#' rsbx = rec("sbx", n = c(0.5, 1, 10))
#' rsbx$prime(p)
#' rsbx$operate(data)
#' @export
RecombinatorSimulatedBinaryCrossover = R6Class("RecombinatorSimulatedBinaryCrossover",
  inherit = RecombinatorPair,
  public = list(
    #' @description
    #' Initialize the `RecombinatorSimulatedBinaryCrossover` object.
    #' @template param_keep_complement
    initialize = function(keep_complement = TRUE) {
      param_set = ps(
        n = p_vct(lower = 0, tags = "required")
      )
      param_set$values = list(n = 1)
      super$initialize(keep_complement, c("ParamInt", "ParamDbl"), param_set = param_set, packages = "stats", dict_entry = "sbx")
    }
  ),
  private = list(
    .recombine = function(values) {
      n_components = ncol(values)
      params = self$param_set$get_values()
      n = params$n
      if (length(n) == 1L) {
        n = rep(n, n_components)
      }
      if (length(n) != n_components) {
        stop("n must have either length 1, or length of input components.")
      }
      affecting <- abs(values[2] - values[1]) > sqrt(.Machine$double.eps)
      if (!any(affecting)) return(values)
      nms = names(values)[affecting]
      n = n[affecting]
      names(n) = nms
      lower = self$primed_ps$lower[nms]
      upper = self$primed_ps$upper[nms]
      is_nn_int = which(private$.primed_ps$class[nms] == "ParamInt")  # data.table assignment needs indices

      lower[is_nn_int] = lower[is_nn_int] - 0.5
      upper[is_nn_int] = upper[is_nn_int] + 0.5

      set(values, , nms, imap(values[, nms, with = FALSE], .f = function(x, name) {
        y1 = min(x)
        y2 = max(x)
        betaq1 = calculate_betaq(1 + (2 * (y1 - lower[name]) / (y2 - y1)), n[name])
        c1 = pmin(pmax(0.5 * ((y1 + y2) - betaq1 * (y2 - y1)), lower[name]), upper[name])
        betaq2 = calculate_betaq(1 + (2 * (upper[name] - y2) / (y2 - y1)), n[name])
        c2 = pmin(pmax(0.5 * ((y1 + y2) + betaq2 * (y2 - y1)), lower[name]), upper[name])
        if (stats::runif(1L, min = 0, max = 1) <= 0.5) {  # TODO: check if this is the way it should be
          c(c1, c2)
        } else {
          c(c2, c1)
        }
      }))
      if (length(is_nn_int)) {

        values_mat <- as.matrix(values[, nms[is_nn_int], with = FALSE])
        ### This is slow:
        # rounded = round(values[, is_nn_int])
        # rounded = sweep(rounded, 2, uppers[is_nn_int], pmin)
        # rounded = sweep(rounded, 2, lowers[is_nn_int], pmax)
        ### This is faster and does the same:
        values_mat = t(pmax(pmin(t(round(values_mat)), upper[is_nn_int] - 0.5), lower[is_nn_int] + 0.5))
        values[, nms[is_nn_int]]  = as.data.table(values_mat)
      }
      values
    }
  )
)
dict_recombinators$add("sbx", RecombinatorSimulatedBinaryCrossover)

calculate_betaq = function(beta, n) {
  r = stats::runif(1L, min = 0, max = 1)
  alpha = 2 - (beta ^ (- (n + 1)))
  betaq = if (r <= (1 / alpha)) {
    (r * alpha) ^ (1 / (n + 1))
  } else {
    (1 / (2 - r * alpha)) ^ (1 / (n + 1))
  }
  betaq
}

