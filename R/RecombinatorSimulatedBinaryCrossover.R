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
#' @section Configuration Parameters:
#' * `p` :: `numeric(1)`\cr
#'  Component-wise probability with which to crossover values. Initialized to 0.5.
#' *  `n` :: `double()`\cr
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
#' @examples
#' set.seed(1)
#' rsbx = rec("sbx")
#' p = ps(x = p_dbl(-5, 5), y = p_dbl(-5, 5), z = p_dbl(-5, 5))
#' data = data.frame(x = 0:5, y = 0:5, z = 0:5)
#'
#' rsbx$prime(p)
#' rsbx$operate(data)
#'
#' rsbx$param_set$values$n = c(0.5, 1, 10)
#' rsbx$param_set$values$p = 1
#' rsbx$operate(data)
#' @export
RecombinatorSimulatedBinaryCrossover = R6Class("RecombinatorSimulatedBinaryCrossover",
  inherit = Recombinator,
  public = list(
    #' @description
    #' Initialize the `RecombinatorSimulatedBinaryCrossover` object.
    initialize = function(keep_complement = TRUE) {
      param_set = ps(p = p_dbl(lower = 0, upper = 1, tags = "required"),
        n = p_uty(custom_check = crate(function(x) {
          check_double(x, lower = 0L, any.missing = FALSE, min.len = 1L)
        }, .parent = topenv()), tags = "required"))
      param_set$values = list(p = 0.5, n = 1)
      super$initialize("ParamDbl", param_set = param_set, n_indivs_in = 2L, n_indivs_out = 2L)
    }
  ),
  private = list(
    .recombine = function(values) {
      n_components = NCOL(values)
      params = self$param_set$get_values()
      p = params$p
      n = params$n
      if (length(n) == 1L) {
        n = rep(n, n_components)
      }
      if (length(n) != n_components) {
        stop("n must have either length 1, or length of input components.")
      }
      nms = names(values)
      names(n) = nms
      lower = self$primed_ps$lower[nms]
      upper = self$primed_ps$upper[nms]

      values[, (nms) := imap(.SD, .f = function(x, name) {
        if (stats::runif(1L, min = 0, max = 1) <= p && abs(diff(x)) > sqrt(.Machine$double.eps)) {
          y1 = min(x)
          y2 = max(x)
          betaq1 = calculate_betaq(1 + (2 * (y1 - lower[name]) / (y2 - y1)), n[name])
          c1 = pmin(pmax(0.5 * ((y1 + y2) - betaq1 * (y2 - y1)), lower[name]), upper[name])
          betaq2 = calculate_betaq(1 + (2 * (upper[name] - y2) / (y2 - y1)), n[name])
          c2 = pmin(pmax(0.5 * ((y1 + y2) + betaq2 * (y2 - y1)), lower[name]), upper[name])
          if (stats::runif(1L, min = 0, max = 1) <= 0.5) {
            c(c1, c2)
          } else {
            c(c2, c1)
          }
        } else {
          x
        }
      })]
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

