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
#' *  `n` :: `double`\cr
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
    #' @param keep_complement (`logical(1)`)\cr
    #'   Whether the operation should keep both individuals that were crossed over (`TRUE`), or only the first and discard
    #'   the crossover complement (`FALSE`). Default `TRUE`.
    #'   The `$keep_complement` field will reflect this value.
    initialize = function(keep_complement = TRUE) {
      param_set = ps(
        p = p_dbl(lower = 0, upper = 1, tags = "required"),
        n = p_vct(lower = 0, tags = "required")
      )
      param_set$values = list(p = 0.5, n = 1)
      super$initialize(c("ParamDbl", "ParamInt"), param_set = param_set, 
        n_indivs_in = 2, n_indivs_out = if (keep_complement) 2 else 1, dict_entry = "sbx")
    }
  ),
  active = list(
    #' @field keep_complement (`logical(1)`)\cr
    #' Whether the operation keeps both individuals that were crossed over or discards the crossover complement.
    keep_complement = function(val) {
      if (!missing(val)) stop("keep_complement is read-only.")
      private$.n_indivs_out == 2
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
      is_nn_int = private$.primed_ps$class == "ParamInt"
      
      lower[is_nn_int] = lower[is_nn_int] - 0.5
      upper[is_nn_int] = upper[is_nn_int] + 0.5

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
      if (any(is_nn_int)) {
        values <- as.matrix(values)
        ### This is slow:
        # rounded = round(values[, is_nn_int])
        # rounded = sweep(rounded, 2, uppers[is_nn_int], pmin)
        # rounded = sweep(rounded, 2, lowers[is_nn_int], pmax)
        ### This is faster and does the same:
        values[, is_nn_int] = t(pmax(pmin(t(round(values[, is_nn_int])), upper[is_nn_int]), lower[is_nn_int]))
        values = as.data.table(values)
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

