#' @title Multi-Objective Fixed Projection Scalor
#'
#' @include Scalor.R
#'
#' @description
#' [`Scalor`] that returns the maximum of a set of projections.
#'
#' Priming PS must contain a `"scalarization_weights"` tagged [`ParamUty`][paradox::ParamUty] that contains
#' weight matrices (Nobjectives x Nweights) or vectors (if Nweights is 1).
#'
#' @section Configuration Parameters:
#' * `scalarization` :: [`function`]\cr
#'   Function taking a fitness-matrix `fitnesses` (Nindivs x Nobjectives, with higher values indicating higher desirability)
#'   and a list of weight matrices `weights` (Nindivs elements of Nobjectives x Nweights matrices; positive weights should indicate a positive contribution
#'   to scale)
#'   and returns a matrix of scalarizations (Nindivs x Nweights, with higher values indicating greater desirability; possibly
#'   an Nindivs vector when Nweights is 1)
#'
#' @templateVar id fixedprojection
#' @template autoinfo_prepare_scl
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family scalors
#' @family scalor wrappers
#' @examples
#' set.seed(1)
#' @export
ScalorFixedProjection = R6Class("ScalorFixedProjection",
  inherit = Scalor,
  public = list(
    #' @description
    #' Initialize the `ScalorFixedProjection` object.
    initialize = function() {
      param_set = ps(scalarization = p_uty(custom_check = function(x) check_function(x, args = c("fitnesses", "weights"))))
      param_set$values = list(scalarization = scalarize_linear())
      super$initialize(param_set = param_set, dict_entry = "fixedprojection")
    },
    #' @description
    #' See [`MiesOperator`] method. Primes both this operator, as well as the operator given to the `operation` configuration parameter.
    #'   Note that this modifies the `$param_set$values$operation` object.
    #' @param param_set ([`ParamSet`][paradox::ParamSet])\cr
    #'   Passed to [`MiesOperator`]`$prime()`.
    #' @return [invisible] `self`.
    prime = function(param_set) {
      weight_id = param_set$ids(tags = "scalarization_weights")
      if (length(weight_id) != 1) stopf("Need exactly one scalarization_weights parameter for ScalorFixedProjection, but found %s: %s",
        length(weight_id), str_collapse(weight_id))
      if (param_set$class[[weight_id]] != "ParamUty") stopf("scalarization_weights parameter must be ParamUty but is %s", param_set$class[[weight_id]])
      super$prime(param_set)
    }
  ),
  private = list(
    .scale = function(values, fitnesses, context) {
      scalarization = self$param_set$get_values(context = context)$scalarization

    }
  )
)
dict_scalors$add("fixedprojection", ScalorFixedProjection)


make_scalarizer = function(name, fn) {
  assert_function(fn, args = c("fitnesses", "weights"))
  assert_string(name)

  cl = match.call(sys.function(-1), sys.call(-1), envir = parent.frame(2))
  for (n in names2(cl)) if (!is.na(n)) cl[[n]] = get(n, pos = parent.frame(1))
  cl[[1]] = as.symbol(name)
  repr = str_collapse(c(capture.output(print(cl)), ""), sep = "\n")

  structure(function(fitnesses, weights) {
      assert_matrix(fitnesses)
      assert_list(weights, len = nrow(fitnesses))
      matrix(fn(fitnesses, weights), nrow = nrow(fitnesses))
    },
    repr = repr,
    class = c("Scalarizer", "function")
  )
}

#' @export
print.Scalarizer = function(x, ...) {
  cat(attr(x, "repr"))
}


#' @export
scalarizer_linear = function() {
  make_scalarizer("scalarize_linear", function(fitnesses, weights) {
    t(mapply(`%*%`, asplit(fitnesses, 1), weights))
  })
}

#' @export
scalarizer_chebyshev = function(rho = 0.05) {
  assert_number(rho)
  make_scalarizer("scalarize_chebyshev", function(fitnesses, weights) {
    t(mapply(function(f, w) {
      apply(w * c(f), 2, min) + rho * f %*% w
    }, asplit(fitnesses, 1), weights))
  })
}

