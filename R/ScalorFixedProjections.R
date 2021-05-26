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
#'   and returns a matrix of scalarizations (Nindivs x Nweights, with higher values indicating greater desirability)
#'
#' @param weights_component_id (`character(1)`)\cr
#'   Id of the search space component identifying the weights by which to scalarize. Default `"scalarization_weights"`.
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
    initialize = function(weights_component_id = "scalarization_weights") {
      private$.weight_id = assert_string(weights_component_id)
      param_set = ps(scalarization = p_uty(custom_check = function(x) check_function(x, args = c("fitnesses", "weights"))))
      param_set$values = list(scalarization = scalarizer_linear())
      super$initialize(param_set = param_set, dict_entry = "fixedprojection")
    },
    #' @description
    #' See [`MiesOperator`] method. Primes both this operator, as well as the operator given to the `operation` configuration parameter.
    #'   Note that this modifies the `$param_set$values$operation` object.
    #' @param param_set ([`ParamSet`][paradox::ParamSet])\cr
    #'   Passed to [`MiesOperator`]`$prime()`.
    #' @return [invisible] `self`.
    prime = function(param_set) {
      if (private$.weight_id %nin% param_set$ids()) stopf("Need the scalarization_weights parameter '%s' for ScalorFixedProjection.", private$.weight_id)
      if (param_set$class[[private$.weight_id]] != "ParamUty") stopf("scalarization weights parameter must be ParamUty but is %s", param_set$class[[private$.weight_id]])
      super$prime(param_set)
    }
  ),
  active = list(
    #' @field weights_component_id (`numeric(1)`)\cr
    #' search space component identifying the weights by which to scalarize.
    weights_component_id = function(rhs) {
      if (!missing(rhs)) stop("weights_component_id is read-only.")
      private$.weight_id
    }
  ),
  private = list(
    .scale = function(values, fitnesses, context) {
      scalarization = self$param_set$get_values(context = context)$scalarization

      weights = values[[private$.weight_id]]
      lapply(weights, assert_matrix, mode = "numeric", nrows = ncol(fitnesses), ncols = ncols(weights[[1]]))  # scalor asserts that values has at least one row

      scaled = scalarization(fitnesses, weights)

      apply(scaled, 1, max)
    },
    .weight_id = NULL
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
      assert_matrix(fitnesses, mode = "numeric")
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

#' @title Sampler for Projection Weights
#'
#' @description
#' Sampler for a single [`ParamUty`][paradox::ParamUty] that samples weight-matrices
#' as used by [`ScalorFixedProjection`].
#'
#' @param nobjectives (`numeric(1)`)\cr
#'   Number of objectives for which weights are generated.
#' @param nweights (`numeric(1)`)\cr
#'   Number of weight vectors generated for each configuration.
#' @param weights_component_id (`character(1)`)\cr
#'   Id of the [`ParamUty`][paradox::ParamUty]. Default is `"scalarization_weights"`.
#'   Can be changed arbitrarily but should match the [`ScalorFixedProjection`]'s `weights_component_id`.
#' @examples
#' set.seed(1)
#' @export
SamplerRandomWeights = R6Class("SamplerRandomWeights", inherit = paradox::Sampler,
  public = list(
    #' @description
    #' Initialize the `SamplerRandomWeights` object.
    initialize = function(nobjectives = 2, nweights = 1, weights_component_id = "scalarization_weights") {
      private$.nweights = assert_count(nweights, tol = 1e-100, positive = TRUE)
      private$.nobjectives = assert_count(nobjectives, tol = 1e-100, positive = TRUE)
      assert_string(weights_component_id)
      # the tag must always be 'scalarization_weights', but the name of the component can be changed
      param_set = do.call(ps,
        structure(list(p_uty(tags = "scalarization_weights", custom_check = check_matrix)),
          names = weights_component_id)
      )
      super$initialize(param_set)
    }
  ),
  active = list(
    #' @field nobjectives (`numeric(1)`)\cr
    #' Number of objectives for which weights are generated.
    nobjectives = function(rhs) {
      if (!missing(rhs)) stop("nobjectives is read-only.")
      private$.nobjectives
    },
    #' @field nweights (`numeric(1)`)\cr
    #' Number of weight vectors generated for each configuration.
    nweights = function(rhs) {
      if (!missing(rhs)) stop("nweights is read-only.")
      private$.nweights
    },
    #' @field weights_component_id (`numeric(1)`)\cr
    #' search space component identifying the weights by which to scalarize.
    weights_component_id = function(rhs) {
      if (!missing(rhs)) stop("weights_component_id is read-only.")
      self$param_set$ids()
    }
  ),
  private = list(
    .sample = function(n) {
      if (n == 0) list()
      nweights = private$.nweights
      nobjectives = private$.nobjectives
      matrixsize = nweights * nobjectives
      matrices = replicate(n, simplify = FALSE, {
        mat = matrix(rexp(matrixsize), ncol = nobjectives, nrow = nweights)
        normalizer = rowSums(mat)
        t(mat / normalizer)
      })
      setNames(data.table(matrices), self$weights_component_id)
    },
    .nweights = NULL,
    .nobjectives = NULL
  )
)
