#' @title Mutator Choosing Action Component-Wise Independently
#'
#' @include Mutator.R
#'
#' @name dict_mutators_cmpmaybe
#'
#' @description
#' [`Mutator`] that chooses which operation to perform probabilistically. The [`Mutator`] wraps two other [`Mutator`]s given during construction,
#' and both of these operators are run. The ultimate result is sampled from the results of these operations independently for each
#' individuum and component: with probability `p` (configuration parameter), the result from the [`Mutator`] given to the `mutator`
#' construction argument is used, and with probability `p - 1` the one given to `mutator_not` is used.
#'
#' @section Configuration Parameters:
#' This operator has the configuration parameters of the [`Mutator`]s that it wraps: The configuration parameters of the operator given to the `mutator` construction argument
#' are prefixed with `"cmpmaybe."`, the configuration parameters of the operator given to the `mutator_not` construction argument are prefixed with `"cmpmaybe_not."`.
#'
#' Additional configuration parameters:
#' * `p` :: `numeric(1)` \cr
#'   Probability per component with which to apply the operator given to the `mutator` construction argument. Must be set by the user.
#'
#' @templateVar id cmpmaybe
#' @templateVar additional , \<mutator\> \[, \<mutator_not\>\]
#' @template autoinfo_prepare_mut
#' @section Supported Operand Types:
#'
#' Supported [`Domain`][paradox::Domain] classes are the set intersection of supported classes of `mutator` and `mutator_not`.
#'
#' @template autoinfo_dict
#'
#' @family mutators
#' @family mutator wrappers
#' @examples
#' set.seed(1)
#' mcm = mut("cmpmaybe", mut("gauss", sdev = 5), p = 0.5)
#' p = ps(x = p_int(-5, 5), y = p_dbl(-5, 5))
#' data = data.frame(x = rep(0, 5), y = rep(0, 5))
#'
#' mcm$prime(p)
#' mcm$operate(data)
#'
#' mcm$param_set$values$p = 0.2
#' mcm$operate(data)
#'
#' mcm2 = mut("cmpmaybe",
#'   mutator = mut("gauss", sdev = 0.01),
#'   mutator_not = mut("gauss", sdev = 10),
#'   p = 0.5
#' )
#'
#' mcm2$prime(p)
#' mcm2$operate(data)
#' @export
MutatorCmpMaybe = R6Class("MutatorCmpMaybe",
  inherit = Mutator,
  public = list(
    #' @description
    #' Initialize the `MutatorCmpMaybe` object.
    #' @param mutator ([`Mutator`])\cr
    #'   [`Mutator`] to wrap. Component-wise results of this operator are used with probability `p` (Configuration parameter).\cr
    #'   The constructed object gets a *clone* of this argument.
    #'   The `$mutator` field will reflect this value.
    #' @param mutator_not ([`Mutator`])\cr
    #'   Another [`Mutator`] to wrap. Results from this operator are used when `mutator` is not chosen. By
    #'   default, this is [`MutatorNull`], i.e. no operation.\cr
    #'   With this default, the
    #'   `MutatorCmpMaybe` object applies the `mutator` operation with probability `p`, and
    #'   no operation at all otherwise.\cr
    #'   The constructed object gets a *clone* of this argument.
    #'   The `$mutator_not` field will reflect this value.
    initialize = function(mutator, mutator_not = MutatorNull$new()) {
      private$.wrapped = assert_r6(mutator, "Mutator")$clone(deep = TRUE)
      private$.wrapped_not = assert_r6(mutator_not, "Mutator")$clone(deep = TRUE)

      if (!paradox_s3) {
        private$.wrapped$param_set$set_id = "cmpmaybe"
        private$.wrapped_not$param_set$set_id = "cmpmaybe_not"
      }
      private$.maybe_param_set = ps(p = p_vct(lower = 0, upper = 1, tags = "required"))
      super$initialize(intersect(mutator$param_classes, mutator_not$param_classes),
        alist(private$.maybe_param_set, cmpmaybe = private$.wrapped$param_set, cmpmaybe_not = private$.wrapped_not$param_set),
        packages = c("stats", mutator$packages, mutator_not$packages), dict_entry = "cmpmaybe",
        own_param_set = quote(private$.maybe_param_set))
    },
    #' @description
    #' See [`MiesOperator`] method. Primes both this operator, as well as the wrapped operators
    #' given to `mutator` and `mutator_not` during construction.
    #' @param param_set ([`ParamSet`][paradox::ParamSet])\cr
    #'   Passed to [`MiesOperator`]`$prime()`.
    #' @return [invisible] `self`.
    prime = function(param_set) {
      private$.wrapped$prime(param_set)
      private$.wrapped_not$prime(param_set)
      super$prime(param_set)
      invisible(self)
    }
  ),
  active = list(
    #' @field mutator ([`Mutator`])\cr
    #' [`Mutator`] being wrapped. This operator gets run with probability `p` (configuration parameter).
    mutator = function(val) {
      if (!missing(val)) stop("mutator is read-only.")
      private$.wrapped
    },
    #' @field mutator_not ([`Mutator`])\cr
    #' Alternative [`Mutator`] being wrapped. This operator gets run with probability `1 - p` (configuration parameter).
    mutator_not = function(val) {
      if (!missing(val)) stop("mutator_not is read-only.")
      private$.wrapped_not
    }
  ),
  private = list(
    .mutate = function(values) {
      mutated = private$.wrapped$operate(values)
      mutated_not = private$.wrapped_not$operate(values)
      p = private$.maybe_param_set$get_values()$p
      p = pmin(pmax(p, 0), 1)
      if (!length(p) %in% c(1, ncol(values))) stop("p must have either length 1, or length of input.")
      mutating = sweep(matrix(stats::runif(nrow(values) * ncol(values)), nrow = nrow(values)), 2, p, `<`)
      setDT(mapply(function(mutnot, mut, takemut) {
        mutnot[takemut] <- mut[takemut]
        mutnot
      }, mutated_not, mutated, as.data.frame(mutating), SIMPLIFY = FALSE))
    },
    .wrapped = NULL,
    .wrapped_not = NULL,
    .maybe_param_set = NULL
  )
)
dict_mutators$add("cmpmaybe", MutatorCmpMaybe, aux_construction_args = alist(mutator = MutatorNull$new()))
