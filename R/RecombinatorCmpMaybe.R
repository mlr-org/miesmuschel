#' @title Recombinator Choosing Action Component-Wise Independently
#'
#' @include Recombinator.R
#'
#' @name dict_recombinators_maybe
#'
#' @description
#' [`Recombinator`] that chooses which operation to perform probabilistically and independently for each component.
#' The [`Recombinator`] wraps two other [`Recombinator`]s given during construction,
#' and both of these operators are run. The ultimate result is sampled from the results of these operations independently for each individuum
#' and component: with probability `p` (configuration parameter), the result from the [`Recombinator`] given to
#' the `recombinator` construction argument is used, and with probability `p - 1` the one given to `recombinator_not` is used.
#'
#' The values of `$n_indivs_in` and `$n_indivs_out` is set to the corresponding values of the wrapped [`Recombinator`]s. Both `recombinator` and `recombinator_not`
#' must currently have the same respective `$n_indivs_in` and `$n_indivs_out` values.
#'
#' @section Configuration Parameters:
#' This operator has the configuration parameters of the [`Recombinator`]s that it wraps: The configuration parameters of the operator given to the `recombinator` construction argument
#' are prefixed with `"cmpmaybe."`, the configuration parameters of the operator given to the `recombinator_not` construction argument are prefixed with `"cmpmaybe_not."`.
#'
#' Additional configuration parameters:
#' * `p` :: `numeric(1)` \cr
#'   Probability per component with which to use the result of applying the operator given to the `recombinator` construction argument. Must be set by the user.
#'
#' @templateVar id cmpmaybe
#' @templateVar additional , \<recombinator\> \[, \<recombinator_not\>\]
#' @template autoinfo_prepare_rec
#'
#' @section Supported Operand Types:
#'
#' Supported [`Param`][paradox::Param] classes are the set intersection of supported classes of `recombinator` and `recombinator_not`.
#'
#' @template autoinfo_dict
#'
#' @family recombinators
#' @family recombinator wrappers
#' @examples
#' set.seed(1)
#' rm = rec("cmpmaybe", rec("swap"), p = 0.5)
#' p = ps(x = p_int(1, 8), y = p_dbl(1, 8), z = p_lgl())
#' data = data.frame(x = 1:8, y = 1:8, z = rep(TRUE, 4))
#'
#' rm$prime(p)
#' rm$operate(data)
#'
#' rm$param_set$values$p = 0.3
#' rm$operate(data)
#'
#' # equivalent to rec("cmpmaybe", rec("swap", keep_complement = FALSE), p = 0.7)
#' rm2 = rec("cmpmaybe",
#'   recombinator = rec("null", 2, 1),
#'   recombinator_not = rec("swap", keep_complement = FALSE),
#'   p = 0.3
#' )
#'
#' rm2$prime(p)
#' rm2$operate(data)
#' @export
RecombinatorCmpMaybe = R6Class("RecombinatorCmpMaybe",
  inherit = Recombinator,
  public = list(
    #' @description
    #' Initialize the `RecombinatorCmpMaybe` object.
    #' @param recombinator ([`Recombinator`])\cr
    #'   [`Recombinator`] to wrap. Component-wise results of this operator are used with probability `p` (Configuration parameter).\cr
    #'   The constructed object gets a *clone* of this argument.
    #'   The `$recombinator` field will reflect this value.
    #' @param recombinator_not ([`Recombinator`])\cr
    #'   Another [`Recombinator`] to wrap. Results from this operator are used when `recombinator` is not chosen. By
    #'   default, this is [`RecombinatorNull`], i.e. no operation, with both `n_indivs_in` and `n_indivs_out` set
    #'   to match `recombinator`. This does not work when `recombinator` has `n_indivs_in < n_indivs_out`, in which
    #'   case this argument must be set explicitly.\cr
    #'   With this default, the `RecombinatorCmpMaybe` object applies the `recombinator` operation with probability `p`, and
    #'   no operation at all otherwise.\cr
    #'   The constructed object gets a *clone* of this argument.
    #'   The `$recombinator_not` field will reflect this value.
    initialize = function(recombinator, recombinator_not = NULL) {
      private$.wrapped = assert_r6(recombinator, "Recombinator")$clone(deep = TRUE)
      if (is.null(recombinator_not)) {
        private$.wrapped_not = RecombinatorNull$new(recombinator$n_indivs_in, recombinator$n_indivs_out)
      } else {
        private$.wrapped_not = assert_r6(recombinator_not, "Recombinator")$clone(deep = TRUE)
      }
      if (private$.wrapped$n_indivs_in != private$.wrapped_not$n_indivs_in ||
          private$.wrapped$n_indivs_out != private$.wrapped_not$n_indivs_out) {
        stop("recombinator and recombinator_not must have the same number of in / out individuals.")
      }

      private$.wrapped$param_set$set_id = "cmpmaybe"
      private$.wrapped_not$param_set$set_id = "cmpmaybe_not"

      private$.maybe_param_set = ps(p = p_vct(lower = 0, upper = 1, tags = "required"))
      super$initialize(recombinator$param_classes,
        alist(private$.maybe_param_set, private$.wrapped$param_set, private$.wrapped_not$param_set),
        recombinator$n_indivs_in, recombinator$n_indivs_out,
        packages = c("stats", recombinator$packages, recombinator_not$packages), dict_entry = "cmpmaybe",
        own_param_set = quote(private$.maybe_param_set))
    },
    #' @description
    #' See [`MiesOperator`] method. Primes both this operator, as well as the wrapped operators
    #' given to `recombinator` and `recombinator_not` during construction.
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
    #' @field recombinator ([`Recombinator`])\cr
    #' [`Recombinator`] being wrapped. This operator gets run with probability `p` (configuration parameter).
    recombinator = function(val) {
      if (!missing(val)) stop("recombinator is read-only.")
      private$.wrapped
    },
    #' @field recombinator_not ([`Recombinator`])\cr
    #' Alternative [`Recombinator`] being wrapped. This operator gets run with probability `1 - p` (configuration parameter).
    recombinator_not = function(val) {
      if (!missing(val)) stop("recombinator_not is read-only.")
      private$.wrapped_not
    }
  ),
  private = list(
    .recombine = function(values) {
      recombined = private$.wrapped$operate(values)
      recombined_not = private$.wrapped_not$operate(values)
      p = private$.maybe_param_set$get_values()$p
      p = pmin(pmax(p, 0), 1)
      if (!length(p) %in% c(1, ncol(values))) stop("p must have either length 1, or length of input.")
      recombining = stats::runif(ncol(values)) < p
      setnames(setDT(ifelse(recombining, recombined, mutated_not)), names(values))
    },
    .wrapped = NULL,
    .wrapped_not = NULL,
    .maybe_param_set = NULL
  )
)
dict_recombinators$add("cmpmaybe", RecombinatorCmpMaybe)
