#' @title Run Multiple Mutator Operations in Sequence
#'
#' @include Mutator.R
#'
#' @name dict_mutators_sequential
#'
#' @description
#' [`Mutator`] that wraps multiple other [`Mutator`]s given during construction and uses them for mutation in sequence.
#'
#' @section Configuration Parameters:
#' This operator has the configuration parameters of the [`Mutator`]s that it wraps: The configuration parameters of the operator given to the `mutators` construction
#' argument are prefixed with `"mutator_1"`, `"mutator_2"`, ... up to `"mutator_#"`, where `#` is `length(mutators)`.
#'
#' @templateVar id sequential
#' @templateVar additional , \<mutators\>
#' @template autoinfo_prepare_mut
#'
#' @section Supported Operand Types:
#'
#' Supported [`Param`][paradox::Param] classes are the set intersection of supported classes of the [`Mutator`]s given in `mutators`.
#'
#' @template autoinfo_dict
#'
#' @family mutators
#' @family mutator wrappers
#' @examples
#' # TODO examples
#' @export
MutatorSequential = R6Class("MutatorSequential",
  inherit = Mutator,
  public = list(
    #' @description
    #' Initialize the `MutatorSequential` object.
    #' @param mutators (`list` of [`Mutator`])\cr
    #'   [`Mutator`]s to wrap. The operations are run in order given to `mutators`.
    #'   The constructed object gets a *clone* of this argument. The `$mutators` field will reflect this value.
    initialize = function(mutators) {
      private$.wrapped = imap(unname(assert_list(mutators, types = "Mutator", min.len = 1)), function(x, i) {
        x$clone(deep = TRUE)
        x$param_set$set_id = sprintf("mutator_%s", i)
        x
      })

      ps_alist = lapply(seq_along(mutators), function(i) substitute(private$.wrapped[[i]], list(i = i)))

      super$initialize(Reduce(intersect, map(private$.wrapped, "param_classes")), ps_alist,
        packages = unique(unlist(map(private$.wrapped, "packages"), use.names = FALSE, recursive = FALSE)),
        dict_entry = "sequential")
    },
    #' @description
    #' See [`MiesOperator`] method. Primes both this operator, as well as the wrapped operators
    #' given to `mutator` and `mutator_not` during construction.
    #' @param param_set ([`ParamSet`][paradox::ParamSet])\cr
    #'   Passed to [`MiesOperator`]`$prime()`.
    #' @return [invisible] `self`.
    prime = function(param_set) {
      lapply(private$.wrapped, function(x) x$prime(param_set))
      super$prime(param_set)
      invisible(self)
    }
  ),
  active = list(
    #' @field mutators (`list` of [`Mutator`])\cr
    #' [`Mutator`]s being wrapped. These operators get run sequentially in order.
    mutators = function(val) {
      if (!missing(val)) stop("mutators is read-only.")
      private$.wrapped
    }
  ),
  private = list(
    .mutate = function(values, context) {
      for (m in private$.wrapped) {
        values = m$operate(values, context = context)
      }
      values
    },
    deep_clone = function(name, value) {
      if (name == ".wrapped") {
        lapply(value, function(x) x$clone(deep = TRUE))
      } else {
        super$deep_clone(name, value)
      }
    },
    .wrapped = NULL
  )
)
dict_mutators$add("sequential", MutatorMaybe)
