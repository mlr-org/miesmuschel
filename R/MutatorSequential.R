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
#' set.seed(1)
#'
#' # dataset:
#' #  - x1 is mutated around +- 10
#' #  - x2 influences sdev of mutation of x1
#' ds = data.frame(x1 = 0, x2 = c(.01, 0.1, 1))
#' p = ps(x1 = p_dbl(-10, 10), x2 = p_dbl(0, 10))
#'
#' # operator that only mutates x1, with sdev given by x2
#' gauss_x1 = mut("combine",
#'   operators = list(
#'     x1 = mut("gauss", sdev_is_relative = FALSE),
#'     x2 = mut("null")
#'   ),
#'   adaptions = list(x1.sdev = function(x) x$x2)
#' )
#'
#' gauss_x1$prime(p)
#' gauss_x1$operate(ds)  # see how x1[1] changes little, x1[3] changes a lot
#'
#' # operator that mutates x1  with sdev given by x2, as well as x2. However,
#' # the value that x2 takes after mutation does not influence the value that
#' # the mutator of x1 "sees" -- although x2 is mutated to extreme values,
#' # mutation of x1 happens as in `gauss_x1`.
#' gauss_x1_x2 = mut("combine",
#'   operators = list(
#'     x1 = mut("gauss", sdev_is_relative = FALSE),
#'     x2 = mut("gauss", sdev = 100)
#'   ),
#'   adaptions = list(x1.sdev = function(x) x$x2)
#' )
#'
#' gauss_x1_x2$prime(p)
#' gauss_x1_x2$operate(ds)  # see how x1 changes in similar ways to above
#'
#' # operator that mutates sequentially: first x2, and then x1 with sdev given
#' # by x2. The value that x2 takes after mutation *does* influence the value
#' # that the mutator of x1 "sees": x1 is mutated either to a large degree,
#' # or not at all.
#'
#' gauss_x2_then_x1 = mut("sequential", list(
#'     mut("combine",
#'       operators = list(
#'         x1 = mut("null"),
#'         x2 = mut("gauss", sdev = 100)
#'       )
#'     ),
#'     mut("combine",
#'       operators = list(
#'         x1 = mut("gauss", sdev_is_relative = FALSE),
#'         x2 = mut("null")
#'       ),
#'       adaptions = list(x1.sdev = function(x) x$x2)
#'     )
#' ))
#'
#' gauss_x2_then_x1$prime(p)
#' gauss_x2_then_x1$operate(ds)
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
        x = x$clone(deep = TRUE)
        x$param_set$set_id = sprintf("mutator_%s", i)
        x
      })

      ps_alist = lapply(seq_along(mutators), function(i) substitute(private$.wrapped[[i]]$param_set, list(i = i)))

      super$initialize(Reduce(intersect, map(private$.wrapped, "param_classes")), ps_alist,
        packages = unique(unlist(map(private$.wrapped, "packages"), use.names = FALSE, recursive = FALSE)),
        dict_entry = "sequential", own_param_set = quote(ps()))
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
    .mutate = function(values) {
      for (m in private$.wrapped) {
        values = m$operate(values)
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
dict_mutators$add("sequential", MutatorSequential, aux_construction_args = alist(mutators = list(MutatorNull$new())))
