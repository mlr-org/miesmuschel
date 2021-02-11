#' @title Recombinator Base Class
#'
#' @include MiesOperator.R
#' @include dictionaries.R
#'
#' @description
#' Base class representing recombination operations, inheriting from [`MiesOperator`].
#'
#' Recombinators get a table of individuals as input and return a table of modified individuals as output. Individuals are acted on by
#' groups: every `$n_indivs_out` lines of output corresponds to a group of `$n_indivs_in` lines of input, and presence or absence
#' of other input groups does not affect the result.
#'
#' Recombination operations are performed in ES algorithms to facilitate exploration of the search space that combine partial
#' solutions.
#'
#' @section Inheriting:
#' `Recombinator` is an abstract base class and should be inherited from. Inheriting classes should implement the private `$.recombine()`
#' function. The user of the object calls `$operate()`, which calls `$.recombine()` for each `$n_indivs_in` sized group of individuals after checking that
#' the operator is primed, that the `values` argument conforms to the primed domain. `$.recombine()` should then return a table of
#' `$n_indivs_out` individuals for each call. Typically, the `$initialize()` function
#' should also be overloaded, and optionally the `$prime()` function; they should call their `super` equivalents.
#'
#' @family base classes
#' @family recombinators
#' @export
Recombinator = R6Class("Recombinator",
  inherit = MiesOperator,
  public = list(
    #' @description
    #' Initialize base class components of the `Recombinator`.
    #' @template param_param_classes
    #' @template param_param_set
    #' @param n_indivs_in (`integer(1)`)\cr
    #'   Number of individuals to consider at the same time. When operating, the number of input individuals must be divisible by this number.
    #'   Default 2.\cr
    #'   The `$n_indivs_in` field will reflect this value.
    #' @param n_indivs_out (`integer(1)`)\cr
    #'   Number of individuals that result for each `n_indivs_in` lines of input. The number of results from the recombinator will be
    #'   `nrow(values) / n_indivs_in * n_indivs_out`. Default equal to `n_indivs_in`.\cr
    #'   The `$n_indivs_out` field will reflect this value.
    initialize = function(param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set = ps(), n_indivs_in = 2, n_indivs_out = n_indivs_in) {
      assert_int(n_indivs_in, lower = 1, tol = 1e-100)
      assert_int(n_indivs_out, lower = 1, tol = 1e-100)
      private$.n_indivs_in = n_indivs_in
      private$.n_indivs_out = n_indivs_out
      super$initialize(param_classes, param_set)
    }
  ),
  active = list(
    #' @field n_indivs_in (`integer(1)`)\cr
    #' Number of individuals to consider at the same time. When operating, the number of input individuals must be divisible by this number.
    n_indivs_in = function(val) {
      if (!missing(val)) stop("n_indivs_in is read-only.")
      private$.n_indivs_in
    },
    #' @field n_indivs_out (`integer(1)`)\cr
    #' Number of individuals produced for each group of `$n_indivs_in` individuals.
    n_indivs_out = function(val) {
      if (!missing(val)) stop("n_indivs_out is read-only.")
      private$.n_indivs_out
    }
  ),
  private = list(
    .n_indivs_in = NULL,
    .n_indivs_out = NULL,
    .recombine = function(values) stop(".recombine needs to be implemented by inheriting class."),
    .operate = function(values) {
      assert_true(nrow(values) %% self$n_indivs_in == 0)
      rbindlist(
        lapply(split(values, rep(seq_len(nrow(values) / self$n_indivs_in), each = self$n_indivs_in)), function(vs) {
          vs = private$.recombine(vs)
          assert_data_table(vs, nrows = self$n_indivs_out)
        }), use.names = TRUE)
    }
  )
)

#' @title Null-Recombinator
#'
#' @name dict_recombinators_null
#'
#' @description
#' Null-recombinator that does not perform any operation on its input. Useful in particular with operator-wrappers such as [`RecombinatorMaybe`] or
#' [`RecombinatorCombination`].
#'
#' `n_indivs_in` and `n_indivs_out` can be set during construction, where `n_indivs_out` must be less or equal `n_indivs_in`. If it is strictly less,
#' then the operation returns only the first `n_indivs_out` individuals out of each `n_indivs_in` sized group.
#'
#' @section Hyperparameters:
#' This operator has no hyperparameters.
#'
#' @templateVar id null
#' @template autoinfo_prepare_rec
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family recombinators
#' @export
RecombinatorNull = R6Class("RecombinatorNull",
  inherit = Recombinator,
  public = list(
    #' @description
    #' Initialize base class components of the `Recombinator`.
    #' @param n_indivs_in (`integer(1)`)\cr
    #'   Number of individuals to consider at the same time. When operating, the number of input individuals must be divisible by this number.
    #'   Setting this number to a number unequal 1 is mostly useful when incorporating this operator in wrappers such as [`RecombinatorMaybe`] or
    #'   [`RecombinatorCombination`].
    #'   Default 1.\cr
    #'   The `$n_indivs_in` field will reflect this value.
    #' @param n_indivs_out (`integer(1)`)\cr
    #'   Number of individuals that result for each `n_indivs_in` lines of input. Must be at most `n_indivs_in`. If this is less than `n_indivs_in`,
    #'   then only the first `n_indivs_out` individuals out of each `n_indivs_in` sized group are returned by an operation.
    #'   Setting this number to a number unequal 1 is mostly useful when incorporating this operator in wrappers such as [`RecombinatorMaybe`] or
    #'   [`RecombinatorCombination`].
    #'   Default equal to `n_indivs_in`.\cr
    #'   The `$n_indivs_out` field will reflect this value.
    initialize = function(n_indivs_in = 1, n_indivs_out = n_indivs_in) {
      assert_int(n_indivs_out, lower = 1, tol = 1e-100)
      assert_int(n_indivs_in, lower = n_indivs_out, tol = 1e-100)
      super$initialize(n_indivs_in = n_indivs_in, n_indivs_out = n_indivs_out)
    }
  ),
  private = list(
    .recombine = function(values) first(values, self$n_indivs_out)
  )
)
dict_recombinators$add("null", RecombinatorNull)

#' @title Proxy-Recombinator that Recombines According to its Hyperparameter
#'
#' @description
#' Recombinator that performs the operation in its `operation` hyperparameter. This is useful, e.g., to make
#' [`OptimizerMies`]'s recombination operation fully parametrizable.
#'
#' Changes in the `operation` hyperparameter are only realized whenever `$prime()` is called, so `$prime()`
#' must be called every time when `operation` is changed.
#'
#' @section Hyperparameters
#' * `operation` :: [`Recombinator`]\cr
#'   Operation to perform. Initialized to [`RecombinatorNull`] with appropriate `n_indivs_in` and `n_indivs_out` values.
#'
#' @templateVar id proxy
#' @template autoinfo_prepare_rec
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family recombinators
#' @family recombinator wrappers
#' @export
RecombinatorProxy = R6Class("RecombinatorProxy",
  inherit = Recombinator,
  public = list(
    #' @description
    #' Initialize the `RecombinatorProxy` object.
    initialize = function(n_indivs_in = 2, n_indivs_out = n_indivs_in) {
      assert_int(n_indivs_out, lower = 1, tol = 1e-100)
      assert_int(n_indivs_in, lower = n_indivs_out, tol = 1e-100)

      param_set = ps(operation = p_uty(custom_check = crate(function(x) {
        if (test_r6(x, "Recombinator") && (n_indivs_in %% x$n_indivs_in == 0) && (n_indivs_out * x$n_indivs_in == x$n_indivs_out * n_indivs_in)) {
          return(TRUE)
        }
        sprintf("Must be a 'Recombinator' where n_indivs_in is a divisor of %s, and where n_indivs_in / n_indivs_out must be %s / %s",
          n_indivs_in, n_indivs_in, n_indivs_out)
      }, n_indivs_in, n_indivs_out))
      param_set$values = list(operation = RecombinatorNew$new(n_indivs_in = n_indivs_in, n_indivs_out = n_indivs_out))
      # call initialization with standard options: allow everything etc.
      super$initialize(param_set = param_set)
    },
    prime = function(param_set) {
      primed_with = self$param_set$get_vlaues()$operation
      operatio = primed_with$clone(deep = TRUE)
      operation$prime(param_set)
      super$prime(param_set)
      private$.operation = operation  # only change operation once everything else succeeded
      private$.primed_with = primed_with  # keep uncloned copy of hyperparameter value for check in `.recombine()`
      invisible(self)
    }
  ),
  private = list(
    .recombine = function(values) {
      if (!is.null(private$.primed_with) && !identical(self$param_set$get_values()$operation, private$.primed_with)) {
        # Unfortunately, when we clone, we can't keep track of self$param_set$values$operation.
        # In that case we ignore and hope the user is doing the right thing. After all, this message
        # is only to be informative.
        stop("RecombinatorProxy$prime() must be called again when the 'operation' hyperparameter changes.")
      }
      private$.operation$operate(values)
    },
    .operation = NULL,
    .primed_with = NULL,
    deep_clone = function(name, value) {
      if (name == ".primed_with") return(NULL)
      super$deep_clone(name, value)
    }
  )
)
dict_recombinators$add("proxy", RecombinatorProxy)

#' @title Recombinator Choosing Action Probabilistically
#'
#' @name dict_recombinators_maybe
#'
#' @description
#' [`Recombinator`] that chooses which operation to perform probabilistically. The [`Recombinator`] wraps two other [`Recombinator`]s given during construction,
#' and for each group of `$n_indivs_in` individuals, the operation to perform is sampled: with probability `p` (hyperparameter), the [`Recombinator`] given to
#' the `recombinator` construction argument is applied, and with probability `p - 1` the one given to `recombinator_not` is applied.
#'
#' The values of `$n_indivs_in` and `$n_indivs_out` is set to the corresponding values of the wrapped [`Recombinator`]s. Both `recombinator` and `recombinator_not`
#' must currently have the same respective `$n_indivs_in` and `$n_indivs_out` values.
#'
#' @section Hyperparameters:
#' This operator has the hyperparameters of the [`Recombinator`]s that it wraps: The hyperparameters of the operator given to the `recombinator` construction argument
#' are prefixed with `"maybe."`, the hyperparameters of the operator given to the `recombinator_not` construction argument are prefixed with `"maybe_not."`.
#'
#' Additional hyperparameters:
#' * `p` :: `numeric(1)` \cr
#'   Probability per group of `n_indivs_in` individuals with which to apply the operator given to the `recombinator` construction argument.
#'
#' @templateVar id maybe
#' @templateVar additional , <recombinator> \[, <recombinator_not>\]
#' @template autoinfo_prepare_rec
#' @section Supported Operand Types:
#'
#' Supported [`Param`][paradox::Param] classes are the set intersection of supported classes of `recombinator` and `recombinator_not`.
#'
#' @template autoinfo_dict
#'
#' @family recombinators
#' @family recombinator wrappers
#' @export
RecombinatorMaybe = R6Class("RecombinatorMaybe",
  inherit = Recombinator,
  public = list(
    #' @description
    #' Initialize the `RecombinatorMaybe` object.
    #' @param recombinator ([`Recombinator`])\cr
    #'   [`Recombinator`] to wrap. This operator gets run with probability `p` (Hyperparameter).\cr
    #'   The constructed object gets a *clone* of this argument.
    #' @param recombinator_not ([`Recombinator`])\cr
    #'   Another [`Recombinator`] to wrap. This operator runs when `recombinator` is not chosen. By
    #'   default, this is [`RecombinatorNull`], i.e. no operation, with both `n_indivs_in` and `n_indivs_out` set
    #'   to match `recombinator`. This does not work when `recombinator` has `n_indivs_in < n_indivs_out`, in which
    #'   case this argument must be set explicitly.
    #'   With the default behaviour, the `RecombinatorMaybe` object applies the `recombinator` operation with probability `p`, and
    #'   no operation at all otherwise.\cr
    #'   The constructed object gets a *clone* of this argument.
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

      private$.wrapped$param_set$set_id = "maybe"
      private$.wrapped_not$param_set$set_id = "maybe_not"

      private$.maybe_param_set = ps(p = p_dbl(0, 1, tags = "required"))
      private$.maybe_param_set$values = list(p = 1)
      super$initialize(recombinator$param_classes,
        alist(private$.maybe_param_set, private$.wrapped$param_set, private$.wrapped_not$param_set),
        recombinator$n_indivs_in, recombinator$n_indivs_out)
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
  private = list(
    .recombine = function(values) {
      if (runif(1) < self$param_set$get_values()$p) {
        private$.wrapped$operate(values)
      } else {
        private$.wrapped_not$operate(values)
      }
    },
    .wrapped = NULL,
    .wrapped_not = NULL,
    .maybe_param_set = NULL
  )
)
dict_recombinators$add("maybe", RecombinatorMaybe)


#' @title Crossover Recombinator
#'
#' @name dict_recombinators_xounif
#'
#' @description
#' Values between two individuals are exchanged with component-wise independent probability.
#'
#' @section Hyperparameters:
#' * `p` :: `numeric(1)`\cr
#'   Component-wise probability with which to exchange values. Initialized to 0.5.
#'
#' @templateVar id xounif
#' @template autoinfo_prepare_rec
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family recombinators
#' @export
RecombinatorCrossoverUniform = R6Class("RecombinatorCrossoverUniform",
  inherit = Recombinator,
  public = list(
    #' @description
    #' Initialize the `RecombinatorCrossoverUniform` object.
    #' @param keep_complement (`logical(1)`)\cr
    #'   Whether the operation should keep both individuals that were crossed over (`TRUE`), or only the first and discard
    #'   the crossover complement (`FALSE`). Default `TRUE`
    initialize = function(keep_complement = TRUE) {
      param_set = ps(p = p_dbl(0, tags = "required"))
      param_set$values = list(p = 0.5)
      super$initialize(c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set, 2, if (keep_complement) 2 else 1)
    }
  ),
  private = list(
    .recombine = function(values) {
      params = self$param_set$get_values()
      index = lapply(sample(1:2, length(values), TRUE, c(1 - params$p, params$p)), function(x) c(x, 3 - x))
      setnames(values[, pmap(list(.SD, index), `[`)][seq_len(self$n_indivs_out)], names(values))
    }
  )
)
dict_recombinators$add("xounif", RecombinatorCrossoverUniform)
