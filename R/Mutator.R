#' @title Mutator Base Class
#'
#' @include MiesOperator.R
#' @include dictionaries.R
#'
#' @description
#' Base class representing mutation operations, inheriting from [`MiesOperator`].
#'
#' Mutations get a table of individuals as input and return a table of modified individuals as output. Individuals are acted on as
#' individuals: every line of output corresponds to the same line of input, and presence or absence of other input lines does not
#' affect the result.
#'
#' Mutation operations are performed in ES algorithms to facilitate exploration of the search space around individuals.
#'
#' @section Inheriting:
#' `Mutator` is an abstract base class and should be inherited from. Inheriting classes should implement the private `$.mutate()`
#' function. The user of the object calls `$operate()`, and the arguments are passed on to private `$.mutate()` after checking that
#' the operator is primed, and that the `values` argument conforms to the primed domain. Typically, the `$initialize()` function
#' should also be overloaded, and optionally the `$prime()` function; they should call their `super` equivalents.
#'
#' In many cases, it is advisable to inherit from one of the abstract subclasses, such as [`MutatorNumeric`], or [`MutatorDiscrete`].
#'
#' @family base classes
#' @family mutators
#' @export
Mutator = R6Class("Mutator",
  inherit = MiesOperator,
  public = list(
    #' @description
    #' Initialize base class components of the `Mutator`.
    #' @template param_param_classes
    #' @template param_param_set
    initialize = function(param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set = ps()) {
      # remove `endomorphism` option
      super$initialize(param_classes = param_classes, param_set = param_set)
    }
  ),
  private = list(
    .operate = function(values) private$.mutate(values),
    .mutate = function(values) stop(".mutate needs to be implemented by inheriting class.")
  )
)

#' @title Null-Mutator
#'
#' @name dict_mutators_null
#'
#' @description
#' Null-mutator that does not perform any operation on its input. Useful in particular with operator-wrappers such as [`MutatorMaybe`] or [`MutatorCombination`].
#'
#' @section Hyperparameters:
#' This operator has no hyperparameters.
#'
#' @templateVar id null
#' @template autoinfo_prepare_mut
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family mutators
#' @export
MutatorNull = R6Class("MutatorNull",
  inherit = Mutator,
  public = list(
    #' @description
    #' Initialize the `MutatorNull` object.
    initialize = function() {
      # call initialization with standard options: allow everything etc.
      super$initialize()
    }
  ),
  private = list(
    .mutate = function(values) values
  )
)
dict_mutators$add("null", MutatorNull)

#' @title Proxy-Mutator that Mutates According to its Hyperparameter
#'
#' @description
#' Mutator that performs the operation in its `operation` hyperparameter. This is useful, e.g., to make
#' [`OptimizerMies`]'s mutation operation fully parametrizable.
#'
#' Changes in the `operation` hyperparameter are only realized whenever `$prime()` is called, so `$prime()`
#' must be called every time when `operation` is changed, *even if* the new hyperparameter value is already primed.
#'
#' @section Hyperparameters:
#' * `operation` :: [`Mutator`]\cr
#'   Operation to perform. Initialized to [`MutatorNull`].
#'
#' @templateVar id proxy
#' @template autoinfo_prepare_mut
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family mutators
#' @family mutator wrappers
#' @export
MutatorProxy = R6Class("MutatorProxy",
  inherit = Mutator,
  public = list(
    #' @description
    #' Initialize the `MutatorProxy` object.
    initialize = function() {
      param_set = ps(operation = p_uty(custom_check = function(x) check_r6(x, "Mutator"), tags = "required"))
      param_set$values = list(operation = MutatorNull$new())
      # call initialization with standard options: allow everything etc.
      super$initialize(param_set = param_set)
    },
    #' @description
    #' See [`MiesOperator`] method. Primes both this operator, as well as the operator given to the `operation` hyperparameter.
    #' This must be called whenever the `operation` hyperparameter changes, *even if* the hyperparameter is already primed.
    #' @param param_set ([`ParamSet`][paradox::ParamSet])\cr
    #'   Passed to [`MiesOperator`]`$prime()`.
    #' @return [invisible] `self`.
    prime = function(param_set) {
      primed_with = self$param_set$get_values()$operation
      operation = primed_with$clone(deep = TRUE)
      operation$prime(param_set)
      super$prime(param_set)
      private$.operation = operation  # only change operation once everything else succeeded
      private$.primed_with = primed_with  # keep uncloned copy of hyperparameter value for check in `.mutate()`
      invisible(self)
    }
  ),
  private = list(
    .mutate = function(values) {
      if (!is.null(private$.primed_with) && !identical(self$param_set$get_values()$operation, private$.primed_with)) {
        # Unfortunately, when we clone, we can't keep track of self$param_set$values$operation.
        # In that case we ignore and hope the user is doing the right thing. After all, this message
        # is only to be informative.
        stop("MutatorProxy$prime() must be called again when the 'operation' hyperparameter changes.")
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
dict_mutators$add("proxy", MutatorProxy)


#' @title Mutator Choosing Action Probabilistically
#'
#' @name dict_mutators_maybe
#'
#' @description
#' [`Mutator`] that chooses which operation to perform probabilistically. The [`Mutator`] wraps two other [`Mutator`]s given during construction,
#' and for each individuum, the operation to perform is sampled: with probability `p` (hyperparameter), the [`Mutator`] given to the `mutator`
#' construction argument is applied, and with probability `p - 1` the one given to `mutator_not` is applied.
#'
#' @section Hyperparameters:
#' This operator has the hyperparameters of the [`Mutator`]s that it wraps: The hyperparameters of the operator given to the `mutator` construction argument
#' are prefixed with `"maybe."`, the hyperparameters of the operator given to the `mutator_not` construction argument are prefixed with `"maybe_not."`.
#'
#' Additional hyperparameters:
#' * `p` :: `numeric(1)` \cr
#'   Probability per individual with which to apply the operator given to the `mutator` construction argument.
#'
#' @templateVar id maybe
#' @templateVar additional , <mutator> \[, <mutator_not>\]
#' @template autoinfo_prepare_mut
#' @section Supported Operand Types:
#'
#' Supported [`Param`][paradox::Param] classes are the set intersection of supported classes of `mutator` and `mutator_not`.
#'
#' @template autoinfo_dict
#'
#' @family mutators
#' @family mutator wrappers
#' @export
MutatorMaybe = R6Class("MutatorMaybe",
  inherit = Mutator,
  public = list(
    #' @description
    #' Initialize the `MutatorMaybe` object.
    #' @param mutator ([`Mutator`])\cr
    #'   [`Mutator`] to wrap. This operator gets run with probability `p` (Hyperparameter).\cr
    #'   The constructed object gets a *clone* of this argument.
    #' @param mutator_not ([`Mutator`])\cr
    #'   Another [`Mutator`] to wrap. This operator runs when `mutator` is not chosen. By
    #'   default, this is [`MutatorNull`], i.e. no operation. With this default, the
    #'   `MutatorMaybe` object applies the `mutator` operation with probability `p`, and
    #'   no operation at all otherwise.\cr
    #'   The constructed object gets a *clone* of this argument.
    initialize = function(mutator, mutator_not = MutatorNull$new()) {
      private$.wrapped = assert_r6(mutator, "Mutator")$clone(deep = TRUE)
      private$.wrapped_not = assert_r6(mutator_not, "Mutator")$clone(deep = TRUE)

      private$.wrapped$param_set$set_id = "maybe"
      private$.wrapped_not$param_set$set_id = "maybe_not"
      private$.maybe_param_set = ps(p = p_dbl(0, 1, tags = "required"))
      private$.maybe_param_set$values = list(p = 1)
      super$initialize(intersect(mutator$param_classes, mutator_not$param_classes),
        alist(private$.maybe_param_set, private$.wrapped$param_set, private$.wrapped_not$param_set))
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
  private = list(
    .mutate = function(values) {
      mutating = runif(nrow(values)) < self$param_set$get_values()$p
      if (any(mutating)) {
        mutated = private$.wrapped$operate(values[mutating])
      } else {
        mutated = values[mutating]
      }
      if (any(!mutating)) {
        mutated_not = private$.wrapped_not$operate(values[!mutating])
      } else {
        mutated_not = values[!mutating]
      }
      rownumbers = seq_len(nrow(values))
      rowoder = order(c(rownumbers[mutating], rownumbers[!mutating]))
      rbind(mutated, mutated_not)[rowoder]
    },
    .wrapped = NULL,
    .wrapped_not = NULL,
    .maybe_param_set = NULL
  )
)
dict_mutators$add("maybe", MutatorMaybe)

#' @title Mutator Choosing Action Component-Wise Independently
#'
#' @name dict_mutators_cmpmaybe
#'
#' @description
#' [`Mutator`] that chooses which operation to perform probabilistically. The [`Mutator`] wraps two other [`Mutator`]s given during construction,
#' and both of these operators are run. The ultimate result is sampled from the results of these operations independently for each
#' individuum and component: with probability `p` (hyperparameter), the result from the [`Mutator`] given to the `mutator`
#' construction argument is used, and with probability `p - 1` the one given to `mutator_not` is used.
#'
#' @section Hyperparameters:
#' This operator has the hyperparameters of the [`Mutator`]s that it wraps: The hyperparameters of the operator given to the `mutator` construction argument
#' are prefixed with `"cmpmaybe."`, the hyperparameters of the operator given to the `mutator_not` construction argument are prefixed with `"cmpmaybe_not."`.
#'
#' Additional hyperparameters:
#' * `p` :: `numeric(1)` \cr
#'   Probability per component with which to apply the operator given to the `mutator` construction argument.
#'
#' @templateVar id cmpmaybe
#' @templateVar additional , <mutator> \[, <mutator_not>\]
#' @template autoinfo_prepare_mut
#' @section Supported Operand Types:
#'
#' Supported [`Param`][paradox::Param] classes are the set intersection of supported classes of `mutator` and `mutator_not`.
#'
#' @template autoinfo_dict
#'
#' @family mutators
#' @family mutator wrappers
#' @export
MutatorCmpMaybe = R6Class("MutatorCmpMaybe",
  inherit = Mutator,
  public = list(
    #' @description
    #' Initialize the `MutatorCmpMaybe` object.
    #' @param mutator ([`Mutator`])\cr
    #'   [`Mutator`] to wrap. This operator gets run with probability `p` (Hyperparameter).\cr
    #'   The constructed object gets a *clone* of this argument.
    #' @param mutator_not ([`Mutator`])\cr
    #'   Another [`Mutator`] to wrap. This operator runs when `mutator` is not chosen. By
    #'   default, this is [`MutatorNull`], i.e. no operation. With this default, the
    #'   `MutatorCmpMaybe` object applies the `mutator` operation with probability `p`, and
    #'   no operation at all otherwise.\cr
    #'   The constructed object gets a *clone* of this argument.
    initialize = function(mutator, mutator_not = MutatorNull$new()) {
      private$.wrapped = assert_r6(mutator, "Mutator")$clone(deep = TRUE)
      private$.wrapped_not = assert_r6(mutator_not, "Mutator")$clone(deep = TRUE)

      private$.wrapped$param_set$set_id = "cmpmaybe"
      private$.wrapped_not$param_set$set_id = "cmpmaybe_not"
      private$.maybe_param_set = ps(p = p_dbl(0, 1, tags = "required"))
      private$.maybe_param_set$values = list(p = 1)
      super$initialize(intersect(mutator$param_classes, mutator_not$param_classes),
        alist(private$.maybe_param_set, private$.wrapped$param_set, private$.wrapped_not$param_set))
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
  private = list(
    .mutate = function(values) {
      mutated = private$.wrapped$operate(values)
      mutated_not = private$.wrapped_not$operate(values)

      mutating = matrix(runif(nrow(values) * ncol(values)) < self$param_set$get_values()$p, nrow = nrow(values))
      as.data.table(mapply(function(mutnot, mut, takemut) {
        mutnot[takemut] <- mut[takemut]
        mutnot
      }, mutated_not, mutated, mutating, SIMPLIFY = FALSE))
    },
    .wrapped = NULL,
    .wrapped_not = NULL,
    .maybe_param_set = NULL
  )
)
dict_mutators$add("cmpmaybe", MutatorCmpMaybe)


#' @title Numeric Mutator Base Class
#'
#' @description
#' Base class for mutation operations on numeric individuals, inheriting from [`Mutator`].
#'
#' `MutatorNumeric` operators perform mutation on numeric (integer and real valued) individuals. Inheriting
#' operators implement the private `$.mutate_numeric()` function that is called once for each individual
#' and is given a numeric vector.
#'
#' @section Inheriting:
#' `MutatorNumeric` is an abstract base class and should be inherited from. Inheriting classes should implement the private `$.mutate_numeric()`
#' function. During `$operate()`, the `$.mutate_numeric()` function is called once for each individual, with the parameters `values` (the
#' individual as a single `numeric` vector), `lowers` and `uppers` (`numeric` vectors, the lower and upper bounds for each component of `values`). Typically,
#' `$initialize()` should also be overloaded.
#'
#' @family base classes
#' @family mutators
#' @export
MutatorNumeric = R6Class("MutatorNumeric",
  inherit = Mutator,
  public = list(
    #' @description
    #' Initialize base class components of the `MutatorNumeric`.
    #' @templateVar allowedparams ParamInt ParamDbl
    #' @template param_param_classes
    #' @template param_param_set
    initialize = function(param_classes = c("ParamInt", "ParamDbl"), param_set = ps()) {
      assert_subset(param_classes, c("ParamInt", "ParamDbl"))
      super$initialize(param_classes, param_set)
    }
  ),
  private = list(
    .mutate = function(values) {
      mutated <- apply(values, 1, private$.mutate_numeric, private$.primed_ps$lower, private$.primed_ps$upper)
      if (is.matrix(mutated)) {
        mutated <- t(mutated)
      } else {
        mutated <- as.matrix(mutated)
      }
      colnames(mutated) <- colnames(values)
      as.data.table(mutated)
    },
    .mutate_numeric = function(values, lowers, uppers) stop(".mutate_numeric needs to be implemented by inheriting class.")
  )
)

#' @title Discrete Mutator Base Class
#'
#' @description
#' Base class for mutation operations on discrete individuals, inheriting from [`Mutator`].
#'
#' `MutatorDiscrete` operators perform mutation on discrete (logical and factor valued) individuals. Inheriting
#' operators implement the private `$.mutate_discrete()` function that is called once for each individual
#' and is given a character vector.
#'
#' @section Inheriting:
#' `MutatorDiscrete` is an abstract base class and should be inherited from. Inheriting classes should implement the private `$.mutate_discrete()`
#' function. During `$operate()`, the `$.mutate_discrete()` function is called once for each individual, with the parameters `values` (the
#' individual as a single `character` vector), and `levels` (a list of `character` containing the possible values for each element of `values`). Typically,
#' `$initialize()` should also be overloaded.
#'
#' @family base classes
#' @family mutators
#' @export
MutatorDiscrete = R6Class("MutatorDiscrete",
  inherit = Mutator,
  public = list(
    #' @description
    #' Initialize base class components of the `MutatorNumeric`.
    #' @templateVar allowedparams ParamLgl ParamFct
    #' @template param_param_classes
    #' @template param_param_set
    initialize = function(param_classes = c("ParamLgl", "ParamFct"), param_set = ps()) {
      assert_subset(param_classes, c("ParamLgl", "ParamFct"))
      super$initialize(param_classes, param_set)
    }
  ),
  private = list(
    .mutate = function(values) {
      vals = as.matrix(values)
      mode(vals) <- "character"
      vals = apply(vals, 1, private$.mutate_discrete, map(private$.primed_ps$levels, as.character))
      if (is.matrix(vals)) {
        vals = t(vals)
      } else {
        vals = as.matrix(vals)
      }
      vals = as.data.table(vals)

      vals = vals[, pmap(list(.SD, private$.primed_ps$class), function(val, class) if (class == "ParamLgl") as.logical(val) else val)]  # TODO maybe this can be done more elegantly
      setnames(vals, private$.primed_ps$ids())
    },
    .mutate_discrete = function(values, levels) stop(".mutate_discrete needs to be implemented by inheriting class.")
  )
)

#' @title Gaussian Distribution Mutator
#'
#' @name dict_mutators_gauss
#'
#' @description
#' Individuals are mutated with an independent normal random variable on each component.
#'
#' @section Hyperparameters:
#' * `sdev` :: `numeric(1)`\cr
#'   Standard deviation of normal distribuion. This is absolute if `sdev_is_relative` is `FALSE`, and
#'   multiplied with each individual component's range (upper - lower) if `sdev_is_relative` is `TRUE`.
#'   Initialized to 1.
#' * `sdev_is_relative` :: `logical(1)`\cr
#'   Whether `sdev` is absolute (`FALSE`) or relative to component range (`TRUE`). Initialized to `TRUE`.
#' * `truncated_normal` :: `logical(1)`\cr
#'   Whether to draw individuals from a normal distribution that is truncated at the bounds of each
#'   component (`TRUE`), or to draw from a normal distribution and then restrict to bounds afterwards
#'   (`FALSE`). The former (`TRUE`) will lead to very few to no samples landing on the exact bounds
#'   (analytically it would be none almost surely, but this is subject to machine precision), the latter
#'   (`FALSE`) can lead to a substantial number of samples landing on the exact bounds. Initialized to `TRUE`.
#'
#' @templateVar id gauss
#' @template autoinfo_prepare_mut
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family mutators
#' @export
MutatorGauss = R6Class("MutatorGauss",
  inherit = MutatorNumeric,
  public = list(
    #' @description
    #' Initialize the `MutatorGauss` object.
    initialize = function() {
      param_set = ps(sdev = p_dbl(0, tags = "required"), sdev_is_relative = p_lgl(tags = "required"), truncated_normal = p_lgl(tags = "required"))
      param_set$values = list(sdev = 1, sdev_is_relative = TRUE, truncated_normal = TRUE)
      super$initialize("ParamDbl", param_set)
    }
  ),
  private = list(
    .mutate_numeric = function(values, lowers, uppers) {
      params = self$param_set$get_values()
      sdev = params$sdev
      if (params$sdev_is_relative) {
        assert_numeric(lowers, finite = TRUE, any.missing = FALSE)
        assert_numeric(uppers, finite = TRUE, any.missing = FALSE)
        sdev = sdev * (uppers - lowers)
      }
      if (params$truncated_normal) {
        mutated <- ifelse(sdev == 0, values,
          qnorm(runif(length(values),
            pnorm(lowers, values, sdev),
            pnorm(uppers, values, sdev)),
            values, sdev)
        )
      } else {
        mutated <- rnorm(length(values), values, sdev)
      }
      pmax(pmin(mutated, uppers), lowers)
    }
  )
)
dict_mutators$add("gauss", MutatorGauss)

#' @title Uniform Discrete Mutator
#'
#' @name dict_mutators_unif
#'
#' @description
#' Discrete components are mutated by sampling from a uniform distribution, either from all possible
#' values of each component, or from all values except the original value.
#'
#' Since the information loss is very high, this should in most cases be combined with [`MutatorCmpMaybe`].
#'
#' @section Hyperparameters:
#' * `can_mutate_to_same` :: `logical(1)`\cr
#'   Whether to sample from entire range of each parameter (`TRUE`) or from all values except the
#'   current value (`FALSE`). Initialized to `TRUE`.
#'
#' @templateVar id unif
#' @template autoinfo_prepare_mut
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family mutators
#' @export
MutatorDiscreteUniform = R6Class("MutatorDiscreteUniform",
  inherit = MutatorDiscrete,
  public = list(
    #' @description
    #' Initialize the `MutatorDiscreteUniform` object.
    initialize = function() {
      param_set = ps(can_mutate_to_same = p_lgl(tags = "required"))
      param_set$values = list(can_mutate_to_same = TRUE)
      super$initialize(c("ParamLgl", "ParamFct"), param_set)
    }
  ),
  private = list(
    .mutate_discrete = function(values, levels) {
      params = self$param_set$get_values()
      unlist(pmap(list(values, levels), function(v, l) {
        if (!params$can_mutate_to_same) l = setdiff(l, v)
        sample(l, 1)
      }))
    }
  )
)
dict_mutators$add("unif", MutatorDiscreteUniform)
