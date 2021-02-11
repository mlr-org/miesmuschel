#' @title Selector Base Class
#'
#' @include MiesOperator.R
#' @include dictionaries.R
#'
#' @description
#' Base class representing selection operations, inheriting from [`MiesOperator`].
#'
#' A [`Selector`] gets a table of individuals as input, along with information on the individuals' performance values and the
#' number of individuals to select, and returns a vector of integers indicating which individuals were selected.
#'
#' Selection operations are performed in ES algorithms to facilitate concentration towards individuals that perform well with regard to the
#' fitness measure.
#'
#' Fitness values are always *maximized*, both in single- and multi-criterion optimization.
#'
#' Unlike most other operator types inheriting from [`MiesOperator`], the `$operate()` function has three arguments, which are passed on to `$.select()`
#' * `values` :: `data.frame`\cr
#'   Individuals to operate on. Must pass the check of the [`Param`][paradox::ParamSet] given in the last `$prime()` call
#'   and may not have any missing components.
#' * `fitnesses` :: `numeric` | `matrix`\cr
#'   Fitnesses for each individual given in `values`. If this is a `numeric`, then its length must be equal to the number of rows in `values`. If
#'   this is a `matrix`, if number of rows must be equal to the number of rows in `values`, and it must have one column when doing single-crit optimization
#'   and one column each for each  "criterion" when doing multi-crit optimization.
#' * `n_select` :: `integer(1)`\cr
#'   Number of individuals to select. Some `Selector`s select individuals with replacement, for which this value may be greater than the number of
#'   rows in `values`.
#'
#' The return value for an operation will be a numeric vector of integer values of ength `n_select` indexing the individuals that were selected. Some `Selector`s
#' select individuals with replacement, for which the return value may contain indices more than once.
#'
#' @section Inheriting:
#' `Selector` is an abstract base class and should be inherited from. Inheriting classes should implement the private `$.select()`
#' function. The user of the object calls `$operate()`, and the arguments are passed on to private `$.select()` after checking that
#' the operator is primed, that the `values` argument conforms to the primed domain and that other values match. Typically, the `$initialize()` function
#' should also be overloaded, and optionally the `$prime()` function; they should call their `super` equivalents.
#'
#' @family base classes
#' @family selectors
#' @export
Selector = R6Class("Selector",
  inherit = MiesOperator,
  public = list(
    #' @description
    #' Initialize base class components of the `Mutator`.
    #' @template param_param_classes
    #' @template param_param_set
    #' @param supported (`character`)\cr
    #'   Subset of `"single-crit"` and `"multi-crit"`, indicating wether single and / or multi-criterion optimization is supported.
    #'   Default both of them.\cr
    #'   The `$supported` field will reflect this value.
    initialize = function(param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set = ps(), supported = c("single-crit", "multi-crit")) {
      assert_subset(supported, c("single-crit", "multi-crit"))
      assert_character(supported, any.missing = FALSE, unique = TRUE, min.len = 1)
      private$.supported = supported
      super$initialize(param_classes, param_set, endomorphism = FALSE)
    }
  ),
  active = list(
    #' @field supported (`character`)\cr
    #' Optimization supported by this `Selector`, can be `"single-crit"`, `"multi-crit"`, or both.
    supported = function(val) {
      if (!missing(val)) stop("supported is read-only.")
      private$.supported
    }
  ),
  private = list(
    .supported = NULL,
    .operate = function(values, fitnesses, n_select) {
      assert_data_table(values, min.rows = 1)
      if ("single-crit" %in% self$supported && test_numeric(fitnesses, any.missing = FALSE, len = nrow(values))) {
        fitnesses = matrix(fitnesses, ncol = 1)
      }
      assert_matrix(fitnesses, nrows = nrow(values),
        min.cols = 1, max.cols = if ("multi-crit" %nin% self$supported) 1,
        mode = "numeric", any.missing = FALSE
      )

      assert_int(n_select, lower = 0, tol = 1e-100)
      selected = private$.select(values, fitnesses, n_select)
      assert_integerish(selected, tol = 1e-100, lower = 1, upper = nrow(values), any.missing = FALSE, len = n_select)
    },
    .select = function(values, fitnesses, n_select) stop(".select needs to be implemented by inheriting class.")
  )
)

#' @title Random Selector
#'
#' @name dict_selectors_random
#'
#' @description
#' Random selector that disregards fitness and individual values and selects individuals randomly. Depending on the hyperparameter `replace`,
#' it samples with or without replacement.
#'
#' @section Hyperparameters:
#' * `replace` :: `logical(1)`\cr
#'   Whether to sample individuals with (`TRUE`) or without (`FALSE`) replacement. When sampling is done without replacement, then
#'   `n_select` must be less or equal the number of rows in `values` when calling `$operate()`. Initialized to `FALSE`.
#'
#' @templateVar id random
#' @template autoinfo_prepare_sel
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family selectors
#' @export
SelectorRandom = R6Class("SelectorRandom",
  inherit = Selector,
  public = list(
    #' @description
    #' Initialize the `SelectorRandom` object.
    initialize = function() {
      param_set = ps(replace = p_lgl(tags = "required"))
      param_set$values = list(replace = FALSE)
      super$initialize(c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set)
    }
  ),
  private = list(
    .select = function(values, fitnesses, n_select) {
      params = self$param_set$get_values()
      sample(nrow(values), n_select, replace = params$replace)
    }
  )
)
dict_selectors$add("random", SelectorRandom)


#' @title Proxy-Selectior that Selects According to its Hyperparameter
#'
#' @description
#' Selector that performs the operation in its `operation` hyperparameter. This is useful, e.g., to make
#' [`OptimizerMies`]'s selection operations fully parametrizable.
#'
#' Changes in the `operation` hyperparameter are only realized whenever `$prime()` is called, so `$prime()`
#' must be called every time when `operation` is changed.
#'
#' @section Hyperparameters
#' * `operation` :: [`Selector`]\cr
#'   Operation to perform. Initialized to [`SelectorBest`].
#'
#' @templateVar id proxy
#' @template autoinfo_prepare_sel
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family selectors
#' @family selector wrappers
#' @export
SelectorProxy = R6Class("SelectorProxy",
  inherit = Selector,
  public = list(
    #' @description
    #' Initialize the `SelectorProxy` object.
    initialize = function() {
      param_set = ps(operation = p_uty(custom_check = function(x) check_r6(x, "Selector")))
      param_set$values = list(operation = SelectorBest$new())
      # call initialization with standard options: allow everything etc.
      super$initialize(param_set = param_set)
    },
    prime = function(param_set) {
      primed_with = self$param_set$get_vlaues()$operation
      operatio = primed_with$clone(deep = TRUE)
      operation$prime(param_set)
      super$prime(param_set)
      private$.operation = operation  # only change operation once everything else succeeded
      private$.primed_with = primed_with  # keep uncloned copy of hyperparameter value for check in `.select()`
      invisible(self)
    }
  ),
  private = list(
    .select = function(values, fitnesses, n_select) {
      if (!is.null(private$.primed_with) && !identical(self$param_set$get_values()$operation, private$.primed_with)) {
        # Unfortunately, when we clone, we can't keep track of self$param_set$values$operation.
        # In that case we ignore and hope the user is doing the right thing. After all, this message
        # is only to be informative.
        stop("SelectorProxy$prime() must be called again when the 'operation' hyperparameter changes.")
      }
      private$.operation$operate(values, fitnesses, n_select)
    },
    .operation = NULL,
    .primed_with = NULL,
    deep_clone = function(name, value) {
      if (name == ".primed_with") return(NULL)
      super$deep_clone(name, value)
    }
  )
)
dict_selectors$add("proxy", SelectorProxy)


#' @title Best Value Selector
#'
#' @name dict_selectors_best
#'
#' @description
#' Selector that selects the top `n_select` individuals based on the  fitness value. When `n_select` is larger than the number
#' of individuals, the selection wraps around: All `nrow(values)` individuals are selected at least `floor(nrow(values) / n_select)`
#' times, with the top `nrow(values) %% n_select` individuals being selected one more time.
#'
#' @section Hyperparameters:
#' This operator has no hyperparameters.
#'
#' @templateVar id best
#' @template autoinfo_prepare_sel
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family selectors
#' @export
SelectorBest = R6Class("SelectorBest",
  inherit = Selector,
  public = list(
    #' @description
    #' Initialize the `SelectorBest` object.
    initialize = function() {
      super$initialize(supported = "single-crit")
    }
  ),
  private = list(
    .select = function(values, fitnesses, n_select) {
      order(fitnesses, decreasing = TRUE)[(seq_len(n_select) - 1) %% nrow(values) + 1]
    }
  )
)
dict_selectors$add("best", SelectorBest)
