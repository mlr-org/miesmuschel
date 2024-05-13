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
    #' @template param_packages
    #' @template param_dict_entry
    #' @template param_own_param_set
    initialize = function(param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set = ps(), packages = character(0), dict_entry = NULL, own_param_set = quote(self$param_set)) {
      # remove `endomorphism` option
      super$initialize(param_classes = param_classes, param_set = param_set, packages = packages, dict_entry = dict_entry, dict_shortaccess = "mut", own_param_set = own_param_set)
    }
  ),
  private = list(
    .operate = function(values) {
      inrows = nrow(values)
      values = private$.mutate(values)
      outrows = nrow(values)
      if (inrows != outrows) stopf("Mutation of %s input rows resulted in %s output rows.", inrows, outrows)
      values
    },
    .mutate = function(values) stop(".mutate needs to be implemented by inheriting class.")
  )
)

#' @title Numeric Mutator Base Class
#'
#' @description
#' Base class for mutation operations on numeric and integer valued individuals, inheriting from [`Mutator`].
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
#' [`MutatorNumeric`]s that perform real-valued operations, such as e.g. [`MutatorGauss`], operate on integers by widening the lower and upper bounds
#' of integer components by 0.5, applying their operation, and rounding resulting values to the nearest integer (while always staying inside bounds).
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
    #' @template param_packages
    #' @template param_dict_entry
    #' @template param_own_param_set
    initialize = function(param_classes = c("ParamInt", "ParamDbl"), param_set = ps(), packages = character(0), dict_entry = NULL, own_param_set = quote(self$param_set)) {
      assert_subset_character(param_classes, c("ParamInt", "ParamDbl"))
      if (identical(param_classes, "ParamDbl")) {
        param_classes = c("ParamInt", "ParamDbl")
        private$.integer_native = FALSE
      } else {
        private$.integer_native = TRUE
      }
      super$initialize(param_classes, param_set, packages = packages, dict_entry = dict_entry, own_param_set = own_param_set)
    }
  ),
  private = list(
    .mutate = function(values) {

      # Is non-native integer
      is_nn_int = !private$.integer_native & private$.primed_ps$class == "ParamInt"
      lowers = private$.primed_ps$lower
      uppers = private$.primed_ps$upper
      mutated = apply(values, 1, private$.mutate_numeric,
        lowers - 0.5 * is_nn_int,
        uppers + 0.5 * is_nn_int
      )

      if (is.matrix(mutated)) {
        mutated <- t(mutated)
      } else {
        mutated <- as.matrix(mutated)
      }

      if (any(is_nn_int)) {
        ### This is slow:
        # rounded = round(mutated[, is_nn_int])
        # rounded = sweep(rounded, 2, uppers[is_nn_int], pmin)
        # rounded = sweep(rounded, 2, lowers[is_nn_int], pmax)
        ### This is faster and does the same:
        mutated[, is_nn_int] = t(pmax(pmin(t(round(mutated[, is_nn_int])), uppers[is_nn_int]), lowers[is_nn_int]))
      }
      colnames(mutated) <- colnames(values)
      as.data.table(mutated)
    },
    .mutate_numeric = function(values, lowers, uppers) stop(".mutate_numeric needs to be implemented by inheriting class."),
    .integer_native = NULL  # whether the operation handles integers natively, or we have to round results.
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
    #' @template param_packages
    #' @template param_dict_entry
    #' @template param_own_param_set
    initialize = function(param_classes = c("ParamLgl", "ParamFct"), param_set = ps(), packages = character(0), dict_entry = NULL, own_param_set = quote(self$param_set)) {
      assert_subset_character(param_classes, c("ParamLgl", "ParamFct"))
      super$initialize(param_classes, param_set, packages = packages, dict_entry = dict_entry, own_param_set = own_param_set)
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
