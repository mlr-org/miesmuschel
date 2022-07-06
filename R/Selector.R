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
#'   and one column each for each  "criterion" when doing multi-crit optimization.\cr
#'   The `fitnesses`-value passed on to `$.select()` is always a `matrix`.
#' * `n_select` :: `integer(1)`\cr
#'   Number of individuals to select. Some `Selector`s select individuals with replacement, for which this value may be greater than the number of
#'   rows in `values`.
#' * `group_size` :: `integer`\cr
#'   Sampling group size hint, indicating that the caller would prefer there to not be any duplicates within this group size, e.g. because the
#'   [`Selector`] is called to select individuals to be given to a [`Recombinator`] with a certain `n_indivs_in`, or because it is called as a
#'   `survival_selector` in [`mies_survival_comma()`] or [`mies_survival_plus()`]. The [`Selector`] may or may not ignore this value, however.
#'   This may possibly happen because of certain configuration parameters, or because the input size is too small.\cr
#'   Must either be a scalar value or sum up to `n_select`. Must be non-negative. A scalar value of 0 is interpreted the same as 1.\cr
#'   If not given, this value defaults to 1.
#'
#' The return value for an operation will be a numeric vector of integer values of length `n_select` indexing the individuals that were selected. Some `Selector`s
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
    #' Initialize base class components of the `Selector`.
    #' @template param_is_deterministic
    #' @template param_param_classes
    #' @template param_param_set
    #' @param supported (`character`)\cr
    #'   Subset of `"single-crit"` and `"multi-crit"`, indicating wether single and / or multi-criterion optimization is supported.
    #'   Default both of them.\cr
    #'   The `$supported` field will reflect this value.
    #' @template param_packages
    #' @template param_dict_entry
    #' @template param_own_param_set
    initialize = function(is_deterministic = FALSE, param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set = ps(), supported = c("single-crit", "multi-crit"), packages = character(0), dict_entry = NULL, own_param_set = quote(self$param_set)) {
      assert_subset(supported, c("single-crit", "multi-crit"))
      assert_character(supported, any.missing = FALSE, unique = TRUE, min.len = 1)
      private$.supported = supported
      if (is_deterministic) {
        assert_r6(param_set, "ParamSet")
        shuffle_ps = ps(shuffle_selection = p_lgl(tags = "required"))
        shuffle_ps$values$shuffle_selection = TRUE
        param_set = ps_union(list(param_set, shuffle_ps))
      }
      super$initialize(param_classes, param_set, endomorphism = FALSE, packages = packages, dict_entry = dict_entry, dict_shortaccess = "sel", own_param_set = own_param_set)
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
    .operate = function(values, fitnesses, n_select, group_size = 1) {
      assert_data_table(values, min.rows = 1)
      if ("single-crit" %in% self$supported && test_numeric(fitnesses) && !test_matrix(fitnesses)) {
        assert_numeric(fitnesses, any.missing = FALSE, len = nrow(values))
        fitnesses = matrix(fitnesses, ncol = 1)
      }
      assert_matrix(fitnesses, nrows = nrow(values),
        min.cols = 1, max.cols = if ("multi-crit" %nin% self$supported) 1,
        mode = "numeric", any.missing = FALSE
      )

      assert_int(n_select, lower = 0, tol = 1e-100)
      assert(check_int(group_size, lower = 0, tol = 1e-100), check_integerish(group_size, lower = 1, tol = 1e-100, any.missing = FALSE, min.len = 1))
      if (length(group_size) > 1 && sum(group_size) != n_select) {
        stop(sprintf("non-scalar group_size must sum up to n_select (%s) but is c(%s).", n_select, paste(group_size, collapse = ",")))
      }
      if (n_select == 0) return(numeric(0))
      if (length(group_size) == 1 && group_size == 0) group_size = 1
      params = self$param_set$get_values()
      selected = private$.select(values, fitnesses, n_select, group_size)
      assert_integerish(selected, tol = 1e-100, lower = 1, upper = nrow(values), any.missing = FALSE, len = n_select)
      if (isTRUE(params$shuffle_selection)) {
        selected = selected[sample.int(length(selected))]
      }
      selected
    },
    .select = function(values, fitnesses, n_select, group_size) stop(".select needs to be implemented by inheriting class.")
  )
)

#' @title Selector making use of Scalors
#'
#' @description
#' Base class inheriting from [`Selector`] for selection operations that make use of scalar values, generated by [`Scalor`].
#'
#' @section Inheriting:
#' `SelectorScaling` is an abstract base class and should be inherited from. Inheriting classes should implement the private `$.select_scalar()`
#' function. During `$operate()`, the `$.select_scalar()` function is called, it should have three arguments, similar to [`Selector`]'s `$.select()` function.
#' `values` and `n_select` are as given to `$.select()` of the [`Selector`]. The `fitnesses` argument is first scaled by the associated [`Scalor`]
#' and then passed on as a `numeric` vector.
#'
#' Typically, `$initialize()` should also be overloaded when inheriting.
#'
#' @family base classes
#' @family selectors
#' @export
SelectorScalar = R6Class("SelectorScalar",
  inherit = Selector,
  public = list(
    #' @description
    #' Initialize base class components of the `SelectorScalar`.
    #' @template param_scalor
    #' @template param_is_deterministic
    #' @template param_param_classes
    #' @template param_param_set
    #' @param supported (`character`)\cr
    #'   Subset of `"single-crit"` and `"multi-crit"`, indicating wether single and / or multi-criterion optimization is supported.
    #'   Default to the supported set of `scalor`.\cr
    #'   The `$supported` field will reflect this value.
    #' @template param_packages
    #' @template param_dict_entry
    initialize = function(scalor = ScalorSingleObjective$new(), is_deterministic = FALSE, param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set = ps(), supported = scalor$supported, packages = character(0), dict_entry = NULL) {
      private$.scalor = assert_r6(scalor, "Scalor")$clone(deep = TRUE)
      assert_r6(param_set, "ParamSet")
      if (is_deterministic) {
        shuffle_ps = ps(shuffle_selection = p_lgl(tags = "required"))
        shuffle_ps$values$shuffle_selection = TRUE
        private$.own_param_set = ps_union(list(param_set, shuffle_ps))
      } else {
        private$.own_param_set = param_set
      }
      private$.scalor$param_set$set_id = "scale"
      super$initialize(is_deterministic = FALSE, param_classes = param_classes, param_set = alist(private$.own_param_set, private$.scalor$param_set), supported = supported,
        packages = c(packages, scalor$packages), dict_entry = dict_entry,
        own_param_set = quote(private$.own_param_set))
    },
    #' @description
    #' See [`MiesOperator`] method. Primes both this operator, as well as the wrapped operator
    #' given to `scalor` during construction.
    #' @param param_set ([`ParamSet`][paradox::ParamSet])\cr
    #'   Passed to [`MiesOperator`]`$prime()`.
    #' @return [invisible] `self`.
    prime = function(param_set) {
      private$.scalor$prime(param_set)
      super$prime(param_set)
      invisible(self)
    }
  ),
  active = list(
    #' @field scalor ([`Scalor`])\cr
    #' [`Scalor`] used to scalarize fitnesses for selection.
    scalor = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.scalor)) {
        stop("scalor is read-only.")
      }
      private$.scalor
    }
  ),
  private = list(
    .select = function(values, fitnesses, n_select, group_size) {
      private$.select_scalar(values, private$.scalor$operate(values, fitnesses), n_select, group_size)
    },
    .scalor = NULL,
    .own_param_set = NULL,
    .select_scalar = function(values, fitnesses_scalar, n_select) stop(".select_scalar needs to be implemented by inheriting class.")
  )
)
