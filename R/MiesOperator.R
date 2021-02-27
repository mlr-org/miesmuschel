#' @title Operator Base Class
#'
#' @description
#' Base class representing MIES-operators: [`Recombinator`], [`Mutator`], and [`Selector`].
#'
#' Operators perform a specific function within ES algorithms, and by exchanging them, the character of ES
#' algorithms can be modified. Operators operate on collections of individuals and return
#' modified individuals (mutated or recombined) or indices of selected individuals. Operators can be combined using
#' [`MutatorCombination`] / [`RecombinatorCombination`] and other operators wrappers.
#'
#' Before applying operators, they have to be *primed* for the domain of the individuals which they are operating on;
#' this is done using the `$prime()` function. Afterwards, the `$operate()` function may be called with a `data.frame`
#' of individuals that fall into this domain. `$operate()` may be called multiple times after priming, and a once
#' primed operator can be primed again for a different domain by calling `$prime()` agian (which forgets the old priming).
#'
#' @section Inheriting:
#' `MiesOperator` is an abstract base class and should be inherited from. Inheriting classes should implement the
#' private `$.operate()` function. The user of the object calls `$operate()`, and the arguments are passed on to
#' private `$.operate()` after checking that the operator is primed, and that the `values` argument conforms to the
#' primed domain. Typically, the `$initialize()` and `$prime()` functions are also overloaded, but should call their
#' `super` equivalents.
#'
#' In most cases, the `MiesOperator` class should not be inherited from, directly; instead, the operator classes
#' ([`Recombinator`], [`Mutator`], [`Selector`]) or their subclasses should be inherited.
#'
#' @family base classes
#' @export
MiesOperator = R6Class("MiesOperator",
  public = list(
    #' @description
    #' Initialize base class components of the `MiesOperator`.
    #' @template param_param_classes
    #' @template param_param_set
    #' @param endomorphism (`logical(1)`)\cr
    #'   Whether the private `$.operate()` operation creates a [`data.table`][data.table::data.table] with the same columns as the input
    #'   (i.e. conforming to the primed [`ParamSet`][paradox::ParamSet]). If this is `TRUE` (default), then the return value of `$.operate()`
    #'   is checked for this and columns are put in the correct order.\cr
    #'   The `$endomorphsim` field will reflect this value.
    initialize = function(param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set = ps(), endomorphism = TRUE) {
      assert_subset(param_classes, c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), empty.ok = FALSE)
      if (inherits(param_set, "ParamSet")) {
        private$.param_set = assert_param_set(param_set)
        private$.param_set_source = NULL
      } else {
        lapply(param_set, function(x) assert_param_set(eval(x)))
        private$.param_set_source = param_set
      }
      private$.param_classes = param_classes
      private$.endomorphism = assert_flag(endomorphism)
    },
    #' @description
    #' Prepare the `MiesOperator` to function on the given [`ParamSet`][paradox::ParamSet]. This must be called before
    #' `$operate()`. It may be called multiple times in the lifecycle of the `MiesOperator` object, and prior primings are
    #' forgotten when priming on a new [`ParamSet`][paradox::ParamSet]. The [`ParamSet`][paradox::ParamSet] on which
    #' the `MiesOperator` was last primed can be read from `$primed_ps`.
    #' @param param_set ([`ParamSet`][paradox::ParamSet])\cr
    #'   The [`ParamSet`][paradox::ParamSet] to which all `values` tables passed to `$operate()` will need to conform to.
    #'   May only contiain [`Param`][paradox::ParamSet] objects that conform to the classes listed in `$param_classes`.
    #' @return [invisible] `self`.
    prime = function(param_set) {
      assert_subset(param_set$class, self$param_classes)
      private$.primed_ps = ps_flatten(param_set)
      invisible(self)
    },
    #' @description
    #' Operate on the given individuals. This calls private `$.operate()`, which must be overloaded by an inheriting class,
    #' passing through all function arguments after performing some checks.
    #' @param values (`data.frame`)\cr
    #'   Individuals to operate on. Must pass the check of the [`Param`][paradox::ParamSet] given in the last `$prime()` call
    #'   and may not have any missing components.
    #' @param ... (any)\cr
    #'   Depending on the concrete class, passed on to `$.operate()`.
    #' @return `data.frame`: the result of the operation. If the input was a [`data.table`][data.table::data.table] instead of
    #'   a `data.frame`, the output is also [`data.table`][data.table::data.table].
    operate = function(values, ...) {
      if (is.null(private$.primed_ps)) stop("Operator must be primed first!")
      ids = private$.primed_ps$ids()
      private$.primed_ps$assert_dt(values)
      assert_names(colnames(values), permutation.of = private$.primed_ps$ids())
      convert = !is.data.table(values)
      if (convert) {
        # don't change input by reference
        values = as.data.table(values)
      }
      # make sure input / output cols are in the order as inndicated by paramset --> use `match` on input (and output if endomorphic)
      values = private$.operate(values[, match(ids, colnames(values), 0), with = FALSE], ...)
      if (self$endomorphism) {
        values = private$.primed_ps$assert_dt(values)[, match(ids, colnames(values), 0), with = FALSE]
        if (convert) {
          setDF(values)
        }
      }
      values
    }
  ),
  active = list(
    #' @field param_set ([`ParamSet`][paradox::ParamSet])\cr
    #' Configuration parameters of the `MiesOperator` object. Read-only.
    param_set = function(val) {
      if (is.null(private$.param_set)) {
        sourcelist = lapply(private$.param_set_source, function(x) eval(x))
        if (length(sourcelist) > 1) {
          private$.param_set = ParamSetCollection$new(sourcelist)
        } else {
          private$.param_set = sourcelist[[1]]
        }
        if (!is.null(private$.param_set_id)) private$.param_set$set_id = private$.param_set_id
      }
      if (!missing(val) && !identical(val, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    },
    #' @field param_classes (`character`)\cr
    #' Classes of parameters that the operator can handle, contains any of `"ParamLgl"`, `"ParamInt"`, `"ParamDbl"`, `"ParamFct"`. Read-only.
    param_classes = function(val) {
      if (!missing(val)) stop("param_classes is read-only.")
      private$.param_classes
    },
    #' @field endomorphism (`logical(1)`)\cr
    #' Whether the output of `$operate()` is a `data.frame` / [`data.table`][data.table::data.table] in the same domain as its input. Read-only.
    endomorphism = function(val) {
      if (!missing(val)) stop("endomorphism is read-only.")
      private$.endomorphism
    },
    #' @field primed_ps ([`ParamSet`][paradox::ParamSet] | `NULL`)\cr
    #' [`ParamSet`][paradox::ParamSet] on which the `MiesOperator` is primed. Is `NULL` if it has not been primed.
    #' Writing to this acrive binding calls `$prime()`.
    primed_ps = function(val) {
      if (!missing(val)) {
        self$prime(val)
      }
      private$.primed_ps
    },
    #' @field is_primed (`logical(1)`)\cr
    #' Whether the `MiesOperator` was primed before. Is `FALSE` exactly when `$primed_ps` is `NULL`. Read-only.
    is_primed = function(val) {
      if (!missing(val)) stop("is_primed is read-only.")
      !is.null(self$primed_ps)
    }
  ),
  private = list(
    deep_clone = function(name, value) {
      if (!is.null(private$.param_set_source)) {
        private$.param_set_id = private$.param_set$set_id
        private$.param_set = NULL  # required to keep clone identical to original, otherwise tests get really ugly
        if (name == ".param_set_source") {
          value = lapply(value, function(x) {
            if (inherits(x, "R6")) x$clone(deep = TRUE) else x
          })
        }
      }
      if (is.environment(value) && !is.null(value[[".__enclos_env__"]])) {
        return(value$clone(deep = TRUE))
      }
      value
    },
    .param_set = NULL,
    .param_set_id = NULL,
    .primed_ps = NULL,
    .param_classes = NULL,
    .param_set_source = NULL,
    .operate = function(values, ...) stop(".operate needs to be implemented by inheriting class."),
    .endomorphism = NULL
  )
)
