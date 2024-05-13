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
    #' @template param_packages
    #' @template param_dict_entry
    #' @template param_dict_shortaccess
    #' @template param_own_param_set
    #' @param endomorphism (`logical(1)`)\cr
    #'   Whether the private `$.operate()` operation creates a [`data.table`][data.table::data.table] with the same columns as the input
    #'   (i.e. conforming to the primed [`ParamSet`][paradox::ParamSet]). If this is `TRUE` (default), then the return value of `$.operate()`
    #'   is checked for this and columns are put in the correct order.\cr
    #'   The `$endomorphsim` field will reflect this value.
    initialize = function(param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set = ps(),
        packages = character(0), dict_entry = NULL, dict_shortaccess = NULL, own_param_set = quote(self$param_set), endomorphism = TRUE) {
      assert_subset(param_classes, c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), empty.ok = FALSE)
      if (inherits(param_set, "ParamSet")) {
        private$.param_set = assert_param_set(param_set)
        if (paradox_context_available) {
          private$.param_set$context_available = "inst"
        }
        private$.param_set_source = NULL
      } else {
        lapply(param_set, function(x) assert_param_set(eval(x)))
        private$.param_set_source = param_set
      }
      private$.param_classes = param_classes
      private$.packages = unique(assert_character(packages, any.missing = FALSE))
      private$.dict_entry = assert_string(dict_entry, null.ok = TRUE)
      private$.dict_shortaccess = assert_character(dict_shortaccess, null.ok = TRUE)
      assert_true(is.language(own_param_set))
      if (paradox_context_available && !inherits(param_set, "ParamSet")) {
        # Doing the following in a less convoluted way gives `Error in ps()$context_available = "inst"` because of the default value of own_param_set.
        eval(substitute({x = own_param_set ; x$context_available = "inst"}, list(own_param_set = own_param_set)))  # TODO: there has to be a better way
      }
      private$.own_param_set_symbol = own_param_set
      private$.own_defaults = assert_r6(eval(own_param_set), "ParamSet")$values
      private$.endomorphism = assert_flag(endomorphism)
    },
    #' @description
    #' Create a [`call`][base::call] object representing this operator.
    #' @param skip_defaults (`logical(1)`)\cr
    #'   Whether to skip construction arguments that have their default value. Default `TRUE`.
    #' @param show_params (`logical(1)`)\cr
    #'   Whether to show [`ParamSet`][paradox::ParamSet] values. Default `TRUE`.
    #' @param show_constructor_args (`logical(1)`)\cr
    #'   Whether to show construction args that are not [`ParamSet`][paradox::ParamSet] values. Default `TRUE`.
    #' @param ... (any)\cr
    #'   Ignored.
    repr = function(skip_defaults = TRUE, show_params = TRUE, show_constructor_args = TRUE, ...) {
      assert_flag(skip_defaults)
      assert_flag(show_params)
      assert_flag(show_constructor_args)
      initformals = formals(self$initialize)
      formalvalues = list()
      deviantformals = list()
      deviantparams = list()

      selfnames = names(self)
      ownps = eval(private$.own_param_set_symbol)
      pnames = ownps$ids()
      pnamesrep = pnames
      if (private$.own_param_set_id != "") {
        pnamesrep = sprintf("%s.%s", private$.own_param_set_id, pnames)
      }
      names(pnamesrep) = pnames

      representable = (!show_constructor_args || all(names(initformals) %in% selfnames)) &&
        !is.null(self$dict_entry) && !is.null(self$dict_shortaccess) &&
        !any(names(initformals) %in% pnames)

      if (representable) {
        for (formalname in if (show_constructor_args) names(initformals)) {

          truevalue = self[[formalname]]
          truerep = repr(truevalue, skip_defaults = skip_defaults, show_params = show_params, show_constructor_args = show_constructor_args, ...)

          validrep = tryCatch({
            eval(truerep, envir = environment(self$initialize))
            TRUE
          }, error = function(e) {
            FALSE
          })

          has_inferred_value = FALSE
          if (skip_defaults && validrep) {
            inferredvalue = NULL
            tryCatch({
              inferredvalue = eval(initformals[[formalname]], envir = formalvalues, enclos = environment(self$initialize))
              has_inferred_value = TRUE
            }, error = function(e) NULL)
            has_inferred_value = has_inferred_value && identical(truerep, repr(inferredvalue, skip_defaults = skip_defaults, show_params = show_params, show_constructor_args = show_constructor_args, ...))
          }
          if (!has_inferred_value) {
            deviantformals[[formalname]] = truerep
          }
          formalvalues[[formalname]] = truevalue
        }

        for (paramname in if (show_params) pnames) {
          truevalue = ownps$values[[paramname]]
          truerep = repr(truevalue, skip_defaults = skip_defaults, show_params = show_params, show_constructor_args = show_constructor_args, ...)
          validrep = tryCatch({
            eval(truerep, envir = environment(self$initialize))
            TRUE
          }, error = function(e) {
            FALSE
          })
          if (!skip_defaults || !validrep || !identical(truerep, repr(private$.own_defaults[[paramname]], skip_defaults = skip_defaults, show_params = show_params, show_constructor_args = show_constructor_args, ...))) {

            deviantparams[[pnamesrep[[paramname]]]] = truerep
          }
        }
      }
      if (!representable) {
        return(substitute(stop(msg), list(msg = sprintf("<%s>", class(self)[[1]]))))
      }
      as.call(c(list(as.symbol(self$dict_shortaccess), self$dict_entry), deviantparams, deviantformals))
    },
    #' @description
    #' Print this operator.
    #' @param verbose (`logical(1)`)\cr
    #'   Whether to show all construction arguments, even the ones at default values. Default `FALSE`.
    #' @param ... (any)\cr
    #'   Ignored.
    print = function(verbose = FALSE, ...) {
      hasparams = length(self$param_set$ids())
      txt = paste(utils::capture.output(repr(self, skip_defaults = !verbose, show_params = FALSE)), collapse = "\n")
      txt = paste0(gsub("stop\\(\"<([^>]*)>\"\\)", "<\\1>", txt), "\n$param_set:", if (hasparams) "\n" else " empty.\n")
      cat(txt)
      if (hasparams) {
        pids = as.data.table(self$param_set)[, c("id", "lower", "upper", "levels")]
        pids = cbind(pids, value = self$param_set$values[pids$id])
        print(pids)
      }
    },
    #' @description
    #' Prepare the `MiesOperator` to function on the given [`ParamSet`][paradox::ParamSet]. This must be called before
    #' `$operate()`. It may be called multiple times in the lifecycle of the `MiesOperator` object, and prior primings are
    #' forgotten when priming on a new [`ParamSet`][paradox::ParamSet]. The [`ParamSet`][paradox::ParamSet] on which
    #' the `MiesOperator` was last primed can be read from `$primed_ps`.
    #' @param param_set ([`ParamSet`][paradox::ParamSet])\cr
    #'   The [`ParamSet`][paradox::ParamSet] to which all `values` tables passed to `$operate()` will need to conform to.
    #'   May only contiain [`Domain`][paradox::Domain] objects that conform to the classes listed in `$param_classes`.
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
    #'   Individuals to operate on. Must pass the check of the [`ParamSet`][paradox::ParamSet] given in the last `$prime()` call
    #'   and may not have any missing components.
    #' @param ... (any)\cr
    #'   Depending on the concrete class, passed on to `$.operate()`.
    #' @return `data.frame`: the result of the operation. If the input was a [`data.table`][data.table::data.table] instead of
    #'   a `data.frame`, the output is also [`data.table`][data.table::data.table].
    operate = function(values, ...) {
      if (is.null(private$.primed_ps)) stop("Operator must be primed first!")
      ids = private$.primed_ps$ids()
      if (getOption("miesmuschel.testing")) private$.primed_ps$assert_dt(values)
      assert_names(colnames(values), permutation.of = private$.primed_ps$ids())
      convert = !is.data.table(values)
      if (convert) {
        # don't change input by reference
        values = as.data.table(values)
      }
      # load packages
      require_namespaces(self$packages, msg = sprintf("The following packages are required for %s operator: %%s", class(self)[[1]]))
      # make sure input / output cols are in the order as inndicated by paramset --> use `match` on input (and output if endomorphic)
      values = private$.operate(values[, match(ids, colnames(values), 0), with = FALSE], ...)
      if (self$endomorphism) {
        if (getOption("miesmuschel.testing")) values = private$.primed_ps$assert_dt(values)[, match(ids, colnames(values), 0), with = FALSE]
        if (convert) {
          setDF(values)
        }
      }
      values
    },
    #' @description
    #' Run [`utils::help()`] for this object.
    #' @param help_type (`character(1)`)\cr
    #'   One of `"text"`, `"html"`, or `"pdf"`: The type of help page to open. Defaults to the `"help_type"` option.
    #' @return `help_files_with_dopic` object, which opens the help page.
    help = function(help_type = getOption("help_type")) {
      parts = strsplit(self$man, split = "::", fixed = TRUE)[[1]]
      match.fun("help")(parts[[2]], package = parts[[1]], help_type = help_type)
    }
  ),
  active = list(
    #' @field param_set ([`ParamSet`][paradox::ParamSet])\cr
    #' Configuration parameters of the `MiesOperator` object. Read-only.
    param_set = function(val) {
      if (is.null(private$.param_set)) {
        # TODO: need test that checks that all paramset elements have good context
        sourcelist = lapply(private$.param_set_source, function(x) eval(x))
        if (length(sourcelist) > 1) {
          private$.param_set = ParamSetCollection$new(sourcelist)
        } else {
          private$.param_set = sourcelist[[1]]
        }


        if (!paradox_s3 && !is.null(private$.param_set_id)) private$.param_set$set_id = private$.param_set_id
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
    #' @field packages (`character`)\cr
    #' Packages needed for the operator. Read-only.
    packages = function(val) {
      if (!missing(val)) stop("packages is read-only.")
      private$.packages
    },
    #' @field dict_entry (`character(1)` | `NULL`)\cr
    #' Key of this class in its respective [`Dictionary`][mlr3misc::Dictionary].
    #' Is `NULL` if this class it not (known to be) in a [`Dictionary`][mlr3misc::Dictionary]. Read-only.
    dict_entry = function(val) {
      if (!missing(val)) stop("dict_entry is read-only.")
      private$.dict_entry
    },
    #' @field dict_shortaccess (`character(1)` | `NULL`)\cr
    #' Name of [`Dictionary`][mlr3misc::Dictionary] short-access function where an object of this class can be retrieved.
    #' Is `NULL` if this class is not (known to be) in a [`Dictionary`][mlr3misc::Dictionary]
    #' with a short-access function. Read-only.
    dict_shortaccess = function(val) {
      if (!missing(val)) stop("dict_shortaccess is read-only.")
      private$.dict_shortaccess
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
    },
    #' @field man (`character(1)`)\cr
    #' Name of this class, in the form `<package>::<classname>`. Used by the `$help()` method.
    man = function(x) {
      if (!missing(x)) stop("man is read-only")
      paste0(topenv(self$.__enclos_env__)$.__NAMESPACE__.$spec[["name"]], "::", class(self)[[1]])
    }
  ),
  private = list(
    deep_clone = function(name, value) {
      if (!is.null(private$.param_set_source)) {
        if (!is.null(private$.param_set)) {
          if (!paradox_s3) private$.param_set_id = private$.param_set$set_id
          private$.param_set = NULL  # required to keep clone identical to original, otherwise tests get really ugly
        }
        if (name == ".param_set_source") {
          value = lapply(value, function(x) {
            if (inherits(x, "R6")) x$clone(deep = TRUE) else x  # nocov
          })
        }
      }
      if (is.environment(value) && !is.null(value[[".__enclos_env__"]])) {
        return(value$clone(deep = TRUE))
      }
      value
    },
    .param_set = NULL,
    .param_set_id = NULL,  # obsolete with s3 paradox
    .primed_ps = NULL,
    .param_classes = NULL,
    .param_set_source = NULL,
    .operate = function(values, ...) stop(".operate needs to be implemented by inheriting class."),
    .packages = NULL,
    .dict_entry = NULL,
    .dict_shortaccess = NULL,
    .own_param_set_symbol = NULL,
    .own_param_set_id = "",
    .own_defaults = NULL,
    .endomorphism = NULL
  )
)
