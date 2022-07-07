
DictionaryEx = R6Class("DictionaryEx",
  inherit = Dictionary,
  public = list(
    metainf = new.env(parent = emptyenv()),
    add = function(key, ..., aux_construction_args = list()) {
      assert_list(aux_construction_args)
      lapply(aux_construction_args, function(x) {
        assert(check_atomic(x), if (is.language(x)) TRUE else "Must be a language-object", .var.name = "All elements of aux_construction_args")
      })
      super$add(key = key, ...)
      self$items[[key]]$aux_construction_args = aux_construction_args
      invisible(self)
    }
  )
)



#' @title Dictionary of Mutators
#'
#' @export
dict_mutators = R6Class("DictionaryMutator",
  inherit = DictionaryEx,
  public = list(
    #' @description
    #' Show help for a contained object, by its entry key.
    #' @template param_key
    #' @template param_help_type
    help = function(key, help_type = getOption("help_type")) dict_help(self = self, private = private, key = key, help_type = help_type)
  ),
  private = list(.dict_name = "dict_mutators"),
  cloneable = FALSE
)$new()

#' @title Dictionary of Recombinators
#'
#' @export
dict_recombinators = R6Class("DictionaryRecombinator",
  inherit = DictionaryEx,
  public = list(
    #' @description
    #' Show help for a contained object, by its entry key.
    #' @template param_key
    #' @template param_help_type
    help = function(help_type = getOption("help_type")) dict_help(self = self, private = private, help_type = help_type)
  ),
  private = list(.dict_name = "dict_recombinators"),
  cloneable = FALSE
)$new()

#' @title Dictionary of Selectors
#'
#' @export
dict_selectors = R6Class("DictionarySelector",
  inherit = DictionaryEx,
  public = list(
    #' @description
    #' Show help for a contained object, by its entry key.
    #' @template param_key
    #' @template param_help_type
    help = function(key, help_type = getOption("help_type")) dict_help(self = self, private = private, key = key, help_type = help_type)
  ),
  private = list(.dict_name = "dict_selectors"),
  cloneable = FALSE
)$new()

#' @title Dictionary of Scalors
#'
#' @export
dict_scalors = R6Class("DictionaryScalor",
  inherit = DictionaryEx,
  public = list(
    #' @description
    #' Show help for a contained object, by its entry key.
    #' @template param_key
    #' @template param_help_type
    help = function(key, help_type = getOption("help_type")) dict_help(self = self, private = private, key = key, help_type = help_type)
  ),
  private = list(.dict_name = "dict_scalors"),
  cloneable = FALSE
)$new()

#' @title Dictionary of Filtors
#'
#' @export
dict_filtors = R6Class("DictionaryFiltor",
  inherit = DictionaryEx,
  public = list(
    #' @description
    #' Show help for a contained object, by its entry key.
    #' @template param_key
    #' @template param_help_type
    help = function(key, help_type = getOption("help_type")) dict_help(self = self, private = private, key = key, help_type = help_type)
  ),
  private = list(.dict_name = "dict_filtors"),
  cloneable = FALSE
)$new()

dict_help = function(self, private, key, help_type) {
  assert_string(key)
  allkeys = self$keys()
  if (key %nin% allkeys) stopf("Element '%s' not in %s.%s", key, class(self)[[1]], did_you_mean(key, allkeys))
  package = topenv(self$.__enclos_env__)$.__NAMESPACE__.$spec[["name"]]
  # need to put 'package' in parentheses because help() otherwise appears to do as.character(substitute()) with it
  h = tryCatch(match.fun("help")(sprintf("%s_%s", private$.dict_name, key), package = (package), help_type = help_type),
    error = function(e) e
  )
  if (!length(h) || inherits(h, "error")) {
    tryCatch({
      h = invoke(self$get, key = key, lapply(self$items[[key]]$aux_construction_args, eval))$help()
    }, error = function(e) NULL)
  }
  if (inherits(h, "error")) stop(h)
  h
}

#' @title Short Access Forms for Operators
#'
#' @description
#' These functions complement [dict_mutators], [dict_recombinators], [dict_selectors] with functions in the spirit
#' of [mlr3::mlr_sugar].
#'
#' @inheritParams mlr3::mlr_sugar
#' @return
#' * [`Mutator`] for `mut()`
#' * list of [`Mutator`] for `muts()`
#' * [`Recombinator`] for `rec()`.
#' * list of [`Recombinator`] for `recs()`.
#' * [`Selector`] for `sel()`.
#' * list of [`Selector`] for `sels()`.
#' * [`Scalor`] for `scl()`.
#' * list of [`Scalor`] for `scls()`.
#' @export
#' @examples
#' mut("gauss", sdev = 0.5)
#' rec("xounif")
#' sel("random")
#' scl("nondom")
mut = function(.key, ...) {
  dictionary_sugar(dict_mutators, .key, ...)
}

#' @rdname mut
#' @export
muts = function(.keys, ...) {
  miesmuschel_dictionary_mget(dict_mutators, .keys, ...)
}

#' @rdname mut
#' @export
rec = function(.key, ...) {
  dictionary_sugar(dict_recombinators, .key, ...)
}

#' @rdname mut
#' @export
recs = function(.key, ...) {
  miesmuschel_dictionary_mget(dict_recombinators, .key, ...)
}

#' @rdname mut
#' @export
sel = function(.key, ...) {
  dictionary_sugar(dict_selectors, .key, ...)
}

#' @rdname mut
#' @export
sels = function(.key, ...) {
  miesmuschel_dictionary_mget(dict_selectors, .key, ...)
}

#' @rdname mut
#' @export
scl = function(.key, ...) {
  dictionary_sugar(dict_scalors, .key, ...)
}

#' @rdname mut
#' @export
scls = function(.key, ...) {
  miesmuschel_dictionary_mget(dict_scalors, .key, ...)
}

#' @rdname mut
#' @export
ftr = function(.key, ...) {
  dictionary_sugar(dict_filtors, .key, ...)
}

#' @rdname mut
#' @export
ftrs = function(.key, ...) {
  miesmuschel_dictionary_mget(dict_filtors, .key, ...)
}


miesmuschel_dictionary_mget = function(dict, .keys, ...) {
  if (missing(.keys)) {
    return(dict)
  }
  values = lapply(X = .keys, FUN = dictionary_sugar_get, dict = dict, ...)
  values
}
