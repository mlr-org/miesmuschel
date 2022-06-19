
#' @title Dictionary of Mutators
#'
#' @export
dict_mutators = R6Class("DictionaryMutator",
  inherit = Dictionary,
  cloneable = FALSE
)$new()

#' @title Dictionary of Recombinators
#'
#' @export
dict_recombinators = R6Class("DictionaryRecombinator",
  inherit = Dictionary,
  cloneable = FALSE
)$new()

#' @title Dictionary of Selectors
#'
#' @export
dict_selectors = R6Class("DictionarySelector",
  inherit = Dictionary,
  cloneable = FALSE
)$new()

#' @title Dictionary of Scalors
#'
#' @export
dict_scalors = R6Class("DictionaryScalor",
  inherit = Dictionary,
  cloneable = FALSE
)$new()

#' @title Dictionary of Filtors
#'
#' @export
dict_filtors = R6Class("DictionaryFiltor",
  inherit = Dictionary,
  cloneable = FALSE
)$new()

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
