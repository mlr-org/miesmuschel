
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
#' @export
#' @examples
#' mut("gauss", sdev = 0.5)
#' rec("xounif")
#' sel("random")
mut = function(.key, ...) {
  dictionary_sugar(dict_mutators, .key, ...)
}

#' @rdname mut
#' @export
muts = function(.keys, ...) {
  dictionary_sugar_mget(dict_mutators, .keys, ...)
}

#' @rdname mut
#' @export
rec = function(.key, ...) {
  dictionary_sugar(dict_recombinators, .key, ...)
}

#' @rdname mut
#' @export
recs = function(.key, ...) {
  dictionary_sugar_mget(dict_recombinators, .key, ...)
}

#' @rdname mut
#' @export
sel = function(.key, ...) {
  dictionary_sugar(dict_selectors, .key, ...)
}

#' @rdname mut
#' @export
sels = function(.key, ...) {
  dictionary_sugar_mget(dict_selectors, .key, ...)
}
