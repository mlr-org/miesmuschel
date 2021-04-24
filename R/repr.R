
#' @title Create a 'call' Object Representation
#'
#' `repr()` creates a [`call`][base::call] object representing `obj`, if possible.
#' Evaluating the call should come close to recreating the original object.
#'
#' In the most trivial cases, it should be possible to recreate objects from their
#' representation by evaluating them using [`eval()`][base::eval]. Important exceptions
#' are:
#' * Functions are represented by their source code, if available, and by their AST if not.
#'   This drops the context from their environments and recreated objects will not work
#'   if they contain functions that depend on specific environments
#' * [`environment`][base::environment]s are not represented.
#' * [`R6`][R6::R6Class] objects are only represented if they have a `$repr()` function.
#'   This function may have arbitrary arguments, and should have a `...` argument to
#'   capture ignored arguments.
#'
#' Objects that can not be represented are currently mapped to the call `stop("<###>")`, where
#' `###` is a short description of the non-representable object.
#'
#' @param obj (any)\cr
#'   Object to create a representation of.
#' @param ... (any)\cr
#'   Further arguments to be passed to class methods. Currently in use are:
#'   * `skip_defaults` (`logical(1)`) whether to skip construction arguments that have their default value. Default `TRUE`.
#'   * `show_params` (`logical(1)`) whether to show [`ParamSet`][paradox::ParamSet] values. Default `TRUE`.
#'   * `show_constructor_args` (`logical(1)`) whether to show construction args that are not [`ParamSet`][paradox::ParamSet] values. Default `TRUE`.
#' @return [`call`][base::call]: A call that, when evaluated, tries to re-create the object.
#' @export
repr = function(obj, ...) {
  UseMethod("repr", obj)
}

#' @export
repr.default = function(obj, ...) {
  if (is.list(obj)) return(repr.list(obj))
  parse(text = deparse(obj, backtick = TRUE,
    control = c("keepInteger", "quoteExpressions", "showAttributes", "useSource", "niceNames")
  ))[[1]]
}

#' @export
repr.data.table = function(obj, ...) {
  careful = any(colnames(obj) %in% names(formals(data.table)))
  objkey = key(obj)
  preclass = class(obj)

  obj = as.data.frame(obj)

  att = attributes(obj)
  attributes(obj)[setdiff(names(attributes(obj)), "names")] = NULL
  if (!identical(preclass, c("data.table", "data.frame"))) {
    att$class = preclass
  } else {
    att$class = NULL
  }

  call = repr(obj, ...)

  if (careful) {
    call = substitute(data.table::as.data.table(x), list(x = call))
  } else {
    call[[1]] = substitute(data.table::data.table)
  }
  if (!is.null(objkey)) {
    call[[length(call) + 1]] = objkey
    names(call)[[length(call)]] = "key"
  }
  wrap_attributes(call, att)
}

#' @export
repr.environment = function(obj, ...) {
  quote(stop("<environment>"))
}

#' @export
repr.data.frame = function(obj, ...) {
  careful = any(colnames(obj) %in% names(formals(data.frame)))
  has_strings = some(obj, is.character)  # stringsAsFactors = FALSE
  has_empty_names = is.null(names(obj)) || some(names(obj), identical, "")  # --> fix.empty.names = FALSE
  has_bad_names = !is.null(names(obj)) && any(names(obj) != "" & make.names(names(obj), unique = TRUE) != names(obj))  # --> optional = TRUE (as.data.frame), check.names = FALSE (data.frame)
  rownames = .row_names_info(obj, type = 0)
  if ((is.integer(rownames) && length(rownames) == 2 && is.na(rownames[[1]])) || !is.atomic(rownames)) {
    # data.frame has no rownames beyond line numbering
    rownames = NULL
  }

  preclass = class(obj)

  att = attributes(obj)
  attributes(obj)[setdiff(names(attributes(obj)), "names")] = NULL
  if (!identical(preclass, "data.frame")) {
    att$class = preclass
  } else {
    att$class = NULL
  }
  if (is.atomic(rownames)) {
    att$row.names = NULL
  }

  call = repr(obj, ...)

  if (careful) {
    call = substitute(as.data.frame(x), list(x = call))
  } else {
    call[[1]] = substitute(data.frame)
  }
  if (!is.null(rownames)) {
    call[[length(call) + 1]] = repr(rownames, ...)
    names(call)[[length(call)]] = "row.names"
  }
  if (has_strings) {
    call[[length(call) + 1]] = FALSE
    names(call)[[length(call)]] = "stringsAsFactors"
  }
  if (has_empty_names) {
    call[[length(call) + 1]] = FALSE
    names(call)[[length(call)]] = "fix.empty.names"
  }
  if (has_bad_names) {
    call[[length(call) + 1]] = careful
    names(call)[[length(call)]] = if (careful) "optional" else "check.names"
  }
  wrap_attributes(call, att)
}


#' @export
repr.list = function(obj, ...) {
  att = attributes(obj)
  att$row.names = .row_names_info(obj, type = 0)  # row.names special storage type
  call = as.call(c(list(quote(list)), lapply(obj, function(x) repr(x, ...))))
  wrap_attributes(call, att)
}


wrap_attributes = function(call, att) {
  att$names = NULL
  if (length(att)) {
    as.call(c(list(quote(structure), call), att))
  } else {
    call
  }
}

#' @export
repr.R6 = function(obj, ...) {
  if (is.function({cl = .subset2(obj, "repr")})) {
    cl(...)
  } else {
    substitute(stop(msg), list(msg = sprintf("<%s>", class(obj)[[1]])))
  }
}

#' @export
`!.MiesOperator` = function(x) {
  repr(x)
}
