
#' @title Create a 'call' Object Representation
#'
#' @description
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
  ## TODO: avoid too deep recursion
  UseMethod("repr", obj)
}

#' @export
repr.default = function(obj, ...) {
  if (is.list(obj)) return(repr.list(obj))
  text = deparse(obj, backtick = TRUE,
    control = c("keepInteger", "quoteExpressions", "showAttributes", "useSource", "niceNames")
  )
  tryCatch(parse(text = text)[[1]], error = function(e) quote(stop(text)))
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
repr.function = function(obj, ...) {

  function_raw = parse(text = deparse(obj, backtick = TRUE,
    control = c("keepInteger", "quoteExpressions", "showAttributes", "useSource", "niceNames")
  ))[[1]]

  env = environment(obj)
  if (is.null(env)) {  # no environment, possibly a primitive
    return(function_raw)
  }
  enclosings = list()
  while (!isNamespace(env) && !identical(env, globalenv()) && !identical(env, emptyenv()) && !identical(env, baseenv())) {
    enclosings[[length(enclosings) + 1]] = as.list(env, all.names = TRUE)
    env = parent.env(env)
  }

  envname = if (identical(env, baseenv())) "R_BaseEnv" else environmentName(env)

  findInList = function(obj, l) {
    names(l)[sapply(l, identical, y = obj)] %??% character(0)  # avoid returning NULL here
  }
  # sanitize function names, should they be found in a namespace: surround by '`', and escape '`' and '\'
  sanitize = function(x) {
    if (make.names(x) == x) x else sprintf("`%s`", gsub("([\\`])", "\\\\\\1", x))
  }

  found_in_namespace = if (identical(env, globalenv())) character(0) else findInList(obj, as.list(env, all.names = TRUE))
  if (length(found_in_namespace)) {
    # the function occurs in a namespace, so we just return '<namespace>::<name>' or '<namespace>:::<item>'.
    # this *could* be baseenv(), but it can not be .GlobalEnv (we exclude this above) and it can not be emptyenv()
    # (since that one is empty).
    if (identical(env, baseenv()) || identical(env, .getNamespace("base"))) {  # these two are different...
      # 'base' has no exports, everything is exported (?)
      str = paste0("base::", sanitize(found_in_namespace[[1]]))
    } else {
      # check if we need to '::' or ':::', depending on exports
      exported = getNamespaceInfo(env, "exports")
      found_in_exports = findInList(obj, as.list(exported, all.names = TRUE))
      if (length(found_in_exports)) {
        # it is possible that something has two names in the namespace, but only one name in the exports.
        # We therefore make sure to use the right name here by referring to found_in_exports

        # we use 'envname' since the only broken envnames (R_BaseEnv etc.) are of environments for which we
        # never reach this codepath.
        str = paste0(envname, "::", sanitize(found_in_exports[[1]]))
      } else {
        # 'envname' see above.
        str = paste0(envname, ":::", sanitize(found_in_namespace[[1]]))
      }
    }
    return(parse(text = str)[[1]])
  }


  selfreflist = list()
  for (idx in as.numeric(seq_along(enclosings))) {
    enc = enclosings[[idx]]
    sr = findInList(obj, enc)
    selfreflist[[idx]] = named_vector(sr, idx)
    enclosings[[idx]][sr] = NULL  # avoid recursion when we 'repr' enclosing environments by removing self-refs
  }
  selfref = unlist(selfreflist)
  if (length(selfref) == 1 && selfref == 1) {
    selfref = names(selfref)
  }

  # function not found in any environment --> we build a crate_env call.
  crate_env_call = c(list(quote(crate_env), function_raw, envname), lapply(enclosings, repr))

  if (length(selfref)) crate_env_call$selfref = selfref

  as.call(crate_env_call)
}

#' @export
`!.MiesOperator` = function(x) {
  repr(x)
}

#' @title Set a Function's Environment
#'
#' @description
#' Useful to represent functions efficiently within `repr()`.
#'
#' @param fun (`function`)\cr
#'   Function of which the environment should be set.
#' @param namespace (`character(1)`)\cr
#'   Name of namespace within which `fun` should be placed, as given by
#'   [`environmentName()`][base::environmentName]. Special values
#'   `"R_GlobalEnv"` (global environment), `"R_BaseEnv"` (base environment;
#'   note this one is non-standard within R), `"R_EmptyEnv"` (empty environment).
#'   Default `"R_GlobalEnv"`.
#' @param ... (`list`)\cr
#'   Content of environments within which to place `fun`.
#' @param selfref (`character(1)` | named `integer` | `NULL`)\cr
#'   If `character(1)`: The name of the entry of the first element in `...` that
#'   refers to the function itself. If a named `integer`, then the values
#'   indicate the lists in `...` where the reference to the function should be
#'   placed see examples. Default `NULL`: No reference to the function itself is
#'   present.
#' @return `function`: The given `fun` with changed environment.
#' @examples
#'
#' identity2 = crate_env(function(x) x, "base")
#' identical(identity, identity2)  # TRUE
#'
#' y = 1
#' f1 = mlr3misc::crate(function(x) x + y, y, .parent = .GlobalEnv)
#' f2 = crate_env(function(x) x + y, "R_GlobalEnv", list(y = 1))
#'
#' # Note identical() does not apply because both contain (equal, but not
#' # identical) 'y = 1'-environments
#' all.equal(f1, f2)  # TRUE
#' f1(10)  # 10 + 1 == 11
#'
#' factorial1 = mlr3misc::crate(
#'   function(x) if (x > 0) x * factorial1(x - 1) else 1,
#'   y, .parent = .GlobalEnv
#' )
#' environment(factorial1)$factorial1 = factorial1
#'
#' factorial2 = crate_env(
#'   function(x) if (x > 0) x * factorial1(x - 1) else 1,
#'   "R_GlobalEnv", list(y = 1), selfref = "factorial1")
#' # putting 'factorial1' into the list (or repeating function(x) ....)
#' # would *not* work, since we want:
#' identical(environment(factorial2)$factorial1, factorial2)  # TRUE
#'
#' all.equal(factorial1, factorial2)  # TRUE
#'
#' g = crate_env(function(x) x + y + z, "miesmuschel",
#'   list(y = 1), list(z = 2), selfref = c(X = 1, Y = 2, Z = 2))
#' g(0)  # 0 + 1 + 2 == 3
#' identical(environment(g)$X, g)
#' identical(parent.env(environment(g))$Y, g)
#' identical(parent.env(environment(g))$Z, g)
#' identical(
#'   parent.env(parent.env(environment(g))),
#'   loadNamespace("miesmuschel")
#' )
#' @export
crate_env = function(fun, namespace = "R_GlobalEnv", ..., selfref = NULL) {
  assertString(namespace)
  enclosings = assertList(list(...), any.missing = FALSE, names = "unnamed", types = "list")
  assert(checkString(selfref, null.ok = TRUE), checkIntegerish(selfref, any.missing = FALSE, names = "named", lower = 1, upper = length(enclosings)))
  if (!length(enclosings) && !is.null(selfref)) stop("When no ... is given, then selfref must be NULL.")

  if (testString(selfref)) selfref = structure(1, names = selfref)

  if (namespace == "R_GlobalEnv") {
    ns = globalenv()
  } else if (namespace == "R_EmptyEnv") {
    ns = emptyenv()
  } else if (namespace == "R_BaseEnv") {
    # Unfortunately environmentName() of both baseenv() and namespace:base are the same, therefore this nonstandard naming here.
    ns = baseenv()
  } else {
    ns = .getNamespace(namespace)
    if (is.null(ns)) {
      ns = withCallingHandlers(loadNamespace(namespace), error = function(e) {
        stop(sprintf("namespace %s could not be loaded when reconstructing function in crate_env.", namespace))
      })
    }
  }

  if (length(enclosings)) {
    enc_envs = Reduce(function(enc, env) {
      list2env(enc, parent = env, size = length(enc) + 1)  # size + 1 to keep space for 'selfref'
    }, enclosings, ns, right = TRUE, accumulate = TRUE)
  } else {
    # this is necessary because Reduce() behaves differently when 'enclosings' has length 0 :-/
    enc_envs = list(ns)
  }

  environment(fun) = enc_envs[[1]]

  imap(selfref, function(idx, name) {
    target_env = enc_envs[[idx]]
    if (name %in% names(target_env)) stop("Name %s is already in '...'-element %s and can therefore not be inserted as function self reference.", name, idx)
    target_env[[name]] = fun
  })
  fun
}
