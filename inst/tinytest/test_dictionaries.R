
source("setup.R", local = TRUE)

library("mlr3misc")

baseclasses = c("Recombinator", "Mutator", "Selector")
dictionaries = list(Mutator = dict_mutators, Recombinator = dict_recombinators, Selector = dict_selectors)
shortforms = list(Mutator = mut, Recombinator = rec, Selector = sel)
abstracts = c(baseclasses, "MutatorNumeric", "MutatorDiscrete")
# the constructors of the following don't know about their inheritance
exceptions = c("MutatorCombination", "RecombinatorCombination")

initargs = list(
  MutatorMaybe = list(mutator = mut("gauss")),
  MutatorCmpMaybe = list(mutator = mut("gauss")),
  MutatorCombination = list(operators = list(ParamAny = mut("gauss"))),
  RecombinatorMaybe = list(recombinator = rec("xounif")),
  RecombinatorCombination = list(operators = list(ParamAny = rec("xounif")))
)

# check if something inherits from any <baseclasses> without constructing it
inherits_from_base = function(r6cg) {
  if (r6cg$classname %in% baseclasses) return(TRUE)
  upper = r6cg$get_inherit()
  if (is.null(upper)) return(FALSE)
  inherits_from_base(upper)
}

# get a list of exported objects
nspath = dirname(system.file("NAMESPACE", package = "miesmuschel"))
exports = parseNamespaceFile(basename(nspath), dirname(nspath))$exports

pkgenv = asNamespace("miesmuschel")
operators = Filter(function(x) {
  objgen = get(x, pkgenv)
  "R6ClassGenerator" %in% class(objgen) && inherits_from_base(objgen)
}, ls(pkgenv, all.names = TRUE))

operators = setdiff(intersect(operators, exports), abstracts)


operators = c(operators, exceptions)

# for each operator that should be in one of the dictionaries, get the corresponding dict and key

dicts = sapply(operators, function(op) {
  for (type in names(dictionaries)) {
    dictobj = dictionaries[[type]]
    matches = keep(names(dictobj$items), function(n) {
      identical(dictobj$items[[n]]$value, get(op, pkgenv))
    })
    expect_character(matches, max.len = 1)
    if (length(matches)) {
      return(list(operator = op, base = type, name = matches))
    }
  }
  return(list(operator = op, base = NULL, name = "__NOT_FOUND__"))
}, simplify = FALSE)


# check that all exported R6 classes that inherit from <baseclasses> and are not <abstracts> have a dictionary
expect_true("__NOT_FOUND__" %nin% map_chr(dicts, "name"))

for (type in names(dictionaries)) {
  unexported = setdiff(dictionaries[[type]]$keys(), map(keep(dicts, function(x) identical(x$base, type)), "name"))
  expect_true(length(unexported) == 0, info = sprintf("%s in dictionary that is not exported: %s", type, str_collapse(unexported)))
}

# check whether we can construct each operator from dictionary and from itself, and get the same result.
for (opinfo in dicts) {
  constructor = get(opinfo$operator, pkgenv)  # constructor as found in pkg namespace
  dictname = opinfo$name  # key in dictionary
  constargs = initargs[[opinfo$operator]] %??% list() # required construction args
  dict = dictionaries[[opinfo$base]]

  test_obj = do.call(constructor$new, constargs)

  expect_inherits(test_obj, opinfo$base)  # inherits from the correct base class

  # check that dict_***$get()  gives the same object as **$new() does
  expect_equal(do.call(dict$get, c(list(dictname), constargs)), test_obj, info = dictname)

  # check that hyperparameters can be changed on construction
  # We do this by automatically generating a hyperparameter value that deviates from the automatically constructed one
  # We only skip this if there are no non-ParamUty parameters.
  eligible_params = test_obj$param_set$params[test_obj$param_set$class != "ParamUty"]
  eligible_params = discard(eligible_params, function(p) {
    # filter out discrete params with only one level, or numeric params with $lower == $upper
    # (Note numeric parameters have 0 levels, discrete parameters have lower == upper == NA)
    length(p$levels) < 2 && isTRUE(all.equal(p$lower, p$upper))
  })
  if (length(eligible_params)) {
    testingparam = eligible_params[[1]]
    origval = test_obj$param_set$values[[testingparam$id]]
    # we want to construct an object where the parameter value is different from the construction value
    # For this we take a few candidate values and filter for feasibility and inequality with original value
    candidates = c(as.list(testingparam$levels), list(testingparam$lower, testingparam$upper,
      testingparam$lower + 1, 0, testingparam$upper - 1, if (is.atomic(origval)) origval + 1))
    val = Filter(function(x) testingparam$test(x) && !is.na(x) && is.finite(x) && (!is.atomic(origval) || origval != x), candidates)[[1]]

    constargs[[testingparam$id]] = val

    pv_obj = do.call(shortforms[[opinfo$base]], c(list(dictname), constargs))
    expect_false(isTRUE(all.equal(test_obj, pv_obj)))
    test_obj$param_set$values[[testingparam$id]] = val
    expect_true(isTRUE(all.equal(test_obj, pv_obj)))
  }
}

