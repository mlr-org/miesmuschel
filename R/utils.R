ps_flatten = function(param_set, clone = TRUE) {
  assert_flag(clone)
  assert_r6(param_set, "ParamSet")
  if (class(param_set)[[1]] == "ParamSet") {
    if (clone) param_set$clone(deep = TRUE) else param_set
  } else {
    ret = ParamSet$new()$add(param_set)
    ret$deps = param_set$deps
    ret$set_id = param_set$set_id
    ret
  }
}

# TODO this is from paradox, probably it should just be exported from there
# Create a ParamSet from a list of ParamSets
# This emulates `ParamSetCollection$new(sets)`, except that
# - The result is a `ParamSet`, not a `ParamSetCollection`
# - The ParamSets are allowed to have `$trafo`, which are collected together into a single function.
# This emulates `ParamSetCollection$new(sets)`, which in particular means that the resulting ParamSet has all the Params
# from the input `sets`, but some `$id`s are changed: If the ParamSet has a non-empty `set_id`, then the Params will
# have their <id> changed to <set_id>.<id>. This is also reflected in deps and in `$trafo`.
# @param sets: list of ParamSet
ps_union = function(sets) {  # nocov start
  assert_list(sets, types = "ParamSet")
  assert_names(discard(map_chr(sets, "set_id"), `==`, ""), type = "unique")

  psc = ParamSetCollection$new(map(sets, function(x) {
    if (x$has_trafo) {
      # PSC can not use ParamSet with a `$trafo` that is set.
      x = x$clone()
      x$trafo = NULL
    }
    x
  }))

  newps = ParamSet$new()$add(psc)

  # This loop collects information that is needed by the trafo.
  # Resulting is a list of named lists, with one element per `sets` entry. Elements of the named lists are:
  # - trafo: trafo of the given ParamSet
  # - set_id: set_id of the given ParamSet
  # - forward_name_translation: named `character`. Names are the Param IDs of the resulting newps,
  #   values are the Param IDs of the original Params in the `sets` argument.
  #   E.g. if a single ParamSet with set_id "sid" and with one Param with id "pid" is given,
  #   then this is a `c(sid.pid = "pid")`.
  #   Why is this needed? If the $trafo() is given a value `list(sid.pid = 1)`, then
  #   `forward_name_translation` can be used to rename this to `list(pid = 1)`, which is what the
  #   original trafo expects.
  setinfo = map(unname(sets), function(s) {
    sparams = s$params  # avoid slow ParamSetCollection $params active binding
    sinfo = list(
      trafo = s$trafo,
      set_id = s$set_id,
      forward_name_translation = names2(sparams)
    )
    psids = names2(sparams)
    if (s$set_id != "") {
      psids = sprintf("%s.%s", s$set_id, psids)
    }
    names(sinfo$forward_name_translation) = psids
    sinfo
  })

  if (any(map_lgl(sets, "has_trafo"))) {
    # allnames: names of all parameters, as seen from the outside
    allnames = names2(unlist(map(setinfo, "forward_name_translation")))
    assert_set_equal(allnames, names2(newps$params))  # this should always be the case

    newps$trafo = crate(function(x, param_set) {
      res = unlist(mlr3misc::map(setinfo, function(s) {
        trafo = s$trafo
        # get the parameter values that the current trafo should operate on,
        # as identified by the names in forward_name_translation
        pv = x[match(names(s$forward_name_translation), names(x), nomatch = 0)]
        if (!is.null(trafo)) {
          # translate name from "<set_id>.<param_id>" to "<param_id>"
          names(pv) = s$forward_name_translation[names(pv)]
          pv = trafo(pv)

          # append prefix again. trafo() could have changed parameter names, so
          # we can't use any cached name_translation magic here
          if (s$set_id != "") {
            names(pv) = sprintf("%s.%s", s$set_id, names(pv))
          }
        }
        pv
      }), recursive = FALSE)

      # add the Params that were not translated at all, because the ParamSet doesn't know about them.
      res = c(mlr3misc::remove_named(x, allnames), res)

      res[c(intersect(names(res), names(x)), setdiff(names(res), names(x)))]  # unchanged parameter names stay in order
    }, setinfo, allnames)
  }
  newps
}  # nocov end

# call inst$eval_batch(xdt), but handle the case where xdt has length 0 correctly.
eval_batch_handle_zero = function(inst, xdt) {
  if (nrow(xdt)) {
    return(inst$eval_batch(xdt))
  }
  assert_data_table(xdt)
  assert_names(colnames(xdt), must.include = inst$search_space$ids())

  result_types = inst$archive$codomain$storage_type
  typegen = function(x) switch(x,
    logical = logical(0),
    integer = integer(0),
    numeric = numeric(0),
    character = character(0)
  )

  invisible(as.data.table(lapply(result_types, typegen)))
}

## Component names that can not be used inside MIES functions
reserved_component_names = c("x_domain", "timestamp", "batch_nr", "eol", "dob")


# tolerance of numeric configuration parameter bounds
default_tol = sqrt(.Machine$double.eps)

# add / subtract appropriate tolerance to bound.
# Tolerance `tol` is added. It is used as absolute *and* relative tolerance,
# depending on which is bigger.
# @param bound: `numeric`, indicating lower or upper bound
# @param side: "lower" -> `bound` is lower bound, subtract tol. "upper" --> `bound` is upper bound etc.
# @param tol: tolerance to add, defaults to default_tol.
# @return: bounds with tolerance term added / subtracted
tol_bound = function(bound, side = c("lower", "upper"), tol = default_tol) {
  side = match.arg(side)
  bound + tol * pmax(1, bound) * switch(side, lower = -1, upper = 1)
}

# Check that the optiminstance is OK for us:
# - is an OptimInstance R6 object
# - neither search_space nor codomain contain reserved components
# - no overlap in names between domain and codomain
assert_optim_instance = function(inst) {
  assert_r6(inst, "OptimInstance")
  search_space_ids = inst$search_space$ids()
  codomain_ids = inst$objective$codomain$ids()
  assert_names(search_space_ids, disjunct.from = reserved_component_names, .var.name = "inst$search_space$ids()")
  assert_names(codomain_ids, disjunct.from = reserved_component_names, .var.name = "inst$objective$codomain$ids()")
  assert_names(search_space_ids, disjunct.from = codomain_ids, .var.name = "inst$search_space$ids()")
  invisible(inst)
}

check_fidelity_schedule = function(x) {
  if (test_data_frame(x, ncols = 3, min.rows = 1) &&
      test_names(colnames(x), identical.to = c("generation", "budget_new", "budget_survivors")) &&
      test_integerish(x$generation, tol = 1e-100, any.missing = FALSE, unique = TRUE) &&
      1 %in% x$generation) {
    TRUE
  } else {
    "must be a data.frame with integer column 'generation' (with unique non-missing values and at least one row with value 1) and columns 'budget_new', 'budget_survivors'."
  }
}
