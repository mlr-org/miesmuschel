#' @title Mixed Integer Evolutionary Strategies Optimizer
#'
#' @include Selector.R
#' @include Mutator.R
#' @include Recombinator.R
#'
#' @description
#' Perform optimization using evolutionary strategies. `OptimizerMies` and `TunerMies` implement a standard ES optimization
#' algorithm, performing initialization first, followed by a loop of performance evaluation, survival selection, parent selection, mutation, and
#' recombination. Currently two different survival modes ("comma" and "plus") are supported. Multi-fidelity optimization, similar
#' to the "rolling-tide" algorithm described in `r cite_bib("fieldsend2014rolling")`. The modular design and reliance on
#' [`MiesOperator`] objects to perform central parts of the optimization algorithm makes this [`Optimizer`][bbotk::Optimizer]
#' highly flexible and configurable. In combination with [`OperatorCombination`] mutators and recombinators, an algorithm
#' as presented in `r cite_bib("li2013mixed")` can easily be implemented.
#'
#' `OptimizerMies` implements a standard evolutionary strategies loop:
#' 1. Prime operators, using `mies_prime_operators()`
#' 2. Initialize population, using `mies_init_population()`
#' 3. Generate offspring by selecting parents, recombining and mutating them, using `mies_generate_offspring()`
#' 4. Evaluate performance, using `mies_evaluate_offspring()`
#' 5. Select survivors, using either `mies_survival_plus()` or `mies_survival_comma()`, depending on the `survival_strategy` hyperparameter
#' 6. Optionally, evaluate survivors with higher fidelity if the multi-fidelity functionality is being used
#' 7. Jump to 3.
#'
#' @section Multi-Fidelity:
#' `miesmuschel` provides a simple multi-fidelity optimization mechanism that both allows increasing fidelity by generation
#' number, and
#'
#' @references
#' `r format_bib("fieldsend2014rolling")`
#'
#' `r format_bib("li2013mixed")`
#'
#' @export
OptimizerMies = R6Class("OptimizerMies", inherit = Optimizer,
  public = list(
      initialize = function(mutator = MutatorProxy$new(), recombinator = RecombinatorProxy$new(), parent_selector = SelectorProxy$new(),
        survival_selector = SelectorProxy$new(), elite_selector = NULL, multi_fidelity = FALSE) {
      private$.mutator = assert_r6(mutator, "Mutator")$clone(deep = TRUE)
      private$.recombinator = assert_r6(recombinator, "Recombinator")$clone(deep = TRUE)
      private$.parent_selector = assert_r6(parent_selector, "Selector")$clone(deep = TRUE)
      private$.survival_selector = assert_r6(survival_selector, "Selector")$clone(deep = TRUE)

      assert_r6(elite_selector, "Selector", null.ok = TRUE)
      if (!is.null(elite_selector)) {
        private$.elite_selector = elite_selector$clone(deep = TRUE)
      }
      private$.own_param_set = do.call(ps, c(list(
          lambda = p_int(1, tags = c("required", "offspring")),
          mu = p_int(1, tags = c("required", "init", "survival")),
          initializer = p_uty(custom_check = function(x) check_function(x, nargs = 2), default = generate_design_random, tags = "init"),  # arguments: param_set, n
          survival_strategy = p_fct(c("plus", if (!is.null(elite_selector)) "comma"), tags = "required")
          additional_component_sampler = p_uty(custom_check = function(x) check_r6(x, "Sampler"))),
        if (multi_fidelity) list(
          fidelity_schedule = p_uty(custom_check = check_fidelity_schedule, tags = "required"),
          fidelity_generation_lookahead = p_lgl(tags = "required"),
          fidelity_current_gen_only = p_lgl(tags = "required"),
          fidelity_monotonic = p_lgl(tags = "required"))
        if (!is.null(elite_selector)) list(
          n_elite = p_int(0, depends = survival_strategy == "comma", tags = "survival"))
      ))
      private$.param_set_source = c(alist(
          private$.own_param_set,
          mutator = self$mutator$param_set,
          recombinator = self$recombinator$param_set,
          parent_selector = self$parent_selector$param_set,
          survival_selector = self$survival_selector$param_set),
        if (!is.null(elite_selector)) alist(
          elite_selector = self$elite_selector$param_set)
      )

      self$param_set$values = c(
        list(lambda = 10, mu = 1, survival_strategy = "plus"),
        if (multi_fidelity) list(
          fidelity_schedule = data.table(generation = 1, budget_new = 1, budget_survivors = 1),
          fidelity_generation_lookahead = TRUE, fidelity_current_gen_only = FALSE, fidelity_monotonic = TRUE)
      )

      param_class_determinants = c(
        list(parent_selector, survival_selector),
        if (!is.null(elite_selector)) list(elite_selector),

        # don't depend on mutate and recombine when we do multi-fidelity because budget may be any unsupported Param.
        if (!multi_fidelity) list(mutator, recombinator)
      )

      properties_determinants = discard(list(parent_selector, survival_selector, elite_selector), is.null)

      super$initialize(
        param_set = self$param_set,  # essentially a nop, since at this point we already set private$.param_set, but we can't give NULL here.
        param_classes = Reduce(intersect, map(param_class_determinants, "param_classes")),
        properties = c("dependencies", Reduce(intersect, map(properties_determinants, "supported")))
      )
    }
  ),
  active = list(
    mutator = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.mutator)) {
        stop("mutator is read-only.")
      }
      private$.mutator
    },
    recombinator = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.recombinator)) {
        stop("recombinator is read-only.")
      }
      private$.recombinator
    },
    parent_selector = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.parent_selector)) {
        stop("parent_selector is read-only.")
      }
      private$.parent_selector
    },
    survival_selector = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.survival_selector)) {
        stop("survival_selector is read-only.")
      }
      private$.survival_selector
    },
    elite_selector = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.elite_selector)) {
        stop("elite_selector is read-only.")
      }
      private$.elite_selector
    },
    param_set = function(rhs) {
      if (is.null(private$.param_set)) {
        sourcelist = lapply(private$.param_set_source, function(x) eval(x))
        private$.param_set = ParamSetCollection$new(sourcelist)
        if (!is.null(private$.param_set_id)) private$.param_set$set_id = private$.param_set_id
      }
      if (!missing(val) && !identical(val, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
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
    .optimize = function(inst) {
      params = private$.own_param_set$get_values()

      budget_id = NULL
      if (!is.null(params$fidelity_schedule)) {
        budget_id = search_space$ids(tags = "budget")
        if (length(budget_id) != 1) stopf("Need exactly one budget parameter for multifidelity, but found %s: %s",
          length(budget_id), str_collapse(budget_id))
      }

      mies_prime_operators(mutators = list(self$mutator), recombinators = list(self$recombinator),
        selectors = discard(list(self$survival_selector, self$parent_selector, self$elite_selector), is.null),
        search_space = inst$search_space, additional_components = params$additional_component_sampler$param_set,
        budget_id = budget_id)

      mies_init_population(inst, mu = params$mu, initializer = params$initializer, fidelity_schedule = params$fidelity_schedule,
        budget_id = budget_id, additional_component_sampler = params$additional_component_sampler)

      survival = switch(params$survival_strategy,
          plus = mies_survival_plus,
          comma = mies_survival_comma)

      repeat {
        mies_step_fidelity(inst, params$fidelity_schedule, budget_id, generation_lookahead = params$fidelity_generation_lookahead,
          current_gen_only = params$fidelity_current_gen_only, monotonic = params$fidelity_monotonic)
        offspring = mies_generate_offspring(inst, lambda = params$lambda,
          parent_selector = self$parent_selector, mutator = self$mutator, recombinator = self$recombinator, budget_id = budget_id)
        mies_evaluate_offspring(inst, offspring = offspring, fidelity_schedule = params$fidelity_schedule, budget_id = budget_id)
        survival(inst, mu = params$mu, survival_selector = self$survival_selector, n_elite = params$n_elite, elite_selector = self$elite_selector)
      }
    },
    .mutator = NULL,
    .recombinator = NULL,
    .parent_selector = NULL,
    .survival_selector = NULL,
    .elite_selector = NULL,
    .own_param_set = NULL,
    .param_set_id = NULL,
    .param_set_sources = NULL
  )
)

check_fidelity_schedule = function(x) {
  if (test_data_frame(x, ncols = 2, min.rows = 1) &&
      test_names(colnames(x), identical.to = c("generation", "budget_new", "budget_survivors")) &&
      test_integerish(x$generation, tol = 1e-100, any.missing = FALSE, unique = TRUE) &&
      1 %in% x$generation) {
    TRUE
  } else {
    "must be a data.table with integer column 'generation' (with unique non-missing values and at least one row with value 1) and columns 'budget_new', 'budget_survivors'."
  }
}

#' @title Evaluate Proposed Configurations Generated in a MIES Iteration
#'
#' @description
#' Calls `$eval_batch` of a given [`OptimInstance`][bbotk::OptimInstance] on a set
#' of configurations as part of a MIES operation. The `dob` extra-info in the archive
#' is also set properly to indicate a progressed generation.
#'
#' This function can be used directly, but it is easier to use it within the [`OptimizerMies`]
#' [`Optimizer`][bbotk::Optimizer] if standard GA operation is desired.
#'
#' "Rolling-tide" multifidelity is supported as described in [`OptimizerMies`] or more thoroughly
#' in `vignette("mies-multifid")`. For this,
#' an extra component named after `budget_id` is appended to each individual, chosen from
#' the `fidelity_schedule` depending on the value of `survivor_budget`. Both `fidelity_schedule`
#' and `budget_id` should be the same values as given to the other `mies_*` functions.
#'
#' @template param_inst
#' @param offspring (`data.frame`)\cr
#'   Proposed configurations to be evaluated, must have columns named after the `inst`'s search space, minus `budget_id` if not `NULL`.
#' @template param_fidelity_schedule_maybenull
#' @template param_budget_id_maybenull
#' @param survivor_budget (`logical(1)`)\cr
#'   When doing multi-fidelity optimization, determines which column of `fidelity_schedule` to use to determine the budget component value.
#' @return invible [`data.table`][data.table::data.table]: the performance values returned when evaluating the `offspring` values
#'   through `eval_batch`.
#' @family mies building blocks
#' @export
mies_evaluate_offspring = function(inst, offspring, fidelity_schedule = NULL, budget_id = NULL, survivor_budget = FALSE) {
  assert(check_r6(inst, "OptimInstance"), check_r6(inst, "TuningInstance"))
  offspring = as.data.table(assert_data_frame(offspring))
  ss_ids = inst$search_space$ids()
  assert_choice(budget_id, ss_ids, null.ok = is.null(fidelity_schedule))
  assert_flag(survivor_budget)
  generation = max(inst$archive$data$dob, 0) + 1
  assert_names(colnames(offspring), must.include = setdiff(ss_ids, survivor_budget), disjunct.from = survivor_budget)  # TODO: must not include survivor_budget, but can include other things

  if (!is.null(fidelity_schedule)) {
    assert(check_fidelity_schedule(fidelity_schedule))
    fidelity_schedule = as.data.table(fidelity_schedule)
    fidelity_schedule = setkeyv(copy(fidelity_schedule), "generation")
    fidelity_column = if (survivor_budget) "budget_survivors" else "budget_new"
    fidelity = fidelity_schedule[data.table(generation = generation), fidelity_column, on = generation, roll = TRUE, with = FALSE]
    setnames(fidelity, budget_id)
    offspring = cbind(offspring, fidelity)
  }

  inst$eval_batch(cbind(offspring, dob = generation, eol = NA_real_))
}

#' @title Re-Evaluate Configurations with Higher Fidelity
#'
#' @description
#' As part of the "rolling-tide" multifidelity-setup, do reevaluation of configurations with
#' higher fidelity that have survived lower-fidelity selection.
#'
#' The `fidelity_schedule` specifies what the fidelity (i.e. value of the "budget" component) of evaluations
#' done or compared at generation `i` should be. The `generation_lookahead` parameter has a subtle influence.
#' If it is `TRUE`, then, at the end of generation `i`, reevaluation is done with the `"budget_survivors"`
#' fidelity of generation `i + 1`, otherwise the entry of generation `i` is used. This gives the `"budget_survivors"`
#' column a slightly different meaning in the two cases: either it is the budget, with which survivors "entering"
#' generation `i` have been evaluated, or it is the budget with which survivors "coming out of" that generation
#' were evaluated. The evaluations are done as part of the *current* generation in any case (see examples).
#'
#' This function should only be called when doing rolling-tide multifidelity, and should not be part of the
#' MIES cycle otherwise.
#'
#' @template param_inst
#' @param fidelity_schedule (`data.frame`)\cr
#'   `data.frame` with three columns `"generation"`, `"budget_new"`, `"budget_survivors"`, in that order. `"budget_new"` and `"budget_survivors"`
#'   are atomic columns. The value of `"budget_survivors"` of the appropriate row is assigned to the `budget_id` component of `offspring`.
#'   `"generation"` is an integer valued column, indicating the first generation at which a row is valid. At least one row with
#'   `generation == 1` must be present.
#' @param budget_id (`character(1)`)\cr
#'   Budget component that is set to the value found in `fidelity_schedule`.
#' @param generation_lookahead (`logical(1)`)\cr
#'   Whether to re-evaluate alive individuals with the `"budget_survivors"` entry of the current generation (`FALSE`), or
#'   of the next generation (`TRUE`).
#' @param current_gen_only (`logical(1)`)\cr
#'   Whether to only re-evaluate survivors individuals generated in the latest generation (`TRUE`), or re-evaluate all currently alive
#'   individuals (`FALSE`). In any case, only individuals that were not already evaluated with the chosen fidelity are evaluated,
#'   so this will usually only affect individuals from before the latest generation if the `"budget_survivors"` changed between
#'   generations (i.e. when a different row from `fidelity_schedule` is used).
#' @param monotonic (`logical(1)`)\cr
#'   Whether to only re-evaluate configurations for which the fidelity would increase. Default `TRUE`.
#' @return [invible] [`data.table`][data.table::data.table]: the performance values returned when evaluating the `offspring` values
#'   through `eval_batch`.
#' @family mies building blocks
#' @export
mies_step_fidelity = function(inst, fidelity_schedule, budget_id, generation_lookahead = TRUE, current_gen_only = FALSE, monotonic = TRUE) {
  assert(check_r6(inst, "OptimInstance"), check_r6(inst, "TuningInstance"))
  assert_flag(update_all_alive_fidelity)
  data = inst$archive$data
  generation = max(data$dob, 0)
  assert_choice(budget_id, inst$search_space$ids())
  assert(check_fidelity_schedule(fidelity_schedule))
  fidelity_schedule = as.data.table(fidelity_schedule)

  if (!any(is.na(data$eol))) stop("No alive individuals. Need to run mies_init_population()?")

  assert_integerish(data$dob, lower = 0, upper = inst$archive$n_batch, any.missing = FALSE, tol = 1e-100)
  assert_integerish(data$eol, lower = 0, upper = inst$archive$n_batch, tol = 1e-100)

  next_fidelity = fidelity_schedule[data.table(generation = generation + generation_lookahead), "budget_survivors", on = generation, roll = TRUE, with = FALSE]

  comparator = if (monotonic) `<` else `!=`
  reeval = which((!current_gen_only | data$dob == generation) & is.na(data$eol) & comparator(data[[budget_id]], next_fidelity))

  data[reeval, eol := ..(generation)]
  indivs = data[reeval, inst$search_space$ids(), with = FALSE]
  indivs[, `:=`(budget_id, next_fidelity)]

  # I hate this... but there seems to be no way to avoid the TerminatorGenerations from terminating here :-(
  # What happens here is that we decrease dob by 1 everywhere, evaluate with current-generation-minus-one,
  # and then increase dob by 1 again. This avoids TerminatorGenerations from firing because if it *did*
  # mind about dob minus 1, then it would have fired already.
  # Weird edge-case: A TerminatorGenerations with `generations` set to 0 will not prevent reevals of initial
  # pop if the initial pop is entirely made up of preexisting values.
  inst$archive$data[, dob := dob - 1]
  # stupid race condition. Hope we don't get trapped here...
  on.exit(inst$archive$data[, dob := dob + 1])

  inst$eval_batch(indivs[, `:=`(dob = ..(generation - 1), eol = NA_real_)])
}

#' @title Choose Survivors According to the "Plus" Strategy
#'
#' @description
#' Choose survivors during a MIES iteration using the "Plus" survival strategy, i.e.
#' combining all alive individuals from the latest and from prior generations indiscriminately and
#' choosing survivors using a survival [`Selector`] operator.
#'
#' @template param_inst
#' @template param_mu
#' @template param_survival_survival_selector
#' @template param_survival_dotdotdot
#' @template param_survival_return
#' @family mies building blocks
#' @export
mies_survival_plus = function(inst, mu, survival_selector, ...) {
  assert(check_r6(inst, "OptimInstance"), check_r6(inst, "TuningInstance"))
  assert_int(mu, lower = 1, tol = 1e-100)
  data = inst$archive$data

  alive = which(is.na(data$eol))
  if (!length(alive)) stop("No alive individuals. Need to run mies_init_population()?")

  assert_integerish(data$dob, lower = 0, upper = inst$archive$n_batch, any.missing = FALSE, tol = 1e-100)
  assert_integerish(data$eol, lower = 0, upper = inst$archive$n_batch, tol = 1e-100)

  survivors = mies_select_from_archive(inst, mu, alive, survival_selector, get_indivs = FALSE)
  if (anyDuplicated(survivors)) stop("survival_selector may not generate duplicates.")
  died = setdiff(alive, survivors)
  data[died, eol := max(dob)]
}

#' @title Choose Survivors According to the "Comma" Strategy
#'
#' @description
#' Choose survivors during a MIES iteration using the "Comma" survival strategy, i.e.
#' selecting survivors from the latest generation only, using a [`Selector`] operator, and choosing
#' "elites" from survivors from previous generations using a different [`Selector`] operator.
#'
#' @template param_inst
#' @template param_mu
#' @template param_survival_survival_selector
#' @param n_elite (`integer(1)`)\cr
#'   Number of individuals to carry over from previous generations. `n_elite` individuals will be selected
#'   by `elite_selector`, while `mu - n_elite` will be selected by `survival_selector` from the most
#'   recent generation. `n_elite` may be 0 (no elitism), in which case only individuals from the newest
#'   generation survive. `n_elite` must be strictly smaller than `mu` to permit any optimization progress.
#' @param elite_selector ([`Selector`])\cr
#'   [`Selector`] operator that selects "elites", i.e. surviving individuals from previous generations,
#'   depending on configuration values
#'   and objective results. When `elite_selector$operate()` is called, then objectives that
#'   are being minimized are multiplied with -1 (through [`mies_get_fitnesses()`]), since [`Selector`]s always try to maximize fitness.\cr
#'   The [`Selector`] must be primed on `inst$search_space`; this *includes* the "budget" component
#'   when performing multi-fidelity optimization.\cr
#'   The given [`Selector`] may *not* return duplicates.
#' @template param_survival_dotdotdot
#' @template param_survival_return
#' @family mies building blocks
#' @export
mies_survival_comma = function(inst, mu, survival_selector, n_elite, elite_selector, ...) {
  assert(check_r6(inst, "OptimInstance"), check_r6(inst, "TuningInstance"))
  assert_int(mu, lower = 1, tol = 1e-100)
  assert_int(n_elite, lower = 0, upper = mu - 1, tol = 1e-100)

  data = inst$archive$data
  assert_integerish(data$dob, lower = 0, upper = inst$archive$n_batch, any.missing = FALSE, tol = 1e-100)
  assert_integerish(data$eol, lower = 0, upper = inst$archive$n_batch, tol = 1e-100)
  alive_before = data[, which(is.na(eol) & dob != max(dob))]
  current_offspring = data[, which(is.na(eol) & dob == max(dob))]

  if (!length(alive_before)) stop("No alive individuals. Need to run mies_init_population()?")
  if (!length(current_offspring)) stop("No current offspring. Need to run mies_evaluate_offspring()?")

  survivors = if (mu > n_elite) mies_select_from_archive(inst, mu - n_elite, current_offspring, survival_selector, get_indivs = FALSE)
  if (anyDuplicated(survivors)) stop("survival_selector may not generate duplicates.")

  elites = mies_select_from_archive(inst, n_elite, alive_before, elite_selector, get_indivs = FALSE),
  if (anyDuplicated(elites)) stop("elite_selector may not generate duplicates.")

  survivors = c(elites, survivors)

  died = setdiff(alive, survivors)
  data[died, eol := max(dob)]
}

#' @title Prime MIES Operators
#'
#' @description
#' Prime the given [`MiesOperator`]s for an optimization run with the given search space.
#'
#' In its simplest form, MIES optimization only optimizes the search space of the [`Objective`][bbotk::Objective] to be optimized. However,
#' more advanced optimization may handle a "budget" parameter for multi-fidelity optimization differently: It is still selected by [`Selector`]s,
#' but not mutated or recombined and instead handled separately. It is also possible to add additional components to the search space that are
#' not evaluated by the objective function, but that are used for self-adaption by other operators.
#'
#' The `mies_prime_operators()` function uses the information that the user usually has readily at hand -- the [`Objective`][bbotk::Objective]`s search space,
#' the budget parameter, and additional components -- and primes [`Mutator`], [`Recombinator`], and [`Selector`] objects in the right way:
#' * [`Selector`]s are primed on a union of `search_space` and `additional_components`
#' * [`Mutator`]s and [`Recombinator`]s are primed on the [`Selector`]'s space with the `budget_id` [`Param`][paradox::Param] removed.
#'
#' `mies_prime_operators()` is called with an arbitrary number of [`MiesOperator`] arguments; typically one [`Mutator`], one [`Recombinator`] and
#' at least two [`Selector`]: one for survival selection, and one parent selection. Supplied [`MiesOperator`]s are primed by-reference, but
#' they are also returned as [invisible] `list`.
#'
#' If neither additional components nor multi-fidelity optimization is used, it is also possible to use the `$prime()` function of hte [`MiesOperator`]s
#' directly, although using `mies_prime_operators()` gives flexibility for future extension.
#' @param mutators (`list` of [`Mutator`])\cr
#'   [`Mutator`] objects to prime. May be empty (default).
#' @param recombinators (`list` of [`Recombinator`])\cr
#'   [`Recombinator`] objects to prime. May be empty (default).
#' @param selectors (`list` of [`Selector`])\cr
#'   [`Selector`] objects to prime. May be empty (default).
#' @param search_space ([`ParamSet`][paradox::ParamSet])\cr
#'   Search space of the [`Objective`][bbotk::Objective] or [`OptimInstance`][bbotk::OptimInstance] to be optimized.
#' @param additional_components ([`ParamSet`][paradox::ParamSet] | `NULL`)\cr
#'   Additional components to optimize over, not included in `search_space`, but possibly used for self-adaption. This must be the [`ParamSet`][paradox::ParamSet]
#'   of `mies_init_population()`'s `additional_component_sampler` argument.
#' @param budget_id (`character(1)` | `NULL`)\cr
#'   Budget component used for multi-fidelity optimization.
#' @return `invisible` named `list` with entries `$mutators` (`list` of [`Mutator`], primed `mutators`), `$recombinators` (`list` of [`Recombinator`], primed `recombinators`),
#'   and `$selectors` (`list` of [`Selector`], primed `selectors`).
mies_prime_operators = function(mutators = list(), recombinators = list(), selectors = list(), search_space, additional_components = NULL, budget_id = NULL) {
  assert_list(mutators, types = "Mutator", any.missing = FALSE)
  assert_list(recombinators, types = "Recombinator", any.missing = FALSE)
  assert_list(selectors, types = "Selector", any.missing = FALSE)
  assert_r6(search_space, "ParamSet")
  assert_r6(additional_components, "ParamSet", null.ok = TRUE)
  assert_choice(budget_id, search_space$ids(), null.ok = TRUE)

  if (is.null(additional_components)) {
    full_search_space = search_space
  } else {
    search_space = search_space$clone(deep = FALSE)
    additional_components = additional_components$clone(deep = FALSE)
    search_space$set_id = ""
    additional_components$set_id = ""
    full_search_space = ps_union(list(search_space, additional_components))
  }

  # selectors are primed with entire searchspace
  selectors = lapply(selectors, function(x) x$prime(full_search_space))

  nobudget_search_space = ParamSetShadow$new(full_search_space, budget_id)

  # mutators, recombinators are primed with search space minus budget_id
  mutators = lapply(mutators, function(x) x$prime(nobudget_search_space))
  recombinators = lapply(recombinators, function(x) x$prime(nobudget_search_space))

  invisible(list(mutators = mutaotrs, recombinators = recombinators, selectors = selectors))
}

#' @title Initialize MIES Optimization
#'
#' @description
#' Set up an [`OptimInstance`][bbotk::OptimInstance] (or [`TuningInstance`][mlr3tuning::TuningInstance]) for MIES optimization.
#' This adds the `dob` and `eol` columns to the instance's archive, and makes sure there are at least `mu` survivors
#' (i.e. entries with `eol` set to `NA`) present. If there are already `>= mu` prior evaluations present, then the last
#' `mu` of these remain alive (the other's `eol` set to 0); otherwise, up to `mu` new randomly sampled configurations
#' are evaluated and added to the archive and have `eol` set to `NA`.
#'
#' @template param_inst
#' @template param_mu
#' @param initializer (`function`)\cr
#'   Function that generates a [`Design`][paradox::Design] object, with arguments `param_set` and `n`, unctioning like [`paradox::generate_design_random`]
#'   or [`paradox::generate_design_lhs`]. Note that [`paradox::generate_design_grid`] can not be used and must be wrapped with
#'   a custom function that ensures that only `n` individuals are produced.
#' @template param_fidelity_schedule_maybenull
#' @template param_budget_id_maybenull
#' @param additional_component_sampler ([`Sampler`][paradox::Sampler] | `NULL`)\cr
#'   [`Sampler`][paradox::Sampler] for components of individuals that are not part of `inst`'s `$search_space`. These components
#'   are never used for performance evaluation, but they may be useful for self-adaptive [`OperatorCombination`]s. See the description
#'   of [`mies_prime_operators()`] on how operators need to be primed to respect additional components.\cr
#'   It is possible that `additional_component_sampler` is used for *more* rows than `initializer`, which happens
#'   when the `inst`'s `$archive` contains prior evaluations that are alive, but does not contain columns pertaining to additional columns,
#'   or contains *all* these columns but there are rows that are `NA` valued. If only *some* of the columns are present, or if all these columns
#'   are present but there are rows that are only `NA` valued for some columns, then an error is thrown.\cr
#'   Default is `NULL`: no additional components.
#' @return [invisible] [`OptimInstance`][bbotk::OptimInstance] | [invisible] [`TuningInstance`][mlr3tuning::TuningInstance]: the input
#'   instance, modified by-reference.
#'
#' @family mies building blocks
#' @export
mies_init_population = function(inst, mu, initializer = generate_design_random, fidelity_schedule = NULL, budget_id = NULL, additional_component_sampler = NULL) {
  assert(check_r6(inst, "OptimInstance"), check_r6(inst, "TuningInstance"))

  assert_int(mu, lower = 1, tol = 1e-100)
  assert_function(initializer, nargs = 2)

  ss_ids = inst$search_space$ids()
  ac_ids = additional_component_sampler$param_set$ids()
  present_cols = colnames(inst$archive$data)

  assert_choice(budget_id, ss_ids, null.ok = is.null(fidelity_schedule))
  if (!is.null(fidelity_schedule)) {
    assert(check_fidelity_schedule(fidelity_schedule))
  }

  if (any(c("dob", "eol") %in% ss_ids)) {
    stop("'dob' and 'eol' may not be search space dimensions.")
  }
  if (any(c("dob", "eol") %in% ac_ids)) {
    stop("'dob' and 'eol' may not be additional component dimensions.")
  }
  if (any(ss_ids %in% ac_ids)) {
    stopf("Search space and additional components name clash: %s", str_collapse(intersect(ss_ids, ac_ids)))
  }
  if (any(ac_ids %in% present_cols) && !all(ac_ids %in% present_cols)) {
    stopf("Some, but not all, additoinal components already in archive: %s", str_collapse(intersect(present_cols, ac_ids)))
  }
  dob = batch_nr = NULL
  if ("eol" %nin% present_cols) {
    inst$archive$data[, eol := NA_real_]
  } else if ("dob" %nin% present_cols) {
    stop("'eol' but not 'dob' column found in archive; this is an undefined state that would probably lead to bad behaviour, so stopping.")
  }

  if ("dob" %nin% present_cols) {
    inst$archive$data[, dob := 0]
  }
  data = inst$archive$data  # adding columns is not guaranteed to happen by reference, so we need to be careful with copies of data!
  assert_integerish(data$dob, lower = 0, upper = inst$archive$n_batch, any.missing = FALSE, tol = 1e-100)
  assert_integerish(data$eol, lower = 0, upper = inst$archive$n_batch, tol = 1e-100)

  alive = which(is.na(inst$archive$eol))
  n_alive = length(alive)
  mu_remaining = mu - n_alive

  # take care of additional components
  # we need to check if / how many additional components we need to sample. If there are no alive individuals,
  # then this is just mu. If there are alive individuals, it *may* also be mu (we will have to insert components
  # at those rows where there are alive individuals!)
  additional_needed = mu  # how many more rows of additional components need to be sampled. by default: mu
  row_insert = last(alive, mu)  # at what rows of `data` the components are inserted
  if (n_alive && all(ac_ids %in% present_cols)) {
    # additional components already present in the archive. Let's check if there are any rows with NAs.
    missing_info = is.na(data[row_insert, ac_ids, with = FALSE])
    any_missing_ac = apply(missing_info, 1, all)
    if (any(apply(missing_info, 1, any) != any_missing_ac)) {
      stop("There are rows in inst$archive$data where some, but not all additional component values are NA.")
    }
    row_insert = row_insert[any_missing_ac]  # only insert at the rows where any/all values are missing
    additional_needed = max(0, mu - n_alive) + length(row_insert)  # need to generate components for the missing rows, and for the values that get sampled new
  }

  additional_components = assert_data_frame(additional_component_sampler$sample(additional_needed)$data, nrows = additional_needed)

  # assert here that:
  # we either have some individuals that need to be sampled (mu_remaining > 0)
  #   in which case the additional_components table's first `length(row_insert)` and last `mu_remaining` rows are complementary
  # or that we have no more individuals to sample, in which case `additional_components` are exactly equal to the number of rows where things get inserted.
  assert_true((mu_remaining > 0 && length(row_insert) + mu_remaining == additional_needed) || (mu_remaining <= 0 && length(row_insert) == additional_needed))

  # if we don't insert anything, then this operation just adds the necessary columns in case they are missing, and does nothing otherwise
  inst$archive$data[row_insert, ac_ids] <- first(additional_components, length(row_insert))

  if (mu_remaining < 0) {
    # TODO: we are currently killing the earliest evals, maybe do something smarter.
    # TODO: also we are not doing anything about budget here, we just hope the user knows what he's doing.
    # TODO: also also the additional component handling above depends on killing earliest now.
    inst$archive$data[first(which(is.na(eol)), -mu_remaining), eol := max(dob)]
  } else if (mu_remaining > 0) {
    mies_evaluate_offspring(inst,
      cbind(remove_named(assert_data_frame(initializer(inst$search_space, mu_remaining)$data, nrows = mu_remaining), budget_id), last(additional_components, mu_remaining))
      fidelity_schedule, budget_id, survivor_budget = TRUE)
  }
  invisible(inst)
}

#' @title Get Fitness Values from OptimInstance
#'
#' @description
#' Get fitness values in the correct form as used by [`Selector`] operators from an
#' [`OptimInstance`][bbotk::OptimInstance] (or [`TuningInstance`][mlr3tuning::TuningInstance]).
#' This works for both single-criterion and multi-criterion optimization, and entails multiplying
#' objectives with -1 if they are being minimized, since [`Selector`] tries to maximize fitness.
#'
#' @template param_inst
#' @template param_rows
#' @return `numeric` `matrix` with `length(rows)` (if `rows` is given, otherwise `nrow(inst$archive$data)`) rows
#' and one column for each objective: fitnesses to be maximized.
#' @family mies building blocks
#' @export
mies_get_fitnesses = function(inst, rows) {
  assert(check_r6(inst, "OptimInstance"), check_r6(inst, "TuningInstance"))

  multiplier = map_dbl(inst$archive$codomain$tags, function(x) switch(x, minimize = -1, maximize = 1, 0))
  fitnesses = as.matrix(inst$archive$data[rows, ..(multiplier) != 0, with = FALSE])
  multiplier = multiplier[multiplier != 0]
  sweep(fitnesses, 2L, multiplier, `*`)
}

#' @title Select Individuals from an OptimInstance
#'
#' @description
#' Apply a [`Selector`] operator to a subset of configurations inside
#' an [`OptimInstance`][bbotk::OptimInstance] (or [`TuningInstance`][mlr3tuning::TuningInstance])
#' and return the index within the archive (when `get_indivs` `FALSE`) or the configurations themselves
#' (when `get_indivs` is `TRUE`).
#'
#' It is not strictly necessary for the selector to select unique individuals / individuals without replacement.
#'
#' @template param_inst
#' @param n_select (`integer(1)`)\cr
#'   Number of individuals to select.
#' @template param_rows
#' @param `selector` ([`Selector`])\cr
#'   [`Selector`] operator that selects individuals depending on configuration values
#'   and objective results. When `selector$operate()` is called, then objectives that
#'   are being minimized are multiplied with -1 (through [`mies_get_fitnesses()`]), since [`Selector`]s always try to maximize fitness.
#'   Defaults to [`SelectorBest`].\cr
#'   The [`Selector`] must be primed on a superset of `inst$search_space`; this *includes* the "budget" component
#'   when performing multi-fidelity optimization. All components on which `selector` is primed on must occur in the archive.\cr
#'   The given [`Selector`] *may* return duplicates.
#' @param get_indivs (`logical(1)`)\cr
#'   Whether to return configuration values from within the archive (`TRUE`) or just the indices within
#'   the archive (`FALSE`). Default is `TRUE`.
#' @return `integer` | [`data.table`][data.table::data.table]: Selected individuals, either index into `inst` or subset of archive table,
#'   depending on `get_indivs`.
#' @family mies building blocks
#' @export
mies_select_from_archive = function(inst, n_select, rows, selector = SelectorBest$new()$prime(inst$search_space), get_indivs = TRUE) {
  assert(check_r6(inst, "OptimInstance"), check_r6(inst, "TuningInstance"))

  assert_r6(selector, "Selector")
  assert_true(selector$is_primed)
  assert_int(n_select, lower = 0, tol = 1e-100)

  selector_params <- selector$primed_ps$ids()
  assert_subset(inst$search_space$ids(), selector_params)
  assert_subset(selector_params, colnames(inst$data))

  if (n_select == 0) if (get_indivs) return(inst$data[0, selector_params, with = FALSE]) else return(integer(0))
  indivs = inst$archive$data[rows, selector_params, with = FALSE]

  fitnesses = mies_get_fitnesses(inst, rows)

  selected = selector$operate(indivs, fitnesses, n_select)
  if (get_indivs) {
    indivs[selected]
  } else {
    if (missing(rows)) selected else rows[selected]
  }
}

#' @title Generate Offspring Through Mutation and Recombination
#'
#' @description
#' Generate new proposal individuals to be evaluated using [`mies_evaluate_offspring()`].
#'
#' Parent individuals are selected using `parent_selector`, then mutated using `mutator`, and thend
#' recombined using `recombinator`. If only a subset of these operations is desired, then
#' it is possible to set `mutator` or `recombinator` to the respective "null"-operators.
#'
#' @template param_inst
#' @param lambda (`integer(1)`)\cr
#'   Number of new individuals to generate. This is not necessarily the number with which `parent_selector`
#'   gets called, because `recombinator` could in principle need more than `lambda` input individuals to
#'   generate `lambda` output individuals.
#' @param parent_selector ([`Selector`])\cr
#'   [`Selector`] operator that selects parent individuals depending on configuration values
#'   and objective results. When `parent_selector$operate()` is called, then objectives that
#'   are being minimized are multiplied with -1 (through [`mies_get_fitnesses()`]), since [`Selector`]s always try to maximize fitness.
#'   Defaults to [`SelectorBest`].\cr
#'   The [`Selector`] must be primed on a superset of `inst$search_space`; this *includes* the "budget" component
#'   when performing multi-fidelity optimization. All components on which `selector` is primed on must occur in the archive.\cr
#'   The given [`Selector`] *may* return duplicates.
#' @param mutator ([`Mutator` | `NULL`])\cr
#'   [`Mutator`] operation to apply to individuals selected out of `inst` using `parent_selector`.\cr
#'   The [`Mutator`] must be primed on a [`ParamSet`][paradox::ParamSet] similar to `inst$search_space`,
#'   but *without* the "budget" component when `budget_id` is given (multi-fidelity optimization). Such a
#'   [`ParamSet`][paradox::ParamSet] can be generated for example using [`mies_prime_operators`].\cr
#'   When this is `NULL` (default), then a [`MutatorNull`] is used, effectively disabling mutation.
#' @param recombinator ([`Recombinator` | `NULL`])\cr
#'   [`Recombinator`] operation to apply to individuals selected out of `int` using `parent_selector` after mutation using `mutator`.
#'   The [`Recombinator`] must be primed on a [`ParamSet`][paradox::ParamSet] similar to `inst$search_space`,
#'   but *without* the "budget" component when `budget_id` is given (multi-fidelity optimization). Such a
#'   [`ParamSet`][paradox::ParamSet] can be generated for example using [`mies_prime_operators`].\cr
#'   When this is `NULL` (default), then a [`RecombinatorNull`] is used, effectively disabling recombination.
#' @param budget_id (`character(1)` | `NULL`)\cr
#'   Budget compnent when doing multi-fidelity optimization. This component of the search space is removed from
#'   individuals sampled from the archive in `inst` before giving it to `mutator` and `recombinator`.
#'   Should be `NULL` when not doing multi-fidelity.
#' @return [`data.table`][data.table::data.table]: A table of configurations proposed as offspring to be evaluated
#' using [`mies_evaluate_offspring()`].
#' @family mies building blocks
#' @export
mies_generate_offspring = function(inst, lambda, parent_selector = SelectorBest$new()$prime(inst$search_space), mutator = NULL, recombinator = NULL, budget_id = NULL) {
  assert(check_r6(inst, "OptimInstance"), check_r6(inst, "TuningInstance"))

  assert_int(lambda, lower = 1, tol = 1e-100)
  assert_r6(parent_selector, "Selector")
  assert_true(parent_selector$is_primed)
  assert_r6(mutator, "Mutator", null.ok = TRUE)
  assert_true(mutator$is_primed)
  assert_r6(recombinator, "Recombinator", null.ok = TRUE)
  assert_true(recombinator$is_primed)

  ps_ids <- parent_selector$primed_ps$ids()
  ss_ids <- inst$search_space$ids()

  assert_subset(ss_ids, ps_ids)
  assert_subset(ps_ids, colnames(inst$data))

  assert_choice(budget_id, ss_ids, null.ok = TRUE)
  if (is.null(mutator) || is.null(recombinator)) {
    selector_space= parent_selector$primed_ps
    if (!is.null(budget_id)) {
      selector_space = ParamSetShadow$new(selector_space, budget_id)
    }
    mutator = mutator %??% MutatorNull$new()$prime(selector_space)
    recombinator = recombinator %??% RecombinatorNull$new()$prime(selector_space)
  }

  assert_set_equal(mutator$primed_ps$ids(), setdiff(ss_ids, budget_id))
  assert_set_equal(recombinator$primed_ps$ids(), setdiff(ss_ids, budget_id))

  needed_recombinations = ceiling(lambda / recombinator$n_indivs_out)
  needed_parents = needed_recombinations * recombinator$n_indivs_in

  parents = mies_select_from_archive(inst, needed_parents, rows, parent_selector)
  if (!is.null(budget_id)) parents[, (budget_id) := NULL]

  recombined = recombinator$operate(parents[sample.int(nrow(parents))])
  recombined = first(recombined, lambda)  # throw away things if we have too many (happens when n_indivs_out is not a divider of lambda)

  mutator$operate(recombined)
}

