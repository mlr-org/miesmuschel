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
#' Multifidelity evaluation is supported as described in `vignette("mies-multifid")`. For this,
#' an extra component named after `budget_id` is appended to each individual, chosen from
#' the `fidelity` argument and depending on the value of `survivor_budget`. `budget_id` should
#' have the same values as given to the other `mies_*` functions.
#'
#' @template param_inst
#' @param offspring (`data.frame`)\cr
#'   Proposed configurations to be evaluated, must have columns named after the `inst`'s search space, minus `budget_id` if not `NULL`.
#' @template param_budget_id_maybenull
#' @template param_fidelity_maybenull
#' @param reevaluate_fidelity (`atomic(1)`)\cr
#'   Fidelity with which to evaluate alive individuals from previous generations that have a budget value below (if `fidelity_monotonic` is `TRUE`) or
#'   different from the current `fidelity` value. Default `NULL`: Do not re-evaluate. Must be `NULL` when `budget_id` and `fidelity` are `NULL`.
#'   See also [`mies_step_fidelity`].
#' @param fidelity_monotonic (`logical(1)`)\cr
#'   When `reevaluate_fidelity` is non-`NULL`, then this indicates whether individuals should only ever be re-evaluated when fidelity would be increased.
#'   Default `TRUE`. Ignored when `reevaluate_fidelity` is `NULL`
#' @return [invisible] [`data.table`][data.table::data.table]: the performance values returned when evaluating the `offspring` values
#'   through `eval_batch`.
#' @family mies building blocks
#' @examples
#' library("bbotk")
#' lgr::threshold("warn")
#'
#' # Define the objective to optimize
#' objective <- ObjectiveRFun$new(
#'   fun = function(xs) {
#'     z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
#'     list(Obj = z)
#'   },
#'   domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4)),
#'   codomain = ps(Obj = p_dbl(tags = "maximize"))
#' )
#'
#' # Get a new OptimInstance
#' oi <- OptimInstanceSingleCrit$new(objective,
#'   terminator = trm("evals", n_evals = 100)
#' )
#'
#' mies_init_population(inst = oi, mu = 3)
#' # Initial state:
#' oi$archive
#'
#' # 'offspring' is just a data.frame of values to evaluate.
#' # In general it should be created using 'mies_generate_offspring()'.
#' offspring = data.frame(x = 1:2, y = 2:1)
#'
#' mies_evaluate_offspring(oi, offspring = offspring)
#'
#' # This evaluated the given points and assigned them 'dob' 2.
#' oi$archive
#'
#' # Note that at this point one would ordinarily call a 'mies_survival_*()'
#' # function.
#'
#' ###
#' # Advanced demo, making use of additional components and doing multi-fidelity
#' ##
#'
#' # declare 'y' the budget parameter. It will not be in the 'offspring'
#' # table any more.
#' budget_id = "y"
#' # but: offspring may contain any other value that is appended to 'oi'. These
#' # are ignored by the objective.
#' offspring = data.frame(x = 0:1, z = 3)
#'
#' mies_evaluate_offspring(oi, offspring = offspring, budget_id = budget_id,
#'   fidelity = 1)
#'
#' # This now has the additional column 'z'. Values of y for the new evaluations
#' # are 1.
#' oi$archive
#'
#' offspring = data.frame(x = 2, z = 3)
#' # Increasing the fidelity will not cause re-evaluation of existing individuals
#' # when `reevaluate_fidelity` is not given.
#' mies_evaluate_offspring(oi, offspring = offspring, budget_id = budget_id,
#'   fidelity = 2)
#' oi$archive
#'
#' offspring = data.frame(x = 3, z = 3)
#' # Depending on the effect of fidelity, this may however have a biasing effect,
#' # so it may be desirable to re-evaluate surviving individuals from previous
#' # generations. The 'reevaluate_fidelity' may even be different from 'fidelity'
#' mies_evaluate_offspring(oi, offspring = offspring, budget_id = budget_id,
#'   fidelity = 3, reevaluate_fidelity = 2)
#'
#' # In this example, only individuals with 'y = 1' were re-evaluated, since
#' # 'fidelity_monotonic' is TRUE.
#' oi$archive
#'
#' @export
mies_evaluate_offspring = function(inst, offspring, budget_id = NULL, fidelity = NULL, reevaluate_fidelity = NULL, fidelity_monotonic = TRUE) {
  assert_optim_instance(inst)

  offspring = as.data.table(assert_data_frame(offspring))
  ss_ids = inst$search_space$ids()
  assert_choice(budget_id, ss_ids, null.ok = is.null(fidelity))
  if (!is.null(reevaluate_fidelity) && is.null(budget_id)) stop("reevaluate_fidelity must be NULL when budget_id is not given.")
  assert_flag(fidelity_monotonic)
  data = inst$archive$data
  ocols = colnames(offspring)
  assert_names(ocols, must.include = setdiff(ss_ids, budget_id), disjunct.from = budget_id)

  reeval = integer(0)  # individuals to be killed; only becomes relevant when `reevaluate_fidelity` is given

  x_id = max(inst$archive$data$x_id, 0, na.rm = TRUE) + seq_len(nrow(offspring))

  if (!is.null(budget_id)) {
    assert_scalar(fidelity)

    fidelity = setnames(data.table(fidelity), budget_id)

    offspring = cbind(offspring, fidelity[nrow(offspring) != 0])  # the conditional is to handle the edge-case for empty offspring tables

    if (!is.null(reevaluate_fidelity) && any(is.na(data$eol))) {
      assert_integerish(data$dob, lower = 0, any.missing = FALSE, tol = 1e-100)
      assert_integerish(data$eol, lower = 0, tol = 1e-100)
      assert_integerish(data$x_id, lower = 0, tol = 1e-100)

      comparator = if (fidelity_monotonic) `<` else `!=`
      reeval = which(is.na(data$eol) & comparator(data[[budget_id]], reevaluate_fidelity))
      indivs = data[reeval, ocols, with = FALSE]
      offspring = rbind(offspring, set(indivs, , budget_id, reevaluate_fidelity))
      x_id = c(x_id, data$x_id[reeval])
    }
  }

  current_gen = max(data$dob, 0, na.rm = TRUE) + 1
  ret = eval_batch_handle_zero(inst, cbind(offspring, data.table(dob = current_gen, eol = NA_real_, x_id = x_id)[nrow(offspring) != 0]))

  set(inst$archive$data, reeval, "eol", current_gen)  # kill old individuals that were fidelity-reevaluated

  invisible(ret)
}

#' @title Re-Evaluate Existing Configurations with Higher Fidelity
#'
#' @description
#' As part of the "rolling-tide" multifidelity-setup, do reevaluation of configurations with
#' higher fidelity that have survived lower-fidelity selection. The evaluations are done as part of the
#' *current* generation, so the `dob` value is not increased.
#'
#' This function should only be called when doing rolling-tide multifidelity, and should not be part of the
#' MIES cycle otherwise.
#'
#' @template param_inst
#' @param budget_id (`character(1)`)\cr
#'   Budget component that is set to the `fidelity` value.
#' @param fidelity (`atomic(1)`)\cr
#'   Atomic scalar indicating the value to be assigned to the `budget_id` component of offspring.
#' @param current_gen_only (`logical(1)`)\cr
#'   Whether to only re-evaluate survivors individuals generated in the latest generation (`TRUE`), or re-evaluate all currently alive
#'   individuals (`FALSE`). In any case, only individuals that were not already evaluated with the chosen fidelity are evaluated,
#'   so this will usually only have an effect when the fidelity of surviving individuals changed between generations.
#' @template param_fidelity_monotonic
#' @template param_additional_components
#' @return [invisible] [`data.table`][data.table::data.table]: the performance values returned when evaluating the `offspring` values
#'   through `eval_batch`.
#' @family mies building blocks
#' @examples
#' library("bbotk")
#' lgr::threshold("warn")
#'
#' # Define the objective to optimize
#' objective <- ObjectiveRFun$new(
#'   fun = function(xs) {
#'     z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
#'     list(Obj = z)
#'   },
#'   domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4)),
#'   codomain = ps(Obj = p_dbl(tags = "maximize"))
#' )
#'
#' # Get a new OptimInstance
#' oi <- OptimInstanceSingleCrit$new(objective,
#'   terminator = trm("evals", n_evals = 100)
#' )
#'
#' budget_id = "y"
#'
#' # Create an initial population with fidelity ("y") value 1
#' mies_init_population(oi, mu = 2, budget_id = budget_id, fidelity = 1)
#'
#' oi$archive
#'
#' # Re-evaluate these individuals with higher fidelity
#' mies_step_fidelity(oi, budget_id = budget_id, fidelity = 2)
#'
#' oi$archive
#'
#' # The following creates a new generation without killing the initial
#' # generation
#' offspring = data.frame(x = 0:1)
#' mies_evaluate_offspring(oi, offspring = offspring, budget_id = budget_id,
#'   fidelity = 3)
#'
#' oi$archive
#'
#' # Re-evaluate only individuals from last generation by setting current_gen_only
#' mies_step_fidelity(oi, budget_id = budget_id, fidelity = 4,
#'   current_gen_only = TRUE)
#'
#' oi$archive
#'
#' # Default: Re-evaluate all that *increase* fidelity: Only the initial
#' # population is re-evaluated here.
#' mies_step_fidelity(oi, budget_id = budget_id, fidelity = 3)
#'
#' oi$archive
#'
#' # To also re-evaluate individuals with *higher* fidelity, use
#' # 'fidelity_monotonic = FALSE'. This does not re-evaluate the points that already have
#' # the requested fidelity, however.
#' mies_step_fidelity(oi, budget_id = budget_id, fidelity = 3, fidelity_monotonic = FALSE)
#'
#' oi$archive
#' @export
mies_step_fidelity = function(inst, budget_id, fidelity, current_gen_only = FALSE, fidelity_monotonic = TRUE, additional_components = NULL) {
  assert_optim_instance(inst)
  assert_flag(current_gen_only)
  assert_flag(fidelity_monotonic)
  assert_r6(additional_components, "ParamSet", null.ok = TRUE)
  ss_ids = inst$search_space$ids()
  ac_ids = character(0)
  if (!is.null(additional_components)) {
    ac_ids = additional_components$ids()
    assert_names(ac_ids, disjunct.from = reserved_component_names, .var.name = "IDs of additional_components")
    assert_names(ac_ids, disjunct.from = ss_ids, .var.name = "IDs of additional_components")
  }
  data = inst$archive$data
  current_gen = max(data$dob, 0)
  assert_choice(budget_id, ss_ids)
  assert_scalar(fidelity)

  if (!any(is.na(data$eol))) stop("No alive individuals. Need to run mies_init_population()?")

  assert_integerish(data$dob, lower = 0, any.missing = FALSE, tol = 1e-100)
  assert_integerish(data$eol, lower = 0, tol = 1e-100)

  comparator = if (fidelity_monotonic) `<` else `!=`
  reeval = which((!current_gen_only | data$dob == current_gen) & is.na(data$eol) & comparator(data[[budget_id]], fidelity))

  indivs = data[reeval, c(ss_ids, ac_ids, "x_id"), with = FALSE]
  set(indivs, , budget_id, fidelity)

  # I hate this... but there seems to be no way to avoid the TerminatorGenerations from terminating here :-(
  # What happens here is that we decrease dob by 1 for the entries of the current generation, evaluate with
  # current-generation-minus-one, and then increase dob by 1 again. This avoids TerminatorGenerations from
  # firing because if it *did* mind about dob minus 1, then it would have fired already.
  # Weird edge-case: A TerminatorGenerations with `generations` set to 0 will not prevent reevals of initial
  # pop if the initial pop is entirely made up of preexisting values.
  current_gen_rows = which(inst$archive$data$dob == current_gen)
  on.exit(set(inst$archive$data, current_gen_rows, "dob", current_gen))  # first sset the on.exit to prevent leaving data in a bad state.
  set(inst$archive$data, current_gen_rows, "dob", current_gen - 1)

  ret = eval_batch_handle_zero(inst, set(indivs, , c("dob", "eol"), list(current_gen, NA_real_)))

  set(inst$archive$data, reeval, "eol", current_gen)  # do this once we are done evaluating replacements, e.g. in case the terminator triggers

  invisible(ret)
}

#' @title Choose Survivors According to the "Mu + Lambda" ("Plus") Strategy
#'
#' @description
#' Choose survivors during a MIES iteration using the "Plus" survival strategy, i.e.
#' combining all alive individuals from the latest and from prior generations indiscriminately and
#' choosing survivors using a survival [`Selector`] operator.
#'
#' When `mu` is greater than the number of alive individuals, then all individuals survive.
#'
#' @template param_inst
#' @template param_mu
#' @template param_survival_survival_selector
#' @template param_survival_dotdotdot
#' @template param_survival_return
#' @family mies building blocks
#' @examples
#' set.seed(1)
#' library("bbotk")
#' lgr::threshold("warn")
#'
#' # Define the objective to optimize
#' objective <- ObjectiveRFun$new(
#'   fun = function(xs) {
#'     z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
#'     list(Obj = z)
#'   },
#'   domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4)),
#'   codomain = ps(Obj = p_dbl(tags = "maximize"))
#' )
#'
#' # Get a new OptimInstance
#' oi <- OptimInstanceSingleCrit$new(objective,
#'   terminator = trm("evals", n_evals = 100)
#' )
#'
#' mies_init_population(inst = oi, mu = 3)
#' offspring = generate_design_random(oi$search_space, 2)$data
#' mies_evaluate_offspring(oi, offspring = offspring)
#'
#' # State before: different generations of individuals. Alive individuals have
#' # 'eol' set to 'NA'.
#' oi$archive
#'
#' s = sel("best")
#' s$prime(oi$search_space)
#' mies_survival_plus(oi, mu = 3, survival_selector = s)
#'
#' # sel("best") lets only the three best individuals survive.
#' # The others have 'eol = 2' (the current generation).
#' oi$archive
#' @export
mies_survival_plus = function(inst, mu, survival_selector, ...) {
  assert_optim_instance(inst)

  assert_int(mu, lower = 0, tol = 1e-100)
  data = inst$archive$data

  alive = which(is.na(data$eol))
  if (!length(alive)) stop("No alive individuals. Need to run mies_init_population()?")

  assert_integerish(data$dob, lower = 0, any.missing = FALSE, tol = 1e-100)
  assert_integerish(data$eol, lower = 0, tol = 1e-100)

  if (length(alive) < mu) {
    survivors = alive
  } else {
    survivors = mies_select_from_archive(inst, mu, alive, survival_selector, get_indivs = FALSE, group_size = mu)
    if (anyDuplicated(survivors)) stop("survival_selector may not generate duplicates.")
  }

  died = setdiff(alive, survivors)
  curgen = data[, max(dob)]
  set(data, died, "eol", curgen)
}

#' @title Choose Survivors According to the "Mu , Lambda" ("Comma") Strategy
#'
#' @description
#' Choose survivors during a MIES iteration using the "Comma" survival strategy, i.e.
#' selecting survivors from the latest generation only, using a [`Selector`] operator, and choosing
#' "elites" from survivors from previous generations using a different [`Selector`] operator.
#'
#' When `n_elite` is greater than the number of alive individuals from previous generations,
#' then all these individuals from previous generations survive. In this case, it is
#' possible that more than `mu - n_elite` individuals from the current generation survive.
#' Similarly, when `mu` is greater
#' than the number of alive individuals from the last generation, then all these individuals survive.
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
#' @examples
#' set.seed(1)
#' library("bbotk")
#' lgr::threshold("warn")
#'
#' # Define the objective to optimize
#' objective <- ObjectiveRFun$new(
#'   fun = function(xs) {
#'     z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
#'     list(Obj = z)
#'   },
#'   domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4)),
#'   codomain = ps(Obj = p_dbl(tags = "maximize"))
#' )
#'
#' # Get a new OptimInstance
#' oi <- OptimInstanceSingleCrit$new(objective,
#'   terminator = trm("evals", n_evals = 100)
#' )
#'
#' mies_init_population(inst = oi, mu = 3)
#' # Usually the offspring is generated using mies_generate_offspring()
#' # Here shorter for demonstration purposes.
#' offspring = generate_design_random(oi$search_space, 3)$data
#' mies_evaluate_offspring(oi, offspring = offspring)
#'
#' # State before: different generations of individuals. Alive individuals have
#' # 'eol' set to 'NA'.
#' oi$archive
#'
#' s = sel("best")
#' s$prime(oi$search_space)
#' mies_survival_comma(oi, mu = 3, survival_selector = s,
#'   n_elite = 2, elite_selector = s)
#'
#' # sel("best") lets only the best individuals survive.
#' # mies_survival_comma selects from new individuals (generation 2 in this case)
#' # and old individuals (all others) separately: n_elite = 2 from old,
#' # mu - n_elite = 1 from new.
#' # The surviving individuals have 'eol' set to 'NA'
#' oi$archive
#' @export
mies_survival_comma = function(inst, mu, survival_selector, n_elite, elite_selector, ...) {
  assert_optim_instance(inst)

  assert_int(mu, lower = 0, tol = 1e-100)
  assert_int(n_elite, lower = 0, upper = mu, tol = 1e-100)

  data = inst$archive$data

  if (!any(is.na(data$eol))) stop("No alive individuals. Need to run mies_init_population()?")

  assert_integerish(data$dob, lower = 0, any.missing = FALSE, tol = 1e-100)
  assert_integerish(data$eol, lower = 0, tol = 1e-100)


  alive_before = data[, which(is.na(eol) & dob != max(dob))]
  current_offspring = data[, which(is.na(eol) & dob == max(dob))]

  if (n_elite > length(alive_before)) {
    elites = alive_before
    n_elite = length(alive_before)
  } else if (n_elite == 0) {
    elites = integer(0)
  } else {
    elites = mies_select_from_archive(inst, n_elite, alive_before, elite_selector, get_indivs = FALSE, group_size = n_elite)
    if (anyDuplicated(elites)) stop("elite_selector may not generate duplicates.")
  }

  if (mu - n_elite > length(current_offspring)) {
    survivors = current_offspring
  } else if (mu - n_elite == 0) {  # don't use survival_selector when not needed.
    survivors = integer(0)
  } else {
    survivors = mies_select_from_archive(inst, mu - n_elite, current_offspring, survival_selector, get_indivs = FALSE, group_size = mu - n_elite)
    if (anyDuplicated(survivors)) stop("survival_selector may not generate duplicates.")
  }

  survivors = c(elites, survivors)

  died = setdiff(c(alive_before, current_offspring), survivors)
  curgen = data[, max(dob)]
  set(data, died, "eol", curgen)
}

#' @title Prime MIES Operators
#'
#' @include ParamSetShadow.R
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
#' @param search_space ([`ParamSet`][paradox::ParamSet])\cr
#'   Search space of the [`Objective`][bbotk::Objective] or [`OptimInstance`][bbotk::OptimInstance] to be optimized.
#' @param mutators (`list` of [`Mutator`])\cr
#'   [`Mutator`] objects to prime. May be empty (default).
#' @param recombinators (`list` of [`Recombinator`])\cr
#'   [`Recombinator`] objects to prime. May be empty (default).
#' @param selectors (`list` of [`Selector`])\cr
#'   [`Selector`] objects to prime. May be empty (default).
#' @param filtors (`list` of [`Filtor`])\cr
#'   [`Filtor`] objects to prime. May be empty (default).
#' @param ... (any)\cr
#'   Must not be given. Other operators may be added in the future, so the following arguments should be passed by name.
#' @template param_additional_components
#' @param budget_id (`character(1)` | `NULL`)\cr
#'   Budget component used for multi-fidelity optimization.
#' @return `invisible` named `list` with entries `$mutators` (`list` of [`Mutator`], primed `mutators`), `$recombinators` (`list` of [`Recombinator`], primed `recombinators`),
#'   and `$selectors` (`list` of [`Selector`], primed `selectors`).
#' @examples
#' # Search space of a potential TuningInstance for optimization:
#' search_space = ps(x = p_dbl(), y = p_dbl())
#' # Additoinal search space components that are not part of the TuningInstance
#' additional_components = ps(z = p_dbl())
#' # Budget parameter not subject to mutation or recombination
#' budget_id = "y"
#'
#' m = mut("gauss")
#' r = rec("xounif")
#' s1 = sel("best")
#' s2 = sel("random")
#' f = ftr("null")
#'
#' mies_prime_operators(search_space, mutators = list(m),
#'   recombinators = list(r), selectors = list(s1, s2), filtors = list(f),
#'   additional_components = additional_components, budget_id = budget_id
#' )
#'
#' # contain search_space without budget parameter, with additional_components
#' m$primed_ps
#' r$primed_ps
#'
#' # contain also the budget parameter
#' s1$primed_ps
#' s2$primed_ps
#' f$primed_ps
#' @export
mies_prime_operators = function(search_space, mutators = list(), recombinators = list(), selectors = list(), filtors = list(), ..., additional_components = NULL, budget_id = NULL) {
  assert_list(mutators, types = "Mutator", any.missing = FALSE)
  assert_list(recombinators, types = "Recombinator", any.missing = FALSE)
  assert_list(selectors, types = "Selector", any.missing = FALSE)
  assert_list(filtors, types = "Filtor", any.missing = FALSE)
  assert_list(list(...), len = 0)
  assert_r6(search_space, "ParamSet")
  assert_r6(additional_components, "ParamSet", null.ok = TRUE)
  assert_choice(budget_id, search_space$ids(), null.ok = TRUE)
  assert_names(search_space$ids(), disjunct.from = reserved_component_names)


  if (is.null(additional_components)) {
    full_search_space = search_space
  } else {
    assert_names(additional_components$ids(), disjunct.from = reserved_component_names)
    search_space = search_space$clone(deep = FALSE)
    additional_components = additional_components$clone(deep = FALSE)
    search_space$set_id = ""
    additional_components$set_id = ""
    full_search_space = ps_union(list(search_space, additional_components))
  }

  # selectors are primed with entire searchspace
  selectors = lapply(selectors, function(x) x$prime(full_search_space))
  filtors = lapply(filtors, function(x) x$prime(full_search_space))

  nobudget_search_space = ParamSetShadow$new(full_search_space, budget_id)

  # mutators, recombinators are primed with search space minus budget_id
  mutators = lapply(mutators, function(x) x$prime(nobudget_search_space))
  recombinators = lapply(recombinators, function(x) x$prime(nobudget_search_space))

  invisible(list(mutators = mutators, recombinators = recombinators, selectors = selectors, filtors = filtors))
}

#' @title Initialize MIES Optimization
#'
#' @description
#' Set up an [`OptimInstance`][bbotk::OptimInstance] for MIES optimization.
#' This adds the `dob` and `eol` columns to the instance's archive, and makes sure there are at least `mu` survivors
#' (i.e. entries with `eol` set to `NA`) present. If there are already `>= mu` prior evaluations present, then the last
#' `mu` of these remain alive (the other's `eol` set to 0); otherwise, up to `mu` new randomly sampled configurations
#' are evaluated and added to the archive and have `eol` set to `NA`.
#'
#' @template param_inst
#' @template param_mu
#' @param initializer (`function`)\cr
#'   Function that generates a [`Design`][paradox::Design] object, with arguments `param_set` and `n`, functioning like [`paradox::generate_design_random`]
#'   or [`paradox::generate_design_lhs`]. Note that [`paradox::generate_design_grid`] can not be used and must be wrapped with
#'   a custom function that ensures that only `n` individuals are produced. The generated design must correspond to the `inst`'s `$search_space`; for
#'   components that are not in the objective's search space, the `additional_component_sampler` is used.
#' @param survival_selector ([`Selector`])\cr
#'   Used when the given [`OptimInstance`][bbotk::OptimInstance] already contains more individuals than `mu`.\cr
#'   [`Selector`] operator that selects surviving individuals depending on configuration values
#'   and objective results,  When `survival_selector$operate()` is called, then objectives that
#'   are being minimized are multiplied with -1 (through [`mies_get_fitnesses`]), since [`Selector`]s always try to maximize fitness.\cr
#'   The [`Selector`] must be primed on `inst$search_space`; this *includes* the "budget" component
#'   when performing multi-fidelity optimization. Default is [`SelectorBest`].\cr
#'   The given [`Selector`] may *not* return duplicates.\cr
#' @template param_budget_id_maybenull
#' @template param_fidelity_maybenull
#' @param fidelity_new_individuals_only (`logical(1)`)\cr
#'   When `fidelity` is not `NULL`: Whether to re-evaluate individuals that are already present in `inst` should they have a smaller (if `fidelity_monotonic` is `TRUE`) or different
#'   (if `fidelity_monotonic` is `FALSE`) value from the one given to `fidelity`. Default `FALSE`. Ignored when `fidelity` is `NULL`.
#' @param fidelity_monotonic (`logical(1)`)\cr
#'   Whether to only re-evaluate configurations for which the fidelity would increase. Default `TRUE`.
#'   Ignored when `fidelity` is `NULL` or when `fidelity_new_individuals_only` is `TRUE`.
#' @param additional_component_sampler ([`Sampler`][paradox::Sampler] | `NULL`)\cr
#'   [`Sampler`][paradox::Sampler] for components of individuals that are not part of `inst`'s `$search_space`. These components
#'   are never used for performance evaluation, but they may be useful for self-adaptive [`OperatorCombination`]s. See the description
#'   of [`mies_prime_operators()`] on how operators need to be primed to respect additional components.\cr
#'   It is possible that `additional_component_sampler` is used for *more* rows than `initializer`, which happens
#'   when the `inst`'s `$archive` contains prior evaluations that are alive, but does not contain columns pertaining to additional columns,
#'   or contains *all* these columns but there are rows that are `NA` valued. If only *some* of the columns are present, or if all these columns
#'   are present but there are rows that are only `NA` valued for some columns, then an error is thrown.\cr
#'   Default is `NULL`: no additional components.
#' @return [invisible] [`OptimInstance`][bbotk::OptimInstance]: the input
#'   instance, modified by-reference.
#'
#' @family mies building blocks
#' @examples
#' library("bbotk")
#' lgr::threshold("warn")
#'
#' # Define the objective to optimize
#' objective <- ObjectiveRFun$new(
#'   fun = function(xs) {
#'     z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
#'     list(Obj = z)
#'   },
#'   domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4)),
#'   codomain = ps(Obj = p_dbl(tags = "maximize"))
#' )
#'
#' # Get a new OptimInstance
#' oi <- OptimInstanceSingleCrit$new(objective,
#'   terminator = trm("evals", n_evals = 100)
#' )
#'
#' mies_init_population(inst = oi, mu = 3)
#'
#' # 3 evaluations, archive contains 'dob' and 'eol'
#' oi$archive
#'
#' ###
#' # Advanced demo, making use of additional components and fidelity
#' ##
#'
#' # Get a new OptimInstance
#' oi <- OptimInstanceSingleCrit$new(objective,
#'   terminator = trm("evals", n_evals = 100)
#' )
#'
#' mies_init_population(inst = oi, mu = 3, budget_id = "y", fidelity = 2,
#'   additional_component_sampler = Sampler1DRfun$new(
#'     param = ParamDbl$new("additional", -1, 1), rfun = function(n) rep(-1, n)
#'   )
#' )
#'
#' # 3 evaluations. We also have 'additional', sampled from rfun (always -1),
#' # which is ignored by the objective. Besides, we have "y", which is 2,
#' # according to 'fidelity'.
#' oi$archive
#'
#' @export
mies_init_population = function(inst, mu, initializer = generate_design_random, survival_selector = SelectorBest$new()$prime(inst$search_space), budget_id = NULL, fidelity = NULL, fidelity_new_individuals_only = FALSE, fidelity_monotonic = TRUE, additional_component_sampler = NULL) {
  assert_optim_instance(inst)

  assert_int(mu, lower = 0, tol = 1e-100)
  assert_function(initializer, args = c("param_set", "n"))
  assert_r6(survival_selector, "Selector")

  assert_r6(additional_component_sampler, "Sampler", null.ok = TRUE)

  ss_ids = inst$search_space$ids()
  ac_ids = if (is.null(additional_component_sampler)) character(0) else additional_component_sampler$param_set$ids()
  present_cols = colnames(inst$archive$data)

  assert_choice(budget_id, ss_ids, null.ok = is.null(fidelity))
  assert_scalar(fidelity, null.ok = TRUE)
  assert_flag(fidelity_monotonic)
  assert_flag(fidelity_new_individuals_only)
  if (is.null(fidelity)) fidelity_new_individuals_only = TRUE  # indicate that fidelity-reevals won't be necessary


  if (any(c("dob", "eol") %in% ac_ids)) {
    stop("'dob' and 'eol' may not be additional component dimensions.")
  }
  assert_names(ac_ids, disjunct.from = reserved_component_names)
  if (any(ss_ids %in% ac_ids)) {
    stopf("Search space and additional components name clash: %s", str_collapse(intersect(ss_ids, ac_ids)))
  }
  if (any(inst$objective$codomain$ids() %in% ac_ids)) {
    stopf("Objective codomain and additional components name clash: %s", str_collapse(intersect(inst$objective$codomain$ids(), ac_ids)))
  }
  if (any(ac_ids %in% present_cols) && !all(ac_ids %in% present_cols)) {
    stopf("Some, but not all, additional components already in archive: %s", str_collapse(intersect(present_cols, ac_ids)))
  }
  dob = batch_nr = NULL
  if (nrow(inst$archive$data)) {
    if ("eol" %nin% present_cols) {
      set(inst$archive$data, , "eol", NA_real_)
    } else if ("dob" %nin% present_cols) {
      stop("'eol' but not 'dob' column found in archive; this is an undefined state that would probably lead to bad behaviour, so stopping.")
    }

    if ("dob" %nin% present_cols) {
      set(inst$archive$data, , "dob", 0)
    }
    data = inst$archive$data  # adding columns is not guaranteed to happen by reference, so we need to be careful with copies of data!
    assert_integerish(data$dob, lower = 0, any.missing = FALSE, tol = 1e-100)
    assert_integerish(data$eol, lower = 0, tol = 1e-100)
  }
  alive = which(is.na(inst$archive$data$eol))
  n_alive = length(alive)

  # use survival_selector to kill superfluous individuals, if any
  # if the survival selector does not depend on the additional components, then we can do this right away here.
  # otherwise we sample the remaining additional components and *then* select
  if (n_alive > mu && !any(survival_selector$primed_ps$ids() %in% ac_ids)) {
    survivors = mies_select_from_archive(inst, mu, alive, survival_selector, get_indivs = FALSE, group_size = mu)
    if (anyDuplicated(survivors)) stop("survival_selector may not generate duplicates.")
    died = setdiff(alive, survivors)
    set(data, died, "eol", max(data$dob))
    alive = sort(survivors)  # want deterministic assignment of additional_component_sampler results
    n_alive = mu
  }
  mu_remaining = mu - n_alive

  # take care of additional components
  # we need to check if / how many additional components we need to sample. If there are no alive individuals,
  # then this is just mu. If there are alive individuals, it *may* also be mu (we will have to insert components
  # at those rows where there are alive individuals!)

  # how many more rows of additional components need to be sampled.
  # at least mu (if new indivs are sampled)
  # but maybe more (if more than mu are alive *and* they need to be selected using a survival_selector that depends on additional components.
  additional_needed = max(mu, n_alive)
  row_insert = alive  # at what rows of `data` additional components are inserted (some alive indivs may have them already, check this next)
  if (n_alive && length(ac_ids) && all(ac_ids %in% present_cols)) {
    # additional components already present in the archive. Let's check if there are any rows with NAs.
    missing_info = is.na(data[row_insert, ac_ids, with = FALSE])
    any_missing_ac = apply(missing_info, 1, all)
    if (any(apply(missing_info, 1, any) != any_missing_ac)) {
      stop("There are rows in inst$archive$data where some, but not all additional component values are NA.")
    }
    row_insert = row_insert[any_missing_ac]  # only insert at the rows where any/all values are missing
    additional_needed = max(0, mu - n_alive) + length(row_insert)  # need to generate components for the missing rows, and for the values that get sampled new
  }

  additional_components = NULL
  if (!is.null(additional_component_sampler)) {
    additional_components = assert_data_frame(additional_component_sampler$sample(additional_needed)$data, nrows = additional_needed)
    assert_names(colnames(additional_components), identical.to = ac_ids)
    # assert here that:
    # we either have some individuals that need to be sampled (mu_remaining > 0)
    #   in which case the additional_components table's first `length(row_insert)` and last `mu_remaining` rows are complementary
    # or that we have no more individuals to sample, in which case `additional_components` are exactly equal to the number of rows where things get inserted.
    assert_true((mu_remaining > 0 && length(row_insert) + mu_remaining == additional_needed) || (mu_remaining <= 0 && length(row_insert) == additional_needed))


    if (length(row_insert)) {
      set(data, row_insert, ac_ids, first(additional_components, length(row_insert)))
    } else {
      # if we don't insert anything, then this operation just adds the necessary columns in case they are missing, and does nothing otherwise.
      # ideally the set() above would do this, but it does not: https://github.com/Rdatatable/data.table/issues/5409
      inst$archive$data[0, ac_ids] = additional_components[0]
      data = inst$archive$data
    }
  }

  if (mu_remaining < 0) {
    # kill superfluous individuals, in the case that the survival selector needs additional components
    survivors = mies_select_from_archive(inst, mu, alive, survival_selector, get_indivs = FALSE, group_size = mu)
    if (anyDuplicated(survivors)) stop("survival_selector may not generate duplicates.")
    died = setdiff(alive, survivors)
    set(data, died, "eol", max(data$dob))
    mu_remaining = 0  # for further down where we call mies_evaluate_offspring to do budget re-evaluations
  }
  if (mu_remaining > 0 || !fidelity_new_individuals_only) {
    # mies_evaluate_offspring needs to be evaluated even if there are no new indivs if fidelity-reevals could be necessary.
    sample_space = inst$search_space
    if (!is.null(budget_id)) {
      sample_space = ParamSetShadow$new(sample_space, budget_id)
    }
    offspring = cbind(as.data.table(assert_data_frame(initializer(sample_space, mu_remaining)$data, nrows = mu_remaining)), last(additional_components, mu_remaining))

    mies_evaluate_offspring(inst, offspring = offspring, budget_id = budget_id, fidelity = fidelity,
      reevaluate_fidelity = if (!fidelity_new_individuals_only) fidelity, fidelity_monotonic = fidelity_monotonic)
  }
  invisible(inst)
}

#' @title Get Fitness Values from OptimInstance
#'
#' @description
#' Get fitness values in the correct form as used by [`Selector`] operators from an
#' [`OptimInstance`][bbotk::OptimInstance].
#' This works for both single-criterion and multi-criterion optimization, and entails multiplying
#' objectives with -1 if they are being minimized, since [`Selector`] tries to maximize fitness.
#'
#' @template param_inst
#' @template param_rows
#' @return `numeric` `matrix` with `length(rows)` (if `rows` is given, otherwise `nrow(inst$archive$data)`) rows
#' and one column for each objective: fitnesses to be maximized.
#' @family mies building blocks
#' @examples
#' set.seed(1)
#' library("bbotk")
#' lgr::threshold("warn")
#'
#' # Define the objective to optimize
#' objective <- ObjectiveRFun$new(
#'   fun = function(xs) {
#'     z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
#'     list(Obj = z)
#'   },
#'   domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4)),
#'   codomain = ps(Obj = p_dbl(tags = "maximize"))
#' )
#'
#' # Get a new OptimInstance
#' oi <- OptimInstanceSingleCrit$new(objective,
#'   terminator = trm("evals", n_evals = 100)
#' )
#'
#' mies_init_population(inst = oi, mu = 3)
#'
#' oi$archive
#'
#' mies_get_fitnesses(oi, c(2, 3))
#'
#' ###
#' # Multi-objective, and automatic maximization:
#' objective2 <- ObjectiveRFun$new(
#'   fun = function(xs) list(Obj1 = xs$x^2, Obj2 = -xs$y^2),
#'   domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4)),
#'   codomain = ps(
#'     Obj1 = p_dbl(tags = "minimize"),
#'     Obj2 = p_dbl(tags = "maximize")
#'   )
#' )
#' # Using MultiCrit!
#' oi <- OptimInstanceMultiCrit$new(objective2,
#'   terminator = trm("evals", n_evals = 100)
#' )
#'
#' mies_init_population(inst = oi, mu = 3)
#'
#' oi$archive
#'
#' # Note Obj1 has a different sign than in the archive.
#' mies_get_fitnesses(oi, c(2, 3))
#'
#' @export
mies_get_fitnesses = function(inst, rows) {
  assert_optim_instance(inst)

  multiplier = map_dbl(inst$archive$codomain$tags, function(x) switch(x, minimize = -1, maximize = 1, 0))
  fitnesses = as.matrix(inst$archive$data[rows, inst$archive$codomain$ids()[multiplier != 0], with = FALSE])
  multiplier = multiplier[multiplier != 0]
  sweep(fitnesses, 2L, multiplier, `*`)
}

#' @title Select Individuals from an OptimInstance
#'
#' @include Selector.R
#'
#' @description
#' Apply a [`Selector`] operator to a subset of configurations inside
#' an [`OptimInstance`][bbotk::OptimInstance]
#' and return the index within the archive (when `get_indivs` `FALSE`) or the configurations themselves
#' (when `get_indivs` is `TRUE`).
#'
#' It is not strictly necessary for the selector to select unique individuals / individuals without replacement.
#'
#' Individuals are selected independently of whether they are "alive" or not. To select only from alive individuals,
#' set `rows` to `inst$archive$data[, which(is.na(eol))]`.
#'
#' @template param_inst
#' @param n_select (`integer(1)`)\cr
#'   Number of individuals to select.
#' @template param_rows
#' @param selector ([`Selector`])\cr
#'   [`Selector`] operator that selects individuals depending on configuration values
#'   and objective results. When `selector$operate()` is called, then objectives that
#'   are being minimized are multiplied with -1 (through [`mies_get_fitnesses()`]), since [`Selector`]s always try to maximize fitness.
#'   Defaults to [`SelectorBest`].\cr
#'   The [`Selector`] must be primed on a superset of `inst$search_space`; this *includes* the "budget" component
#'   when performing multi-fidelity optimization. All components on which `selector` is primed on must occur in the archive.\cr
#'   The given [`Selector`] *may* return duplicates.
#' @param group_size (`integer`)\cr
#'   Sampling group size hint, indicating that the caller would prefer there to not be any duplicates within this group size.
#'   The [`Selector`] may or may not ignore this value, however.
#'   This may possibly happen because of certain configuration parameters, or because the input size is too small.\cr
#'   Must either be a scalar value or sum up to `n_select`. Must be non-negative. A scalar value of 0 is interpreted the same as 1.\cr
#'   Default is 1.
#' @param get_indivs (`logical(1)`)\cr
#'   Whether to return configuration values from within the archive (`TRUE`) or just the indices within
#'   the archive (`FALSE`). Default is `TRUE`.
#' @return `integer` | [`data.table`][data.table::data.table]: Selected individuals, either index into `inst` or subset of archive table,
#'   depending on `get_indivs`.
#' @family mies building blocks
#' @examples
#' set.seed(1)
#' library("bbotk")
#' lgr::threshold("warn")
#'
#' # Define the objective to optimize
#' objective <- ObjectiveRFun$new(
#'   fun = function(xs) {
#'     z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
#'     list(Obj = z)
#'   },
#'   domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4)),
#'   codomain = ps(Obj = p_dbl(tags = "maximize"))
#' )
#'
#' # Get a new OptimInstance
#' oi <- OptimInstanceSingleCrit$new(objective,
#'   terminator = trm("evals", n_evals = 100)
#' )
#'
#' s = sel("best")
#' s$prime(oi$search_space)
#'
#' mies_init_population(inst = oi, mu = 6)
#'
#' oi$archive
#'
#' # Default: get individuals
#' mies_select_from_archive(oi, n_select = 2, rows = 1:6, selector = s)
#'
#' # Alternatively: get rows within archive
#' mies_select_from_archive(oi, n_select = 2, rows = 1:6, selector = s,
#'   get_indivs = FALSE)
#'
#' # Rows gotten from archive are relative from *all* rows, not from archive[rows]:
#' mies_select_from_archive(oi, n_select = 2, rows = 3:6, selector = s,
#'   get_indivs = FALSE)
#'
#' ##
#' # When using additional components: mies_select_from_archive learns about
#' # additional components from primed selector.
#'
#' # Get a new OptimInstance
#' oi <- OptimInstanceSingleCrit$new(objective,
#'   terminator = trm("evals", n_evals = 100)
#' )
#'
#' mies_init_population(inst = oi, mu = 6,
#'   additional_component_sampler = Sampler1DRfun$new(
#'     param = ParamDbl$new("additional", -1, 1), rfun = function(n) -1
#'   )
#' )
#'
#' oi$archive
#'
#' # Wrong: using selector primed only on search space. The resulting
#' # individuals do not have the additional component.
#' mies_select_from_archive(oi, n_select = 2, rows = 1:6, selector = s)
#'
#' # Correct: selector must be primed on search space + additional component
#' mies_prime_operators(oi$search_space, selectors = list(s),
#'   additional_components = ps(additional = p_dbl(-1, 1)))
#'
#' mies_select_from_archive(oi, n_select = 2, rows = 1:6, selector = s)
#' @export
mies_select_from_archive = function(inst, n_select, rows, selector = SelectorBest$new()$prime(inst$search_space), group_size = 1, get_indivs = TRUE) {
  assert_optim_instance(inst)

  assert_r6(selector, "Selector")
  assert_true(selector$is_primed)
  assert_int(n_select, lower = 0, tol = 1e-100)
  if (!missing(rows)) assert_integerish(rows, lower = 1, upper =  nrow(inst$archive$data), tol = 1e-100)

  selector_params <- selector$primed_ps$ids()
  assert_subset(inst$search_space$ids(), selector_params)
  assert_subset(selector_params, colnames(inst$archive$data))

  if (n_select == 0) if (get_indivs) return(inst$archive$data[0, selector_params, with = FALSE]) else return(integer(0))
  indivs = inst$archive$data[rows, selector_params, with = FALSE]

  fitnesses = mies_get_fitnesses(inst, rows)

  selected = selector$operate(indivs, fitnesses, n_select, group_size)
  if (get_indivs) {
    indivs[selected]
  } else {
    if (missing(rows)) selected else rows[selected]
  }
}

#' @title Generate Offspring Through Mutation and Recombination
#'
#' @include Selector.R
#' @include Mutator.R
#' @include Recombinator.R
#' @include ParamSetShadow.R
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
#' @param parent_selector ([`Selector`] | `NULL`)\cr
#'   [`Selector`] operator that selects parent individuals depending on configuration values
#'   and objective results. When `parent_selector$operate()` is called, then objectives that
#'   are being minimized are multiplied with -1 (through [`mies_get_fitnesses()`]), since [`Selector`]s always try to maximize fitness.
#'   When this is `NULL` (default), then a [`SelectorBest`] us used.\cr
#'   The [`Selector`] must be primed on a superset of `inst$search_space`; this *includes* the "budget" component
#'   when performing multi-fidelity optimization. All components on which `selector` is primed on must occur in the archive.\cr
#'   The given [`Selector`] *may* return duplicates.
#' @param mutator ([`Mutator`] | `NULL`)\cr
#'   [`Mutator`] operation to apply to individuals selected out of `inst` using `parent_selector`.\cr
#'   The [`Mutator`] must be primed on a [`ParamSet`][paradox::ParamSet] similar to `inst$search_space`,
#'   but *without* the "budget" component when `budget_id` is given (multi-fidelity optimization). Such a
#'   [`ParamSet`][paradox::ParamSet] can be generated for example using [`mies_prime_operators`].\cr
#'   When this is `NULL` (default), then a [`MutatorNull`] is used, effectively disabling mutation.
#' @param recombinator ([`Recombinator`] | `NULL`)\cr
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
#' @examples
#' set.seed(1)
#'
#' library("bbotk")
#' lgr::threshold("warn")
#'
#' # Define the objective to optimize
#' objective <- ObjectiveRFun$new(
#'   fun = function(xs) {
#'     z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
#'     list(Obj = z)
#'   },
#'   domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4)),
#'   codomain = ps(Obj = p_dbl(tags = "maximize"))
#' )
#'
#' # Get a new OptimInstance
#' oi <- OptimInstanceSingleCrit$new(objective,
#'   terminator = trm("evals", n_evals = 100)
#' )
#'
#' # Demo operators
#' m = mut("gauss", sdev = 0.1)
#' r = rec("xounif")
#' s = sel("random")
#' # Operators must be primed
#' mies_prime_operators(objective$domain, list(m), list(r), list(s))
#'
#' # We would normally call mies_init_population, but for reproducibility
#' # we are going to evaluate three given points
#'
#' oi$eval_batch(data.table::data.table(x = 0:2, y = 2:0, dob = 1, eol = NA_real_))
#'
#' # Evaluated points:
#' oi$archive
#'
#' # Use default operators: no mutation, no recombination, parent_selctor is
#' # sel("best") --> get one individual, the one with highest performance in the
#' # archive (x = 1, y = 1).
#' # (Note 'mies_generate_offspring()' does not modify 'oi')
#' mies_generate_offspring(oi, lambda = 1)
#'
#' # Mutate the selected individual after selection. 'm' has 'sdev' set to 0.1,
#' # so the (x = 1, y = 1) is slightly permuted.
#' mies_generate_offspring(oi, lambda = 1, mutator = m)
#'
#' # Recombination, then mutation.
#' # Even though lambda is 1, there will be two individuals selected with
#' # sel("best") and recombined, because rec("xounif") needs two parents. One
#' # of the crossover results is discarded (respecting that 'lambda' is 1),
#' # the other is mutated and returned.
#' mies_generate_offspring(oi, lambda = 1, mutator = m, recombinator = r)
#'
#' # General application: select, recombine, then mutate.
#' mies_generate_offspring(oi, lambda = 5, parent_selector = s, mutator = m, recombinator = r)
#'
#' @export
mies_generate_offspring = function(inst, lambda, parent_selector = NULL, mutator = NULL, recombinator = NULL, budget_id = NULL) {
  assert_optim_instance(inst)

  assert_int(lambda, lower = 1, tol = 1e-100)
  assert_r6(parent_selector, "Selector", null.ok = TRUE)
  if (!is.null(parent_selector)) assert_true(parent_selector$is_primed)
  assert_r6(mutator, "Mutator", null.ok = TRUE)
  if (!is.null(mutator)) assert_true(mutator$is_primed)
  assert_r6(recombinator, "Recombinator", null.ok = TRUE)
  if (!is.null(recombinator)) assert_true(recombinator$is_primed)

  data = inst$archive$data

  if (!any(is.na(data$eol))) stop("No alive individuals. Need to run mies_init_population()?")

  assert_integerish(data$dob, lower = 0, any.missing = FALSE, tol = 1e-100)
  assert_integerish(data$eol, lower = 0, tol = 1e-100)

  ss_ids = inst$search_space$ids()
  assert_choice(budget_id, ss_ids, null.ok = TRUE)

  if (is.null(parent_selector)) {
    # need to find out what to prime parent_selector with: If mutator or recombinator exist,
    # then we use their SS and add budget_id, if necessary. This correctly infers additional_components.
    # Only if no operator at all was given do we default to inst$search_space
    if (!is.null(mutator)) {
      ps_ps = mutator$primed_ps
    } else if (!is.null(recombinator)) {
      ps_ps = recombinator$primed_ps
    } else {
      ps_ps = inst$search_space
    }
    if (!is.null(budget_id) && budget_id %nin% ps_ps$ids()) {
      ps_ps = ps_ps$clone()$add(inst$search_space$params[[budget_id]])
    }
    parent_selector = SelectorBest$new()$prime(ps_ps)
  }

  ps_ids = parent_selector$primed_ps$ids()

  assert_subset(ss_ids, ps_ids)
  assert_subset(ps_ids, colnames(data))
  if (is.null(mutator) || is.null(recombinator)) {
    selector_space = parent_selector$primed_ps
    if (!is.null(budget_id)) {
      selector_space = ParamSetShadow$new(selector_space, budget_id)
    }
    mutator = mutator %??% MutatorNull$new()$prime(selector_space)
    recombinator = recombinator %??% RecombinatorNull$new()$prime(selector_space)
  }

  assert_set_equal(mutator$primed_ps$ids(), setdiff(ps_ids, budget_id))
  assert_set_equal(recombinator$primed_ps$ids(), setdiff(ps_ids, budget_id))

  needed_recombinations = ceiling(lambda / recombinator$n_indivs_out)
  needed_parents = needed_recombinations * recombinator$n_indivs_in

  parents = mies_select_from_archive(inst, needed_parents, which(is.na(data$eol)), parent_selector, group_size = recombinator$n_indivs_in)
  if (!is.null(budget_id)) set(parents, , budget_id, NULL)

  recombined = recombinator$operate(parents)
  recombined = first(recombined, lambda)  # throw away things if we have too many (happens when n_indivs_out is not a divider of lambda)

  mutator$operate(recombined)
}

#' @title Filter Offspring
#'
#' @description
#' Uses a [`Filtor`] to extract a subset of individuals from a given set. The individuals are either returned directly (when `get_indivs` is `TRUE`)
#' or in form of an index into the given individuals (when `get_indivs` is `FALSE`).
#'
#' [`Filtor`]s must always select individuals without replacement, so selected individual indices are unique.
#'
#' @template param_inst
#' @param individuals (`data.frame` | [`data.table`][data.table::data.table])\cr
#'   Individuals to filter. Must have columns according to `filtor$primed_ps`, and must have at least `filtor$needed_input(lambda)` rows.
#' @param lambda (`integer(1)`)\cr
#'   Number of individuals to filter down to.
#' @param filtor ([`Filtor`] | `NULL`)\cr
#'   [`Filtor`] operator that filters. When `NULL` is given, then the [`FiltorNull`] operation is performed and the first `lambda` individuals are
#'   taken from `individuals`.
#' @template param_budget_id_maybenull
#' @param fidelity (`atomic` | `NULL`)\cr
#'   scalar indicating the value of the `budget_id` component with which to evaluate individuals to be filtered.\cr
#'   This value must be `NULL` when no multi-fidelity optimization is performed, but it may **also** be `NULL` when the
#'   maximum value of the `budget_id` found in `inst$archive` should be used (the default).
#' @param get_indivs (`logical(1)`)\cr
#'   Whether to return the `data.frame` or [`data.table`][data.table::data.table] of selected individuals, or an index into `individuals`.
#' @return If `get_indivs` is `TRUE`: a `data.frame` or [`data.table`][data.table::data.table] (depending on the input type of `individuals`) of filtered configurations.
#'   If `get_indivs` is `FALSE`: an `integer` vector indexing the filtered individuals.
#' @export
mies_filter_offspring = function(inst, individuals, lambda, filtor = NULL, budget_id = NULL, fidelity = NULL, get_indivs = TRUE) {
  assert_optim_instance(inst)
  assert_int(lambda, lower = 0, tol = 1e-100)
  individuals_dt = as.data.table(assert_data_frame(individuals, min.rows = lambda))
  assert_r6(filtor, "Filtor", null.ok = TRUE)
  data = inst$archive$data
  if (!nrow(data)) stop("mies_filter_offspring does not work with empty OptimInstance.")
  ss_ids = inst$search_space$ids()
  assert_choice(budget_id, ss_ids, null.ok = is.null(fidelity))

  assert_names(colnames(individuals), must.include = setdiff(ss_ids, budget_id), disjunct.from = budget_id, subset.of = colnames(data))

  current_gen = max(data$dob, 0, na.rm = TRUE)

  if (lambda == 0) if (get_indivs) individuals_dt[0] else integer(0)
  if (!is.null(budget_id)) {
    if (is.null(fidelity)) {
      fidelity = max(data[[budget_id]])
    } else {
      assert_scalar(fidelity)
    }
    set(individuals_dt, , budget_id, fidelity)
  }

  if (is.null(filtor)) {
    filtor = FiltorNull$new()$prime(inst$search_space)
    if (any(colnames(individuals_dt) %nin% ss_ids)) stop("filtor must be given when individuals contain additional components.")
  }

  f_ids = filtor$primed_ps$ids()
  assert_set_equal(f_ids, colnames(individuals_dt))

  known_values = inst$archive$data[, f_ids, with = FALSE]
  fitnesses = mies_get_fitnesses(inst)
  selected = filtor$operate(individuals_dt, known_values, fitnesses, lambda)
  if (get_indivs) {
    individuals[selected, ]
  } else {
    selected
  }
}


#' @title Get Performance Values by Generation
#'
#' @description
#' Get evaluated performance values from an [`OptimInstance`][bbotk::OptimInstance] for all individuals that were alive
#' at a given generation. Depending on `survivors_only`, all individuals alive at the *end* of a generation are returned,
#' or all individuals alive at any point during a generation.
#'
#' The resulting [`data.table`][data.table::data.table] object is formatted for easy manipulation to get relevant
#' information about optimization progress. To get aggregated values per generation, use `by = "dob"`.
#'
#' @template param_inst
#' @template param_as_fitnesses
#' @template param_survivors_only
#' @template param_condition_on_budget_id
#' @return a [`data.table`][data.table::data.table] with the column `"dob"`, indicating the generation, as well as further
#'   columns named by the [`OptimInstance`][bbotk::OptimInstance]'s objectives.
#'
#' @examples
#' library("bbotk")
#' lgr::threshold("warn")
#'
#' # Define the objective to optimize
#' objective <- ObjectiveRFun$new(
#'   fun = function(xs) {
#'     z <- 10 - exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
#'     list(Obj = z)
#'   },
#'   domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4)),
#'   codomain = ps(Obj = p_dbl(tags = "minimize"))
#' )
#'
#' oi <- OptimInstanceSingleCrit$new(objective,
#'   terminator = trm("evals", n_evals = 6)
#' )
#'
#' op <- opt("mies",
#'   lambda = 2, mu = 2,
#'   mutator = mut("gauss", sdev = 0.1),
#'   recombinator = rec("xounif"),
#'   parent_selector = sel("best")
#' )
#' set.seed(1)
#' op$optimize(oi)
#'
#' # negates objectives that are minimized:
#' mies_get_generation_results(oi)
#'
#' # real objective values:
#' mies_get_generation_results(oi, as_fitnesses = FALSE)
#'
#' # Individuals that died are included:
#' mies_get_generation_results(oi, survivors_only = FALSE)
#' @export
mies_get_generation_results = function(inst, as_fitnesses = TRUE, survivors_only = TRUE, condition_on_budget_id = NULL) {
  assert_optim_instance(inst)
  assert_flag(as_fitnesses)
  assert_flag(survivors_only)
  ss_ids = inst$search_space$ids()
  assert_choice(condition_on_budget_id, ss_ids, null.ok = TRUE)
  if (!is.null(condition_on_budget_id) && !inst$search_space$is_number[[condition_on_budget_id]]) {
    stopf("condition_on_budget_id for non-numeric components is not supported. It is '%s', which is not numeric.", condition_on_budget_id)
  }
  data = copy(inst$archive$data)
  present_cols = colnames(data)

  if ("eol" %nin% present_cols) {
    if (nrow(data)) set(data, , "eol", NA_real_)
  } else if ("dob" %nin% present_cols) {
    stop("'eol' but not 'dob' column found in archive; this is an undefined state.")
  }

  objectives = inst$archive$codomain$ids()

  if (as_fitnesses) {
    fitness_negate = map_lgl(inst$archive$codomain$tags, function(x) switch(x, minimize = TRUE, maximize = FALSE, NA))
    objectives = objectives[!is.na(fitness_negate)]
    fitness_negate = fitness_negate[!is.na(fitness_negate)]
    for (col_negate in objectives[fitness_negate]) {
      set(data, , col_negate, -data[[col_negate]])
    }
  }

  if (!nrow(data)) {
    return(data[, c("dob", objectives), with = FALSE])
  }

  if ("dob" %nin% present_cols) {
    set(data, , "dob", 0)
  }
  assert_integerish(data$dob, lower = 0, any.missing = FALSE, tol = 1e-100)
  assert_integerish(data$eol, lower = 0, tol = 1e-100)

  data$eol[is.na(data$eol)] = Inf  # don't set() here because 'eol' could be integer, in which case this doesn't work.

  generations = seq.int(min(data$dob), max(data$dob))

  gentbl = data.table(generations = generations)

  if (survivors_only) {
    conditions = c("dob <= generations", "eol > generations")
  } else {
    conditions = c("dob <= generations", "eol >= generations")
  }
  results = data[gentbl, c("dob", objectives, condition_on_budget_id), on = conditions, nomatch = NULL, with = FALSE]

  if (!is.null(condition_on_budget_id)) {
    results = results[, .SD[get(condition_on_budget_id) == max(get(condition_on_budget_id))], by = "dob"]
    set(results, , condition_on_budget_id, NULL)
  }

  results
}

#' @title Get Aggregated Performance Values by Generation
#'
#' @description
#' Get evaluated performance values from an [`OptimInstance`][bbotk::OptimInstance] aggregated for each generation.
#' This may either concern all individuals that were alive at the end of a given generation (`survivors_only` `TRUE`)
#' or at any point during a generation (`survivors_only` `FALSE`).
#'
#' The result is a single [`data.table`][data.table::data.table] object with a `dob` column indicating the
#' generation, as well as one column for each `aggregations` entry crossed with each objective of `inst`.
#'
#' @template param_inst
#' @param objectives (`character`)\cr
#'   Objectives for which to calculate aggregates. Must be a subset of the codomain elements of `inst`, but when `as_fitnesses` is `TRUE`, elements
#'   that are neither being minimized nor maximized are ignored.
#' @param aggregations (named `list` of `function`)\cr
#'   List containing aggregation functions to be evaluated on a vector of objective falues for each generation. These functions should take
#'   a single argument and return a scalar value.
#' @template param_as_fitnesses
#' @template param_survivors_only
#' @template param_condition_on_budget_id
#' @return a [`data.table`][data.table::data.table] with the column `"dob"`, indicating the generation, as well as further
#'   columns named by the items in `aggregations`. There is more on element in `objectives`
#'   (or more than one element not being minimized/maximized when `as_fitnesses` is `TRUE`), then columns are named `<aggregations element name>.<objective name>`.
#'   Otherwise, they are named by `<aggregations element name>` only. To get a guarantee that elements are only named after elements in `aggregations`, set `objectives`
#'   to a length 1 `character`.
#' @examples
#' library("bbotk")
#' lgr::threshold("warn")
#'
#' # Define the objective to optimize
#' objective <- ObjectiveRFun$new(
#'   fun = function(xs) {
#'     z <- 10 - exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
#'     list(Obj = z)
#'   },
#'   domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4)),
#'   codomain = ps(Obj = p_dbl(tags = "minimize"))
#' )
#'
#' oi <- OptimInstanceSingleCrit$new(objective,
#'   terminator = trm("evals", n_evals = 6)
#' )
#'
#' op <- opt("mies",
#'   lambda = 2, mu = 2,
#'   mutator = mut("gauss", sdev = 0.1),
#'   recombinator = rec("xounif"),
#'   parent_selector = sel("best")
#' )
#' set.seed(1)
#' op$optimize(oi)
#'
#' # negates objectives that are minimized:
#' mies_aggregate_generations(oi)
#'
#' # silly aggregation: first element
#' mies_aggregate_generations(oi, aggregations = list(first = function(x) x[1]))
#'
#' # real objective values:
#' mies_aggregate_generations(oi, as_fitnesses = FALSE)
#'
#' # Individuals that died are included:
#' mies_aggregate_generations(oi, survivors_only = FALSE)
#' @export
mies_aggregate_generations = function(inst, objectives = inst$archive$codomain$ids(), aggregations = list(min = min, mean = mean, max = max, median = stats::median, size = length),
    as_fitnesses = TRUE, survivors_only = TRUE, condition_on_budget_id = NULL) {
  assert_optim_instance(inst)
  assert_character(objectives, any.missing = FALSE, unique = TRUE)
  assert_subset(objectives, inst$archive$codomain$ids())

  generationed = mies_get_generation_results(inst, as_fitnesses = as_fitnesses, survivors_only = survivors_only, condition_on_budget_id = condition_on_budget_id)

  generationed = generationed[, c("dob", intersect(colnames(generationed), objectives)), with = FALSE]

  if (ncol(generationed) == 1) return(generationed)  # 'objectives' did not contain any columns that are minimized/maximized; may in particular happen when as_fitnesses is TRUE

  eol = aggregations  # 'generationed' is guaranteed not to contain the 'eol' column. This is necessary, unfortunately, for the case that an objective is named 'aggregations'.
  generationed[, unlist(lapply(eol, function(agf) lapply(if (length(.SD) == 1) unname(.SD) else .SD, agf)), recursive = FALSE), by = "dob"]
}

#' @title Get the Last Generation that was Evaluated
#'
#' @description
#' Gets the last generation that was evaluated as counted by the `"dob"` column in the [`OptimInstance`][bbotk::OptimInstance]'s [`Archive`][bbotk::Archive].
#'
#' This accepts [`OptimInstance`][bbotk::OptimInstance]s that were not evaluated with `miesmuschel` and are therefore missing the `"dob"` column, returning
#' a value of 0. However, if the `"dob"` column is invalid (the inferred generation is not integer numeric or not non-negative), an error is thrown.
#'
#' @template param_inst
#' @return a scalar integer value indicating the last generation that was evaluated in `inst`. It is 0 when `inst` is empty, and also typically 0 if all evaluations
#'   in `inst` so far were performed outside of `miesmuschel`. Every call of [`mies_init_population`] that actually performs evaluations, as well as each call to
#'   [`mies_evaluate_offspring`] with non-empty `offspring`, increases the generation by 1.
#' @examples
#' library("bbotk")
#' lgr::threshold("warn")
#'
#' # Define the objective to optimize
#' objective <- ObjectiveRFun$new(
#'   fun = function(xs) {
#'     z <- 10 - exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
#'     list(Obj = z)
#'   },
#'   domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4)),
#'   codomain = ps(Obj = p_dbl(tags = "minimize"))
#' )
#'
#' oi <- OptimInstanceSingleCrit$new(objective,
#'   terminator = trm("evals", n_evals = 6)
#' )
#'
#' op <- opt("mies",
#'   lambda = 2, mu = 2,
#'   mutator = mut("gauss", sdev = 0.1),
#'   recombinator = rec("xounif"),
#'   parent_selector = sel("best")
#' )
#' set.seed(1)
#'
#' mies_generation(oi)
#'
#' op$optimize(oi)
#' mies_generation(oi)
#'
#' oi$terminator = trm("evals", n_evals = 10)
#'
#' op$optimize(oi)
#' mies_generation(oi)
#' @export
mies_generation = function(inst) {
  assert_optim_instance(inst)
  (assert_int(max(inst$archive$data$dob, 0, na.rm = TRUE), lower = 0, tol = 1e-100))  # parentheses because we don't want to return invisibly
}
