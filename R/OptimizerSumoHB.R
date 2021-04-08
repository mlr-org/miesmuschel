#' @title Surrogate Model Assisted Hyperband Optimizer
#'
#' @description
#' Perform Surrogate Model Assisted Hyperband Optimization.
#'
#' Given a population size `mu`, a fraction of surviving individuals `survival_fraction`, a number of generations `n` and a fidelity progression `F1`, `F1`, ..., `Fn`,
#' the algorithm works as follows:
#' 1. Sample an initial design of size `mu` at fidelity `F1`
#' 2. Kill individuals that are not in the top `survival_fraction` part of individuals, by performance
#' 3. Generate new individuals using random sampling and optionally a [`Filtor`], until `mu` alive individuals are present again.
#' 4. Evaluate all alive individuals at fidelity `F{generation}`
#' 5. Jump to 2., until termination, possibly because `n` generations are reached.
#'
#' The number of generations `n` is determined from the [`OptimInstance`][bbotk::OptimInstance]'s [`Terminator`][bbotk::Terminator] object, see **Terminating**
#' below.
#'
#' The **fidelity progression** uses a specially designated "budget" parameter of the [`OptimInstance`][bbotk::OptimInstance], which must have a `"budget"` tag.
#' The lower limit of the budget parameter is used as `F0`. The upper limit is used as `Fn`. The budget is evaluated at equally spaced values in generations `0..n`,
#' so `F1` - `F0` = `F2` - `F1` etc. In many cases it is desirable to have a multiplicative progression of "difficulty" of the problem. In this case, it is
#' recommended to use a budget parameter with exponential "trafo", or one with `logscale = TRUE` (see example).
#'
#' A [`Filtor`] can be used for filtering, see the respective class's documentation for details on algorithms. The [`FiltorSurrogateProgressive`] can be used
#' for progressive surrogate model filtering. [`FiltorMaybe`] can be used for random interleaving.
#'
#' @section Fidelity Steps:
#' The number of fidelity steps can be determined through the `fidelity_steps` configuration parameter, or can be determined when a
#' [`TerminatorGenerations`] is used to determine the number of fidelity refinements that are being performed. For this, the  [`OptimInstance`][bbotk::OptimInstance]
#' being optimized must contain a [`TerminatorGenerations`], either directly (`inst$terminator`), or indirectly through a
#' [`bbotk::TerminatorCombo`] with `$any` set to `TRUE` (recursive [`TerminatorCombo`][bbotk::TerminatorCombo] may also be used). When `fidelity_steps` is `0`,
#' the number of generations is determined from the given [`Terminator`][bbotk::Terminator] object and the number of fidelity refinements
#' is planned according to this number. Other terminators may be present in a [`TerminatorCombo`][bbotk::TerminatorCombo] that may
#' lead to finishing the tuning process earlier.
#'
#' It is possible to continue optimization runs that quit early due to other terminators. It is not recommended to change `fidelity_steps` (or the number of generations
#' when `fidelity_steps` is 0) between run continuations, however, unless the fidelity bounds are also adjusted, since the continuation would then have a decrease in fidelity.
#'
#' @section Configuration Parameters:
#' `OptimizerSumoHB`'s configuration parameters are the hyperparameters of the [`Filtor`] given to the `filtor` construction argument, as well as:
#'
#' * `mu` :: `integer(1)`\cr
#'   Population size: Number of individuals that are sampled in the beginning, and which are re-evaluated in each fidelity step. Initialized to 2.
#' * `survival_fraction` :: `numeric(1)`\cr
#'   Fraction of the population that survives at each fidelity step. The number of newly sampled individuals is (1 - `survival_fraction`) * `mu`. Initialized to 0.5.
#' * `sampling` :: `function`\cr
#'   Function that generates the initial population, as well as new individuals to be filtered from, as a [`Design`][paradox::Design] object. The function must have
#'   arguments `param_set` and `n` and function like [`paradox::generate_design_random`] or [`paradox::generate_design_lhs`].
#'   This is equivalent to the `initializer` parameter of [`mies_init_population()`], see there for more information. Initialized to
#'   [`generate_design_random()`][paradox::generate_design_random].
#' * `fidelity_steps` :: `integer(1)`\cr
#'   Number of fidelity steps. When it is 0, the number is determined from the [`OptimInstance`][bbotk::OptimInstance]'s [`Terminator`][bbotk::Terminator]. See the
#'   section **Fidelity Steps** for more details. Initialized to 0.
#' * `filter_with_max_budget` :: `logical(1)`\cr
#'   Whether to perform filtering with the maximum fidelity value found in the archive, as opposed the current `budget_survivors`. This has only an effect when
#'   `fidelity_steps` is greater than 1 and some evaluations are done when the archive already contains evaluations with greater fidelity. Initialized to `FALSE`.
#'
#' @param filtor ([`Filtor`])\cr
#'   [`Filtor`] for the filtering algorithm. Default is [`FiltorProxy`], which exposes the operation as
#'   a configuration parameter of the optimizer itself.\cr
#'   The `$filtor` field will reflect this value.
#'
#' @family optimizers
#' @examples
#' \donttest{
#' lgr::threshold("warn")
#'
#' # Define the objective to optimize
#' # The 'budget' here simulates averaging 'b' samples from a noisy function
#' objective <- ObjectiveRFun$new(
#'   fun = function(xs) {
#'     z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
#'     z <- z + rnorm(1, sd = 1 / sqrt(xs$b))
#'     list(Obj = z)
#'   },
#'   domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4), b = p_int(1)),
#'   codomain = ps(Obj = p_dbl(tags = "maximize"))
#' )
#'
#' search_space = objective$domain$search_space(list(
#'   x = to_tune(),
#'   y = to_tune(),
#'   b = to_tune(p_int(1, 2^10, logscale = TRUE, tags = "budget"))
#' ))
#'
#' # Get a new OptimInstance. Here we determine that the optimizatoin goes
#' # for 10 generations.
#' oi <- OptimInstanceSingleCrit$new(objective,
#'   search_space = search_space,
#'   terminator = trm("gens", generations = 10)
#' )
#'
#' library("mlr3learners")
#' # use the 'regr.ranger' as surrogate.
#' # The following settings have 30 individuals in a batch, the 20 best
#' # of which survive, while 10 are sampled new.
#' # For this, 100 individuals are sampled randomly, and the top 10, according
#' # to the surrogate model, are used.
#' sumohb_opt <- opt("sumohb", ftr("surprog",
#'     surrogate_learner = mlr3::lrn("regr.ranger"),
#'     filter_rate_first = 100, filter_rate_per_sample = 0),
#'   mu = 30, survival_fraction = 2/3
#' )
#' # sumohb_opt$optimize performs SumoHB optimization and returns the optimum
#' sumohb_opt$optimize(oi)
#'
#' #####
#' # Optimizing a Machine Learning Method
#' #####
#'
#' # Note that this is a short example, aiming at clarity and short runtime.
#' # The settings are not optimal for hyperparameter tuning.
#'
#' library("mlr3")
#' library("mlr3learners")
#' library("mlr3tuning")
#'
#' # The Learner to optimize
#' learner = lrn("classif.xgboost")
#'
#' # The hyperparameters to optimize
#' learner$param_set$values[c("eta", "booster")] = list(to_tune())
#' learner$param_set$values$nrounds = to_tune(p_int(1, 4, tags = "budget", logscale = TRUE))
#'
#' # Get a TuningInstance
#' ti = TuningInstanceSingleCrit$new(
#'   task = tsk("iris"),
#'   learner = learner,
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.acc"),
#'   terminator = trm("gens", generations = 3)
#' )
#'
#' # use ftr("maybe") for random interleaving: only 50% of proposed points are filtered.
#' sumohb_tune <- tnr("sumohb", ftr("maybe", p = 0.5, filtor = ftr("surprog",
#'     surrogate_learner = lrn("regr.ranger"),
#'     filter_rate_first = 100, filter_rate_per_sample = 0)),
#'   mu = 20, survival_fraction = 0.5
#' )
#' # sumohb_tune$optimize performs SumoHB optimization and returns the optimum
#' sumohb_tune$optimize(ti)
#'
#' }
#' @export
OptimizerSumoHB = R6Class("OptimizerSumoHB", inherit = Optimizer,
  public = list(
    #' @description
    #' Initialize the 'OptimizerSumoHB' object.
    initialize = function(filtor = FiltorProxy$new()) {
      private$.filtor = assert_r6(filtor, "Filtor")
      param_set = ps(
        mu = p_int(1, tags = "required"),
        survival_fraction = p_dbl(0, 1, tags = "required"),
        sampling = p_uty(custom_check = function(x) check_function(x, args = c("param_set", "n")), tags = c("init", "required")),
        fidelity_steps = p_int(0, tags = "required"),
        filter_with_max_budget = p_lgl(tags = "required")
      )
      param_set$values = list(mu = 2, survival_fraction = 0.5, sampling = generate_design_lhs, fidelity_steps = 0, filter_with_max_budget = FALSE)

      private$.own_param_set = param_set

      private$.param_set_source = alist(private$.own_param_set, private$.filtor$param_set)

      can_dependencies = TRUE  # TODO filtor needs to announce this

      super$initialize(
        param_set = self$param_set, param_classes = private$.filtor$param_classes,
        properties = c(if (can_dependencies) "dependencies", "single-crit"),
        packages = "miesmuschel"  # TODO: packages from filtor, this is in a different branch currently
      )
    }
  ),
  active = list(
    #' @field filtor ([`Filtor`])\cr
    #' Filtering algorithm used.
    filtor = function(rhs) {
      ret = private$.filtor
      if (!missing(rhs) && !identical(rhs, ret)) {
        stop("filtor is read-only.")
      }
      ret
    },
    #' @field param_set ([`ParamSet`][paradox::ParamSet])\cr
    #' Configuration parameters of the optimization algorithm.
    param_set = function(rhs) {
      if (is.null(private$.param_set)) {
        sourcelist = lapply(private$.param_set_source, function(x) eval(x))
        private$.param_set = ParamSetCollection$new(sourcelist)
        if (!is.null(private$.param_set_id)) private$.param_set$set_id = private$.param_set_id
      }
      if (!missing(rhs) && !identical(rhs, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    }
  ),
  private = list(
    deep_clone = function(name, value) {
      if (!is.null(private$.param_set_source)) {
        if (!is.null(private$.param_set)) {
          private$.param_set_id = private$.param_set$set_id
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
    .optimize = function(inst) {
      params = self$param_set$get_values()

      budget_id = inst$search_space$ids(tags = "budget")
      if (length(budget_id) != 1) stopf("Need exactly one budget parameter for multifidelity method, but found %s: %s",
        length(budget_id), str_collapse(budget_id))

      generations = params$fidelity_steps
      if (generations == 0) generations = terminator_get_generations(inst$terminator)
      if (!is.finite(generations)) {
        stop("When fidelity_steps is 0, then the given OptimizationInstance must have a TerminatorGenerations, or a TerminatorCombo with 'any = TRUE' containing at least one TerminatorGenerations (possibly recursively).")
      }
      if (generations < 1) {
        stopf("At least one generation must be evaluated (at full fidelity), but terminator had %s generations.", generations)
      }

      # use rev(seq(upper, lower)), so that with sequence length 1, we get the upper bound only.
      budget_progression = rev(seq(inst$search_space$upper[[budget_id]], inst$search_space$lower[[budget_id]], length.out = generations))
      fidelity_schedule_base = data.table(generation = seq_len(generations), budget_new = budget_progression, budget_survivors = budget_progression)

      # if this is a continued run, then `inst$archie$data$dob` may already contain 'dob' values, and mies_init_population will perform
      # evaluations with 'max(inst$archive$data$dob) + 1`. fidelity_schedule then needs to be recycled, so we get the last available generation here.
      last_gen = max(inst$archive$data$dob, 0, na.rm = TRUE)

      fidelity_schedule = recycle_fidelity_schedule(fidelity_schedule_base, last_gen, generations)

      mutator = MutatorErase$new()
      mutator$param_set$values$initializer = params$sampling
      survival_selector = SelectorBest$new()
      parent_selector = SelectorRandom$new()
      parent_selector$param_set$values$replace = TRUE

      mies_prime_operators(mutators = list(mutator), selectors = list(survival_selector, parent_selector), filtors = list(private$.filtor),
        search_space = inst$search_space, budget_id = budget_id)

      mies_init_population(inst, mu = params$mu, initializer = params$sampling, fidelity_schedule = fidelity_schedule,
        budget_id = budget_id)

      survivors = max(round(params$survival_fraction * params$mu), 1)
      if (survivors == params$mu) {
        stop("Number of survivors equals the total population size. survival_fraction must be lower or mu must be larger.")
      }
      lambda = params$mu - survivors
      pre_filter_size = private$.filtor$needed_input(lambda)
      pre_filter_size_wraparound = private$.filtor$needed_input(params$mu)  # need this many indivs pre-filter when 'generations' are reached and all indivs are sampled new.

      repeat {
        last_gen = max(inst$archive$data$dob, na.rm = TRUE)
        if (last_gen %% generations == 0) {
          fidelity_schedule = recycle_fidelity_schedule(fidelity_schedule_base, last_gen, generations)
          # generation cycle is over; kill all individuals, sample new ones.
          keep_alive= 0
          sample_new = pre_filter_size_wraparound
          filter_down_to = params$mu
        } else {
          keep_alive = survivors
          sample_new = pre_filter_size
          filter_down_to = lambda
        }
        offspring = mies_generate_offspring(inst, lambda = sample_new, parent_selector = parent_selector, mutator = mutator, budget_id = budget_id)
        mies_survival_plus(inst, mu = keep_alive, survival_selector = survival_selector)
        offspring = mies_filter_offspring(inst, offspring, filter_down_to, private$.filtor,
          fidelity_schedule = if (!params$filter_with_max_budget) fidelity_schedule, budget_id = budget_id)
        mies_evaluate_offspring(inst, offspring = offspring, fidelity_schedule = fidelity_schedule, budget_id = budget_id, step_fidelity = TRUE)
      }
    },
    .own_param_set = NULL,
    .param_set_id = NULL,
    .param_set_source = NULL,
    .filtor = NULL
  )
)

terminator_get_generations = function(x) {
  UseMethod("terminator_get_generations")
}

terminator_get_generations.default = function(x) {
  stop("Invalid terminator given.")
}

terminator_get_generations.Terminator = function(x) Inf  # normie terminator not limiting generations in any way

terminator_get_generations.TerminatorGenerations = function(x) x$param_set$values$generations

terminator_get_generations.TerminatorCombo = function(x) {
  if (x$param_set$values$any) {
    # Terminate on "any" condition being true --> minimum of generations of child objects
    min(sapply(x$terminators, terminator_get_generations))
  } else {
    max(sapply(x$terminators, terminator_get_generations))
  }
}

recycle_fidelity_schedule = function(fidelity_schedule_base, last_gen, generations) {
  current_cycle = floor(last_gen / generations)
  if (current_cycle == 0) {
    fidelity_schedule_base
  } else {
    # fidelity wrap-around
    # build the new fidelity schedule: it goes from last_gen + 1 .. last_gen + generations.
    # BUT: formal requirement for fidelity_schedule is that '1' also occurs in 'generations' column, so we just
    # copy the fidelity_schedule_base.
    rbind(fidelity_schedule_base, copy(fidelity_schedule_base)[, generation := generation + generations * current_cycle])
  }
}
