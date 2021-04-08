#' @title Surrogate Model Assisted Hyperband Optimizer
#'
#' @description
#' Perform Surrogate Model Assisted Hyperband Optimization.
#'
#' Given a population size `mu`, a fraction of surviving individuals `survival_fraction`, a number of generations `n` and a fidelity progression `F0`, `F1`, ..., `Fn`,
#' the algorithm works as follows:
#' 1. Sample an initial design of size `mu` at fidelity `F0`
#' 2. Kill individuals that are not in the top `survival_fraction` part of individuals, by performance
#' 3. Generate new individuals using either random sampling, or **surrogate model filtering**, until `mu` alive individuals are present again.
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
#' @section Terminating:
#' [`TerminatorGenerations`] is used to determine the number of fidelity refinements to be performed. Therefore, the  [`OptimInstance`][bbotk::OptimInstance]
#' being optimized must contain a [`TerminatorGenerations`]. Either directly (`inst$terminator`), or indirectly through a
#' [`bbotk::TerminatorCombo`] with `$any` set to `TRUE` (recursive [`TerminatorCombo`][bbotk::TerminatorCombo] may also be used). The
#' number of generations is determined from the given [`Terminator`][bbotk::Terminator] object and the number of fidelity refinements
#' is planned according to this number. Other terminators may be present in a [`TerminatorCombo`][bbotk::TerminatorCombo] that may
#' lead to finishing the tuning process earlier.
#'
#' TODO: what does generations == 0 mean?
#'
#' @section Surrogate Model Filtering:
#' A *surrogate model* is a regression model, based on an [`mlr3::Learner`], which predicts the approximate performance of newly sampled configurations
#' given the empirical performance of already evaluated configurations. If the optional `surrogate_learner` construction argument is given to `SumoHB`,
#' then the surrogate model is used to propose points that have, according to the surrogate model, a relatively high chance of performing well.
#'
#' Given the number `lambda` of of new individuals to sample, surrogate model filtering proceeds as follows:
#' 1. Sample `filter_rate_first` configurations, predict their expected performance using the surrogate model, and put them
#'    into a pool `P` of configurations to consider.
#' 2. Take the individual that is optimal according to predicted performance, remove it from `P` and add it to solution set `S`.
#' 3. If the number of solutions in `S` equals `lambda`, quit.
#' 4. Sample `filter_rate_per_sample` configurations, predict their expected performance using the surrogate model, and add them to `P`.
#' 5. Jump to 2.
#'
#' (The algorithm presented here is optimized for clarity; the actual implementation does all the surrogate model prediction in one go, but is functionally
#' equivalent).
#'
#' The `filter_rate_first` and `filter_rate_per_sample` configuration parameters of this algorithm determine how agressively the surrogate model is used to
#' filter out sampled configurations. If the filtering is agressive (`filter_rate_first` is large), then more "exploitation" at the cost of "exploration" is performed.
#' When `filter_rate_first` is small but `filter_rate_per_sample` is large, then successive individuals are filtered successively more agressively, potentially
#' leading to a tradeoff between "exploration" and "exploitation".
#'
#' When `filter_rate_per_sample` is set to 0, then the method is equivalent to sampling the top `lambda` individuals from `filter_rate_first`
#' sampled ones. When `filter_rate_per_sample` is 1 and `filter_rate_first` is 0, then the method is equivalent to random sampling.
#'
#' `filter_rate_first` and `filter_rate_per_sample` may be fractional; the total number of individuals to select from when selecting `i`
#' individuals is always round(`filter_rate_first` + (`filter_rate_per_sample` - 1) * (`i` - 1)). However, `filter_rate_first` must
#' be at least 1, and `filter_rate_first` + `filter_rate_per_sample` * (`lambda` - 1) must be at least `lambda`.
#'
#' @section Configuration Parameters:
#' `OptimizerSumoHB`'s configuration parameters are the hyperparameters of the `surrogate_learner` [`Learner`][mlr3::Learner], as well as:
#'
#' * `mu` :: `integer(1)`\cr
#'   Population size: Number of individuals that are sampled in the beginning, and which are re-evaluated in each fidelity step. Initialized to 2.
#' * `survival_fraction` :: `numeric(1)`\cr
#'   Fraction of the population that survives at each fidelity step. The number of newly sampled individuals is (1 - `survival_fraction`) * `mu`.
#' * `filter_rate_first` :: `numeric(1)`\cr
#'   Only present when the `surrogate_learner` construction argument is not `NULL`.
#'   `filter_rate_first` parameter of the surrogate model filtering algorithm, see the corresponding section. Initialized to 1. Together with the
#'   default of `filter_rate_per_sample`, this is equivalent to random sampling new individuals.
#' * `filter_rate_per_sample` :: `numeric(1)`\cr
#'   Only present when `surrogate_learner` construction argument is not `NULL`.
#'   `filter_rate_per_sample` parameter of the surrogate model filtering algorithm, see the corresponding section.
#'   Initialized to 1. Together with the default of `filter_rate_per_sample`, this is equivalent to random sampling new individuals.
#' * `sampling` :: `function`\cr
#'   Function that generates the initial population, as well as new individuals to be filtered from, as a [`Design`][paradox::Design] object. The function must have
#'   arguments `param_set` and `n` and function like [`paradox::generate_design_random`] or [`paradox::generate_design_lhs`].
#'   This is equivalent to the `initializer` parameter of [`mies_init_population()`], see there for more information. Initialized to
#'   [`generate_design_random()`][paradox::generate_design_random].
#'
#' @param surrogate_learner ([`mlr3::LearnerRegr`] | `NULL`)\cr
#'   Regression learner for the surrogate model filtering algorithm. May be `NULL`, in which case no surrogate model is used and individuals are sampled randomly.
#'
#' @family optimizers
#' @examples
#' \donttest{
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
#' # TODO
#' # sumohb_opt <- # TODO
#' # sumohb_opt$optimize performs SumoHB optimization and returns the optimum
#' # sumohb_opt$optimize(oi)
#'
#' #####
#' # Optimizing a Machine Learning Method
#' #####
#'
#' # Note that this is a short example, aiming at clarity and short runtime.
#' # The settings are not optimal for hyperparameter tuning. The resampling
#' # in particular should not be "holdout" for small datasets where this gives
#' # a very noisy estimate of performance.
#'
#' library("mlr3")
#' library("mlr3tuning")
#'
#' # The Learner to optimize
#' learner = lrn("classif.rpart")
#'
#' # The hyperparameters to optimize
#' learner$param_set$values[c("cp", "maxdepth")] = list(to_tune())
#'
#' # Get a TuningInstance
#' ti = TuningInstanceSingleCrit$new(
#'   task = tsk("iris"),
#'   learner = learner,
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.acc"),
#'   terminator = trm("gens", generations = 10)
#' )
#'
#' # sumohb_tune <- # TODO
#' # sumohb_tune$optimize performs SumoHB optimization and returns the optimum
#' # sumohb_tune$optimize(ti)
#' }
#' @export
OptimizerSumoHB = R6Class("OptimizerSumoHB", inherit = Optimizer,
  public = list(
    #' @description
    #' Initialize the 'OptimizerSumoHB' object.
    initialize = function(surrogate_learner = NULL) {
      assert_r6(surrogate_learner, "LearnerRegr", null.ok = TRUE)
      param_set = ps(
        mu = p_int(1, tags = "required"),
        survival_fraction = p_dbl(0, 1, tags = "required"),
        sampling = p_uty(custom_check = function(x) check_function(x, args = c("param_set", "n")), tags = c("init", "required"))
      )
      param_set$values = list(mu = 2, survival_fraction = 0.5, sampling = generate_design_lhs)

      private$.own_param_set = param_set

      if (is.null(surrogate_learner)) {
        private$.filtor = FiltorNull$new()
      } else {
        private$.filtor = FiltorSurrogateProgressive$new(surrogate_learner)
      }

      private$.param_set_source = alits(private$.own_param_set, private$.filtor$param_set)

      can_dependencies = is.null(surrogate_learner) || "missings" %in% surrogate_learner$properties

      super$initialize(
        param_set = self$param_set, param_classes = private$.filtor$param_classes,
        properties = c(if (can_dependencies) "dependencies", "single-crit"),
        packages = c("miesmuschel", "lhs", surrogate_learner$packages)
      )
    }
  ),
  active = list(
    #' @field surrogate_learner ([`mlr3::LearnerRegr`] | `NULL`)\cr
    #' Regression learner for the surrogate model filtering algorithm.
    surrogate_learner = function(rhs) {
      ret = private$.filtor$surrogate_learner
      if (!missing(rhs) && !identical(rhs, ret)) {
        stop("surrogate_learner is read-only.")
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

      generations = terminator_get_generations(inst$terminator)
      if (!is.finite(generations)) {
        stop("Given OptimizationInstance must have a TerminatorGenerations, or a TerminatorCombo with 'any = TRUE' containing at least one TerminatorGenerations (possibly recursively).")
      }
      if (generations < 1) {
        stopf("At least one generation must be evaluated (at full fidelity), but terminator had %s generations.", generations)
      }

      # use rev(seq(upper, lower)), so that with sequence length 1, we get the upper bound only.
      budget_progression = rev(seq(inst$search_space$upper[budget_id], inst$search_space$lower[budget_id], length.out = generations))
      fidelity_schedule = data.table(generation = seq_len(generations), budget_new = budget_progression, budget_survivors = budget_progression)

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

      repeat {
        mies_survival_plus(inst, mu = survivors, survival_selector = survival_selector)
        offspring = mies_generate_offspring(inst, lambda = pre_filter_size, parent_selector = parent_selector, mutator = mutator, budget_id = budget_id)
        offspring = mies_filter_offspring(inst, offspring, lambda, private$.filtor)
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
