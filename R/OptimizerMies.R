#' @title Mixed Integer Evolutionary Strategies Optimizer
#'
#' @include Selector.R
#' @include Mutator.R
#' @include Recombinator.R
#' @include mies_methods.R
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
#' 2. Initialize and evaluate population, using `mies_init_population()`
#' 3. Optionally, evaluate survivors with higher fidelity if the multi-fidelity functionality is being used
#' 4. Generate offspring by selecting parents, recombining and mutating them, using `mies_generate_offspring()`
#' 5. Evaluate performance, using `mies_evaluate_offspring()`
#' 6. Select survivors, using either `mies_survival_plus()` or `mies_survival_comma()`, depending on the `survival_strategy` configuration parameter
#' 7. Jump to 3.
#'
#' @section Terminating:
#' As with all optimizers, [`Terminator`][bbotk::Terminator]s are used to end optimization after a specific number of evaluations were performed,
#' time elapsed, or other conditions are satisfied. Of particular interest is [`TerminatorGenerations`], which terminates after a number
#' of generations were evaluated in `OptimizerMies`. The initial population counts as generation 1, its offspring as generation 2 etc.;
#' fidelity refinements are always included in their generation, [`TerminatorGenerations`] avoids terminating right before they are evaluated.
#' Other terminators may, however, end the optimization process at any time.
#'
#' @section Multi-Fidelity:
#' `miesmuschel` provides a simple multi-fidelity optimization mechanism that allows increasing fidelity both by generation
#' number and survival status. When `multi_fidelity` is `TRUE`, then one search space component of the [`OptimInstance`][bbotk::OptimInstance]
#' must have the `"budget"` tag, which is then optimized as the "budget" component. This means that the value of this component is
#' determined by the `fidelity_schedule` configuration parameter, which must contain a `data.frame` with columns `"generation"`, `"budget_new"` and
#' `"budget_survivors"`. The budget component's value of newly sampled individuals is set to the `"budget_new"` entry in the generation's row,
#' and surviving individuals are evaluated again with the budget set to the `"budget_survivors"` value (unless they are the same). At the
#' end of a generation, if the `"budget_survivors"` value changes, all individuals from previous generations are re-evaluated with the
#' new budget value, unless `fidelity_current_gen_only` is set to `TRUE` (and unless the budget value decreases and `fidelity_monotonic` is `TRUE`).
#' This makes it possible to implement increasing fidelity by generation, and also increasing fidelity for samples that survived a generation.
#'
#' The `fidelity_schedule` configuration parameter's `"generation"` column determines which row is currently active. A row becomes active in the
#' generation that is listed, and becomes inactive whenever a different row becomes active. So e.g. if `fidelity_schedule` contains a row
#' with `"generation"` set to 1, and one set to 4, then the first row is active during generations 1, 2, and 3, and the second row
#' is active for all following generations.
#'
#' Note that the multifidelity functionality is experimental and the UI may change in the future.
#'
#' @section Additional Components:
#' The search space over which the optimization is performed is fundamentally tied to the [`Objective`][bbotk::Objective], and therefore
#' to the [`OptimInstance`][bbotk::OptimInstance] given to `OptimizerMies$optimize()`. However, some advanced Evolutionary Strategy based
#' algorithms may need to make use of additional search space components that are independent of the particular objective. An example is
#' self-adaption as implemented in [`OperatorCombination`], where one or several components can be used to adjust operator behaviour.
#' These additional components are supplied to the optimizer through the `additional_component_sampler` configuration parameter, which takes
#' a [`Sampler`][paradox::Sampler] object. This object both has an associated [`ParamSet`][paradox::ParamSet] which represents the
#' additional components that are present, and it provides a method for generating the initial values of these components. The search space
#' that is seen by the [`MiesOperator`]s is then the union of the [`OptimInstance`]'s [`ParamSet`][paradox::ParamSet], and the
#' [`Sampler`][paradox::Sampler]'s [`ParamSet`][paradox::ParamSet].
#'
#' @section Configuration Parameters:
#' `OptimizerMies` has the configuration parameters of the `mutator`, `recombinator`, `parent_selector`, `survival_selector`, and, if given,
#' `elite_selector` operator given during construction, and prefixed according to the name of the argument (`mutator`'s configuration parameters
#' are prefixed `"mutator."` etc.). When using the construction arguments' default values, they are all "proxy" operators: [`MutatorProxy`],
#' [`RecombinatorProxy`] and [`SelectorProxy`]. This means that the respective configuration parameters become `mutator.operation`, `recombinator.operation` etc.,
#' so the operators themselves can be set via configuration parameters in this case.
#'
#' Further configuration parameters are:
#' * `lambda` :: `integer(1)`\cr
#'   Offspring size: Number of individuals that are created and evaluated anew for each generation. This is equivalent to the
#'   `lambda` parameter of [`mies_generate_offspring()`], see there for more information. Initialized to 10.
#' * `mu` :: `integer(1)`\cr
#'   Population size: Number of individuals that are sampled in the beginning, and which are selected with each survival step.
#'   This is equivalent to the `mu` parameter of [`mies_init_population()`], see there for more information. Initialized to 1.
#' * `survival_strategy` :: `character(1)`\cr
#'   May be `"plus"`, or, if the `elite_selector` construction argument is not `NULL`, `"comma"`: Choose whether [`mies_survival_plus()`]
#'   or [`mies_survival_comma()`] is used for survival selection. Initialized to `"plus"`.
#' * `n_elite` :: `integer(1)`\cr
#'   Only if the `elite_selector` construction argument is not `NULL`, and only valid when `survival_strategy` is `"comma"`:
#'   Number of elites, i.e. individuals from the parent generation, to keep during "Comma" survival.
#'   This is equivalent to the `n_elite` parameter of [`mies_survival_comma()`], see there for more information.
#' * `initializer` :: `function`\cr
#'   Function that generates the initial population as a [`Design`][paradox::Design] object,
#'   with arguments `param_set` and `n`, functioning like [`paradox::generate_design_random`] or [`paradox::generate_design_lhs`].
#'   This is equivalent to the `initializer` parameter of [`mies_init_population()`], see there for more information. Initialized to
#'   [`generate_design_random()`][paradox::generate_design_random].
#' * `additional_component_sampler` :: [`Sampler`][paradox::Sampler] | `NULL`\cr
#'   Additional components that may be part of individuals as seen by mutation, recombination, and selection [`MiesOperator`]s, but
#'   that are not part of the search space of the [`OptimInstance`][bbotk::OptimInstance] being optimized.
#'   This is equivalent to the `additional_component_sampler` parameter of [`mies_init_population()`], see there for more information.
#'   Initialized to `NULL` (no additional components).
#' * `fidelity_schedule` :: `data.frame`\cr
#'   Only if the `multi_fidelity` construction argument is `TRUE`:
#'   Table that determines the value of the "budget" component of individuals being evaluated when doing multi-fidelity optimization.
#'   This is equivalent to the `fidelity_schedule` parameter of [`mies_init_population()`], [`mies_evaluate_offspring()`], and [`mies_step_fidelity()`];
#'   see there for more information.\cr
#'   When this configuration parameter is present (i.e. `multi_fidelity` is `TRUE`), then it is initialized to a `data.frame` containing one row
#'   for generation 1, setting budget to 1 for both new and survivor individuals.\cr
#'   Note that the multifidelity functionality is experimental and the UI may change in the future.
#' * `fidelity_generation_lookahead` :: `logical(1)`\cr
#'   Only if the `multi_fidelity` construction argument is `TRUE`:
#'   Whether to use the `"survivor_budget"` of the *next* generation, instead of the *current* generation, when doing fidelity refinement
#'   in [`mies_step_fidelity()`].
#'   This is equivalent to the `generation_lookahead` parameter of [`mies_step_fidelity()`], see there for more information.\cr
#'   When this configuration parameter is present (i.e. `multi_fidelity` is `TRUE`), then it is initialized to `TRUE`.\cr
#'   Note that the multifidelity functionality is experimental and the UI may change in the future.
#' * `fidelity_current_gen_only` :: `logical(1)`\cr
#'   Only if the `multi_fidelity` construction argument is `TRUE`:
#'   When doing fidelity refinement in [`mies_step_fidelity()`], whether to refine all individuals with different budget component,
#'   or only individuals created in the current generation.
#'   This is equivalent to the `current_gen_only` parameter of [`mies_step_fidelity()`], see there for more information.\cr
#'   When this configuration parameter is present (i.e. `multi_fidelity` is `TRUE`), then it is initialized to `FALSE`.\cr
#'   Note that the multifidelity functionality is experimental and the UI may change in the future.
#' * `fidelity_monotonic` :: `logical(1)`\cr
#'   Only if the `multi_fidelity` construction argument is `TRUE`:
#'   Whether to only do fidelity refinement in [`mies_step_fidelity()`] for individuals for which the when budget component value would *increase*.
#'   This is equivalent to the `monotonic` parameter of [`mies_step_fidelity()`], see there for more information.\cr
#'   When this configuration parameter is present (i.e. `multi_fidelity` is `TRUE`), then it is initialized to `TRUE`.\cr
#'   Note that the multifidelity functionality is experimental and the UI may change in the future.
#'
#' @param mutator ([`Mutator`])\cr
#'   Mutation operation to perform during [`mies_generate_offspring()`], see there for more information. Default is [`MutatorProxy`], which
#'   exposes the operation as a configuration parameter of the optimizer itself.\cr
#'   The `$mutator` field will reflect this value.
#' @param recombinator ([`Recombinator`])\cr
#'   Recombination operation to perform during [`mies_generate_offspring()`], see there for more information. Default is [`RecombinatorProxy`],
#'   which exposes the operation as a configuration parameter of the optimizer itself. Note: The default [`RecombinatorProxy`] has `$n_indivs_in` set to 2,
#'   so to use recombination operations with more than two inputs, or to use population size of 1, it may be necessary to construct this
#'   argument explicitly.\cr
#'   The `$recombinator` field will reflect this value.
#' @param parent_selector ([`Selector`])\cr
#'   Parent selection operation to perform during [`mies_generate_offspring()`], see there for more information. Default is [`SelectorProxy`],
#'   which exposes the operation as a configuration parameter of the optimizer itself.\cr
#'   The `$parent_selector` field will reflect this value.
#' @param survival_selector ([`Selector`])\cr
#'   Survival selection operation to use in [`mies_survival_plus()`] or [`mies_survival_comma()`] (depending on the `survival_strategy` configuration parameter),
#'   see there for more information. Default is [`SelectorProxy`], which exposes the operation as a configuration parameter of the optimizer itself.\cr
#'   The `$survival_selector` field will reflect this value.
#' @param elite_selector ([`Selector`] | `NULL`)\cr
#'   Elite selector used in [`mies_survival_comma()`], see there for more information. "Comma" selection is only available when this
#'   argument is not `NULL`. Default `NULL`.\cr
#'   The `$elite_selector` field will reflect this value.
#' @param multi_fidelity (`logical(1)`)\cr
#'   Whether to enable multi-fidelity optimization. When this is `TRUE`, then the [`OptimInstance`][bbotk::OptimInstance] being optimized must
#'   contain a [`Param`][paradox::Param] tagged `"budget"`, which is then used as the "budget" search space component, determined by
#'   `fidelity_schedule` instead of by the [`MiesOperator`]s themselves. For multi-fidelity optimization, the `fidelity_schedule`,
#'   `fidelity_generation_lookahead`, `fidelity_current_gen_only`, and `fidelity_monotonic` configuration parameters must be given to determine
#'   multi-fidelity behaviour. (While the initial values for most of these are probably good for most cases in which more budget implies
#'   higher fidelity, the `fidelity_schedule` configuration parameter should be adjusted in most cases). Default is `FALSE`.
#' @references
#' `r format_bib("fieldsend2014rolling")`
#'
#' `r format_bib("li2013mixed")`
#'
#' @family optimizers
#' @examples
#' \donttest{
#' lgr::threshold("warn")
#'
#' op.m <- mut("gauss")
#' op.r <- rec("xounif", p = .3)
#' op.parent <- sel("random")
#' op.survival <- sel("best")
#'
#' #####
#' # Optimizing a Function
#' #####
#'
#' library("bbotk")
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
#' # Create OptimizerMies object
#' mies_opt <- opt("mies", mutator = op.m, recombinator = op.r,
#'   parent_selector = op.parent, survival_selector = op.survival,
#'   mu = 10, lambda = 5)
#'
#' # mies_opt$optimize performs MIES optimization and returns the optimum
#' mies_opt$optimize(oi)
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
#' # Create TunerMies object
#' mies_tune <- tnr("mies", mutator = op.m, recombinator = op.r,
#'   parent_selector = op.parent, survival_selector = op.survival,
#'   mu = 10, lambda = 5)
#'
#' # mies_tune$optimize performs MIES optimization and returns the optimum
#' mies_tune$optimize(ti)
#' }
#' @export
OptimizerMies = R6Class("OptimizerMies", inherit = Optimizer,
  public = list(
    #' @description
    #' Initialize the `OptimizerMies` object.
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
      commareq = quote(survival_strategy == "comma")  # TODO: put back when paradox 0.8 is up
      private$.own_param_set = do.call(ps, c(list(
          lambda = p_int(1, tags = c("required", "offspring")),
          mu = p_int(1, tags = c("required", "init", "survival")),
          survival_strategy = p_fct(c("plus", if (!is.null(elite_selector)) "comma"), tags = "required")),
        if (!is.null(elite_selector)) list(
          n_elite = p_int(0, depends = commareq, tags = "survival")),
        list(
          initializer = p_uty(custom_check = function(x) check_function(x, args = c("param_set", "n")), tags = c("init", "required")),  # arguments: param_set, n
          additional_component_sampler = p_uty(custom_check = function(x) if (is.null(x)) TRUE else check_r6(x, "Sampler"))),
        if (multi_fidelity) list(
          fidelity_schedule = p_uty(custom_check = check_fidelity_schedule, tags = "required"),
          fidelity_generation_lookahead = p_lgl(tags = "required"),
          fidelity_current_gen_only = p_lgl(tags = "required"),
          fidelity_monotonic = p_lgl(tags = "required"))
      ))

      self$mutator$param_set$set_id = "mutator"
      self$recombinator$param_set$set_id = "recombinator"
      self$parent_selector$param_set$set_id = "parent_selector"
      self$survival_selector$param_set$set_id = "survival_selector"
      if (!is.null(elite_selector)) self$elite_selector$param_set$set_id = "elite_selector"
      private$.param_set_source = c(alist(private$.own_param_set, self$mutator$param_set, self$recombinator$param_set,
        self$parent_selector$param_set, self$survival_selector$param_set), if (!is.null(elite_selector)) alist(self$elite_selector$param_set))

      self$param_set$values = insert_named(self$param_set$values, c(
        list(lambda = 10, mu = 1, initializer = generate_design_random, survival_strategy = "plus"),
        if (multi_fidelity) list(
          fidelity_schedule = data.frame(generation = 1, budget_new = 1, budget_survivors = 1),
          fidelity_generation_lookahead = TRUE, fidelity_current_gen_only = FALSE, fidelity_monotonic = TRUE)
      ))

      param_class_determinants = c(
        list(parent_selector, survival_selector),
        if (!is.null(elite_selector)) list(elite_selector),

        # don't depend on mutate and recombine when we do multi-fidelity because budget may be any unsupported Param.
        if (!multi_fidelity) list(mutator, recombinator)
      )

      properties_determinants = discard(list(parent_selector, survival_selector, elite_selector), is.null)

      super$initialize(
        id = "mies",
        param_set = self$param_set,  # essentially a nop, since at this point we already set private$.param_set, but we can't give NULL here.
        param_classes = Reduce(intersect, map(param_class_determinants, "param_classes")),
        properties = c("dependencies", Reduce(intersect, map(properties_determinants, "supported"))),
        packages = "miesmuschel"
      )
    }
  ),
  active = list(
    #' @field mutator ([`Mutator`])\cr
    #' Mutation operation to perform during [`mies_generate_offspring()`].
    mutator = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.mutator)) {
        stop("mutator is read-only.")
      }
      private$.mutator
    },
    #' @field recombinator ([`Recombinator`])\cr
    #' Recombination operation to perform during [`mies_generate_offspring()`].
    recombinator = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.recombinator)) {
        stop("recombinator is read-only.")
      }
      private$.recombinator
    },
    #' @field parent_selector ([`Selector`])\cr
    #' Parent selection operation to perform during [`mies_generate_offspring()`].
    parent_selector = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.parent_selector)) {
        stop("parent_selector is read-only.")
      }
      private$.parent_selector
    },
    #' @field survival_selector ([`Selector`])\cr
    #' Survival selection operation to use in [`mies_survival_plus()`] or [`mies_survival_comma()`].
    survival_selector = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.survival_selector)) {
        stop("survival_selector is read-only.")
      }
      private$.survival_selector
    },
    #' @field elite_selector ([`Selector`] | `NULL`)\cr
    #' Elite selector used in [`mies_survival_comma()`].
    elite_selector = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.elite_selector)) {
        stop("elite_selector is read-only.")
      }
      private$.elite_selector
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
      params = private$.own_param_set$get_values()

      budget_id = NULL
      if (!is.null(params$fidelity_schedule)) {
        budget_id = inst$search_space$ids(tags = "budget")
        if (length(budget_id) != 1) stopf("Need exactly one budget parameter for multifidelity, but found %s: %s",
          length(budget_id), str_collapse(budget_id))
      }

      additional_components = params$additional_component_sampler$param_set
      mies_prime_operators(mutators = list(self$mutator), recombinators = list(self$recombinator),
        selectors = discard(list(self$survival_selector, self$parent_selector, self$elite_selector), is.null),
        search_space = inst$search_space, additional_components = additional_components,
        budget_id = budget_id)

      mies_init_population(inst, mu = params$mu, initializer = params$initializer, fidelity_schedule = params$fidelity_schedule,
        budget_id = budget_id, additional_component_sampler = params$additional_component_sampler)

      survival = switch(params$survival_strategy,
          plus = mies_survival_plus,
          comma = mies_survival_comma)

      repeat {
        if (!is.null(params$fidelity_schedule)) {
          mies_step_fidelity(inst, params$fidelity_schedule, budget_id, generation_lookahead = params$fidelity_generation_lookahead,
            current_gen_only = params$fidelity_current_gen_only, monotonic = params$fidelity_monotonic,
            additional_components = additional_components)
        }
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
    .param_set_source = NULL
  )
)

