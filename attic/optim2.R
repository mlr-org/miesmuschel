library("mlr3learners")
library("paradox")
library("mlr3pipelines")
library("mlr3misc")
loadNamespace("miesmuschel")


# --- possible surrogate learners

imputepl <- po("imputeoor", offset = 1, multiplier = 10) %>>% po("fixfactors") %>>% po("imputesample")
learnerlist <- list(
  ranger = GraphLearner$new(imputepl %>>% mlr3::lrn("regr.ranger", fallback = mlr3::lrn("regr.featureless"), encapsulate = c(train = "evaluate", predict = "evaluate"))),
  knn1 = GraphLearner$new(imputepl %>>% mlr3::lrn("regr.kknn", k = 1, fallback = mlr3::lrn("regr.featureless"), encapsulate = c(train = "evaluate", predict = "evaluate"))),
  knn7 = GraphLearner$new(imputepl %>>% mlr3::lrn("regr.kknn", k = 7, fallback = mlr3::lrn("regr.featureless"), encapsulate = c(train = "evaluate", predict = "evaluate"))),
  bohblrn = GraphLearner$new(
    po("colapply", id = "colapply0", applicator = as.factor, affect_columns = selector_type("character")) %>>%
    po("fixfactors") %>>%
    po("colapply", applicator = as.numeric, affect_columns = selector_type("integer")) %>>%
    po("stratify", predict_choice = "exact_or_less") %>>%
    list(
      po("densitysplit") %>>%
      # would love to use a multiplicity here, but nested mults have problems when the outer one is empty, which can actually happen here.
      list(
          po("removeconstants", id = "goodremoveconstants") %>>%
          po("imputesample", id = "goodimputesample") %>>%
          mlr3::lrn("density.np", id = "gooddensity", bwmethod = "normal-reference-numeric", min_bandwidth = 1e-3),
          po("removeconstants", id = "badremoveconstants") %>>%
          po("imputesample", id = "badimputesample") %>>%
          mlr3::lrn("density.np", id = "baddensity", bwmethod = "normal-reference-numeric", min_bandwidth = 1e-3)
      ) %>>%
      po("densityratio") %>>%
      po("predictionunion", collect_multiplicity = TRUE),
      mlr3::lrn("regr.featureless")
    ) %>>% po("predictionunion", id = "fallback_union")
  )
)

learnerlist$knn7$graph$pipeops$regr.kknn$param_set$context_available = "task"
learnerlist$knn7$param_set$values$regr.kknn.k = ContextPV(function(task) if (task$nrow < 8) stop("need 8 samples") else 7)
learnerlist$bohblrn$graph$pipeops$stratify$param_set$context_available = "inputs"
learnerlist$bohblrn$graph$pipeops$densitysplit$param_set$context_available = "inputs"
learnerlist$bohblrn$graph$pipeops$stratify$param_set$values$min_size = ContextPV(function(inputs) inputs[[1]]$ncol + 2)
learnerlist$bohblrn$graph$pipeops$densitysplit$param_set$values$min_size = ContextPV(function(inputs) inputs[[1]]$ncol + 1)
learnerlist$bohblrn$graph$pipeops$stratify$param_set$values$stratify_feature = ContextPV(function(inputs) stop("needs to be set to the budget param"))

learnerlist <- lapply(learnerlist, function(x) { class(x) <- c("LearnerRegr", class(x)) ; x })

# --- bohb-like sampling

generate_design_bohb = ContextPV(function(inst) function(param_set, n) {
  target = inst$archive$codomain$ids()
  if (!nrow(inst$archive$data)) return(generate_design_random(param_set, n))
  task = TaskRegr$new("archive", inst$archive$data[, c(inst$archive$search_space$ids(), target), with = FALSE], target = target)
  sampler = SamplerKD$new(param_set, task, inst$archive$codomain$tags[[1]] == "minimize", alpha = .15, min_points_in_model = 0, bandwidth_factor = 3, min_bandwidth = 1e-3)
  sampler$sample(n)
})

# --- search space components

suggested_meta_searchspace = ps(
  budget_log_step = p_dbl(log(2) / 4, log(2) * 4, logscale = TRUE),
  survival_fraction = p_dbl(0, 1),  # values close to 1 may fail depending on mu; somehow interpolate that.
  surrogate_learner = p_fct(names(learnerlist)),  # TODO: not numeric yet
  filter_with_max_budget = p_lgl(),
  filter_factor_first = p_dbl(1, 1000, logscale = TRUE),
  random_interleave_fraction = p_dbl(0, 1),
  random_interleave_random = p_lgl()
)

searchspace_component_sample = ps(sample = p_fct(c("random", "bohb")))
searchspace_component_sample_numeric = ps(sample = p_int(1, 2, trafo = function(x) c("random", "bohb")[x]))

searchspace_component_mu = ps(mu = p_int(2, 200, logscale = TRUE))

searchspace_component_batchmethod = ps(batch_method = p_fct(c("smashy", "hb")))
searchspace_component_batchmethod_numeric = ps(batch_method = p_int(1, 2, trafo = function(x) c("smashy", "hb")[x]))

searchspace_component_vario = ps(
  filter_factor_last = p_dbl(1, 1000, logscale = TRUE)
)

searchspace_component_infill = ps(
  filter_algorithm = p_fct(c("tournament", "progressive")),
  filter_select_per_tournament = p_int(1, 10, logscale = TRUE)
)

searchspace_component_infill_numeric = ps(
  filter_algorithm = p_int(1, 2, trafo = function(x) c("tournament", "progressive")[x]),
  filter_select_per_tournament = p_int(1, 10, logscale = TRUE)
)

searchspace_component_siman = ps(
  filter_factor_first.end = p_dbl(1, 1000, logscale = TRUE),
  random_interleave_fraction.end = p_dbl(0, 1)
)

searchspace_component_siman_vario = ps(
  filter_factor_last.end = p_dbl(1, 1000, logscale = TRUE)
)

searchspace_component_siman_infill = ps(
  filter_select_per_tournament.end = p_int(1, 10, logscale = TRUE)
)

searchspace_component_mo = ps(mo_selection_method = p_fct(c("nondom.crowding", "nondom.hvcontrib", "hvimprovement", "amazon")))

# This is probably relatively useless, it would be better to have a surrogate model that does one hot encoding preprocessing
searchspace_component_mo_numeric = ps(
  mo_selection_method.nondom.crowding = p_dbl(0, 1),
  mo_selection_method.nondom.hvcontrib = p_dbl(0, 1),
  mo_selection_method.nondom.hvimprovement = p_dbl(0, 1),
  mo_selection_method.nondom.amazon = p_dbl(0, 1),
  .extra_trafo = function(x, param_set) {
    list(mo_selection_method = c("nondom.crowding", "nondom.hvcontrib", "hvimprovement", "amazon")[which.max(unlist(x))])
  }
)

get_searchspace <- function(include.mu, include.batchmethod, infill, include.siman, include.mo, numeric.only = FALSE) {
  assertFlag(include.mu)
  assertFlag(include.siman)
  assertFlag(include.batchmethod)
  assertChoice(infill, c("rs", "vario", "all"))
  assertFlag(numeric.only)

  pss = list(
    suggested_meta_searchspace,
    if (numeric.only) searchspace_component_sample_numeric else searchspace_component_sample,
    if (include.mu) searchspace_component_mu,
    if (include.batchmethod) {
      if (numeric.only) searchspace_component_batchmethod_numeric else searchspace_component_batchmethod
    },
    if (infill %in% c("vario", "all")) searchspace_component_vario,
    if (infill == "all") {
      if (numeric.only) searchspace_component_infill_numeric else searchspace_component_infill
    },
    if (include.siman) searchspace_component_siman,
    if (include.siman && infill %in% c("vario", "all")) searchspace_component_siman_vario,
    if (include.siman && infill == "all") searchspace_component_siman_infill,
    if (include.mo) {
      if (numeric.only) searchspace_component_mo_numeric else searchspace_component_mo
    }
  )
  miesmuschel:::ps_union(discard(pss, is.null))
}

# --- domain

suggested_meta_domain = ps(
  budget_log_step = p_dbl(log(2) / 4, log(2) * 4),
  survival_fraction = p_dbl(0, 1),  # values close to 1 may fail depending on mu; somehow interpolate that.
  surrogate_learner = p_uty(),
  filter_with_max_budget = p_lgl(),
  filter_factor_first = p_dbl(1, 1000),
  random_interleave_fraction = p_dbl(0, 1),
  random_interleave_random = p_lgl(),
  sample = p_fct(c("random", "bohb")),
  mu = p_int(2, 200),
  batch_method = p_fct(c("smashy", "hb")),
  filter_factor_last = p_dbl(1, 1000),
  filter_algorithm = p_fct(c("tournament", "progressive")),
  filter_select_per_tournament = p_int(1, 10),
  filter_factor_first.end = p_dbl(1, 1000),
  random_interleave_fraction.end = p_dbl(0, 1),
  filter_factor_last.end = p_dbl(1, 1000),
  filter_select_per_tournament.end = p_int(1, 10),
  mo_selection_method = p_fct(c("nondom.crowding", "nondom.hvcontrib", "hvimprovement", "amazon"))
)

# --- ContextPV helpers

# for simulated annealing
get_progress <- function(inst) {
  prog = inst$terminator$status(inst$archive)
  if (prog["max_steps"] <= 0) return(1)
  min(1, max(0, prog["current_steps"] / prog["max_steps"]))
}

# interpolate context param value
interpolate_cpv <- function(beginning, end, logscale = FALSE, round = FALSE) {
  ContextPV(function(inst) {
    prog = get_progress(inst)
    result = if (logscale) {
      exp(prog * log(end) + (1 - prog) * log(beginning))
    } else {
      prog * end + (1 - prog) * beginning
    }
    if (round) result = round(result)
    result
  }, beginning, end, get_progress, logscale, round)
}

# --- objective construction

setup_smashy <- function(search_space, budget_log_step,
    survival_fraction, mu, sample, batch_method,
    filter_algorithm = "tournament", surrogate_learner, filter_with_max_budget,
    filter_factor_first, filter_factor_last = filter_factor_first, filter_select_per_tournament = 1, random_interleave_fraction,
    filter_factor_first.end = filter_factor_first, filter_factor_last.end = filter_factor_last,
    filter_select_per_tournament.end = filter_select_per_tournament, random_interleave_fraction.end = random_interleave_fraction,
    random_interleave_random,
    multiobjective = FALSE, mo_selection_method = NULL, highest_budget_only = TRUE, mo_nadir = 0) {
  library("checkmate")
  library("miesmuschel")
  library("mlr3learners")


  # Objective Parameters
  assert_r6(search_space, "ParamSet")  # search space, has one parameter tagged 'budget', with *** 'logscale = TRUE' ***

  # HB Parameters
  assert_number(budget_log_step, lower = 0)  # log() of budget fidelity steps to make. E.g. log(2) for doubling
  assert_int(mu, lower = 2)  # population size
  assert_number(survival_fraction, lower = 0, upper = 1)  # fraction of individuals that survive. round(mu * survival_fraction) must be < survival_fraction, but we just clip because we don't want to crash out of nowhere.
  assert_choice(sample, c("random", "bohb"))  # sample points randomly or using BOHB's mechanism.
  assert_choice(batch_method, c("smashy", "hb"))  # whether to use synchronized batches, or generalized hyperband
  # (for true hyperband (up to rounding), set `budget_log_step` to `-log(survival_fraction)` and `mu` to `survival_fraction ^ -fidelity_steps` (note fidelity_steps is 0-based))

  # Surrogate Options
  assert_choice(filter_algorithm, c("tournament", "progressive"))  # The two implemented filter algorithms
  assert_choice(surrogate_learner, names(learnerlist))


  #  assert_r6(surrogate_learner, "Learner")
  # Whether to use surrogate predictions at the largest budget so far evaluated, or at the budget of the last evaluated budget.
  # (This only makes a difference after HB "restarts", i.e. when max-budget configs were already evaluated and HB samples new low-budget individuals.)
  assert_flag(filter_with_max_budget)
  # How big is the pool from which the first individual / of the last individual is sampled from? (Relative to select_per_tournament)
  assert_number(filter_factor_first, lower = 1)
  assert_number(filter_factor_first.end, lower = 1)
  assert_number(filter_factor_last, lower = 1)
  assert_number(filter_factor_last.end, lower = 1)
  assert_int(filter_select_per_tournament, lower = 1)  # tournament size, only really used if `filter_algorithm` is "tournament"
  assert_int(filter_select_per_tournament.end, lower = 1)

  assert_number(random_interleave_fraction, lower = 0, upper = 1)  # fraction of individuals sampled with random interleaving
  assert_number(random_interleave_fraction.end, lower = 0, upper = 1)  # fraction of individuals sampled with random interleaving
  assert_flag(random_interleave_random)  # whether the number of random interleaved individuals is drawn from a binomial distribution, or the same each generation

  assert_flag(multiobjective)
  assert_choice(mo_selection_method, c("nondom.crowding", "nondom.hvcontrib", "hvimprovement", "amazon"), null.ok = TRUE)
  assert_flag(highest_budget_only)  # relevant for MO-evaluation
  assert_numeric(mo_nadir, any.missing = FALSE, finite = TRUE)


  # We change the lower limit of the budget parameter:
  # suppose: budget_step is 2, budget param goes from 1 to 6
  # we want steps of length 2, and highest step should be 6, so we want to eval with 6, 4, 2
  # --> there are 2 budget_steps. lower bound needs to be adjusted to 6 - 2 (# of budget steps) * 2 (budget step size) --> 2
  budget_param = search_space$ids(tags = "budget")
  fidelity_steps = floor((search_space$upper[budget_param] - search_space$lower[budget_param]) / budget_log_step)
  search_space$params[[budget_param]]$lower = search_space$upper[budget_param] - fidelity_steps * budget_log_step

  learnerlist$bohblrn$graph$pipeops$stratify$param_set$values$stratify_feature = budget_param
  surrogate_learner = learnerlist[[surrogate_learner]]

  survivors = max(round(survival_fraction * mu), 1)
  lambda = mu - survivors
  if (lambda < 1) {
    # return("infeasible: no new samples per generation")
    survival_fraction <- 1 - 1 / mu
  }

  additional_component_sampler = NULL

  if (is.null(mo_selection_method)) {
    # scalor: scalarizes multi-objective results. "one": take the single objective.
    scalor = scl("one")
  } else {
    selector = switch(mo_selection_method,
      nondom.crowding = scl("nondom", tiebreak = "crowdingdist"),  # "nondom": nondominated sorting w/ crowding distance tie breaker
      nondom.hvcontrib = scl("nondom", tiebreak = "hvcontrib", nadir = mo_nadir),  # hv contribution tie breaker
      hvimprovement = scl("hypervolume", nadir = mo_nadir, baseline = ContextPV(function(inst) {
        if (!nrow(inst$archive$data)) return(NULL)
        if (highest_budget_only) {
          budget_id = inst$search_space$ids(tags = "budget")
          budgetvals = inst$archive$data[[budget_id]]
          mies_get_fitnesses(inst, budgetvals == max(budgetvals))
        } else {
          mies_get_fitnesses(inst)
        }
      }, highest_budget_only)),
      amazon = {
        additional_component_sampler = ContextPV(function(inst) SamplerRandomWeights(nobjectives = inst$objective$codomain$length, nweights = 100))
        scl("fixedprojection", scalarization = scalarizer_linear())
      }
    )
  }
  # selector: take the best, according to scalarized objective
  selector = sel("best", scalor)

  # filtor: use surtour or surprog, depending on filter_algorithm config argument
  filtor = switch(filter_algorithm,
    tournament = ftr("surtour", surrogate_learner = surrogate_learner, surrogate_selector = selector,
      filter.per_tournament = interpolate_cpv(filter_select_per_tournament, filter_select_per_tournament.end, logscale = TRUE, round = TRUE),
      filter.tournament_size = interpolate_cpv(
        filter_factor_first * filter_select_per_tournament,
        filter_factor_first.end * filter_select_per_tournament.end,
        logscale = TRUE
      ),
      filter.tournament_size_last = interpolate_cpv(
        filter_factor_last * filter_select_per_tournament,
        filter_factor_last.end * filter_select_per_tournament.end,
        logscale = TRUE
      )
    ),
    progressive = ftr("surprog", surrogate_learner = surrogate_learner, surrogate_selector = selector,
      filter.pool_factor = interpolate_cpv(filter_factor_first, filter_factor_first.end, logscale = TRUE),
      filter.pool_factor_last = interpolate_cpv(filter_factor_last, filter_factor_last.end, logscale = TRUE)
    )
  )

  random_interleave_fraction_cpv  = interpolate_cpv(1 - random_interleave_fraction, 1 - random_interleave_fraction.end)  # linear scale

  interleaving_filtor = ftr("maybe", filtor, p = random_interleave_fraction_cpv, random_choice = random_interleave_random)

  sampling_fun = switch(sample, random = paradox::generate_design_random, bohb = generate_design_bohb)

  optimizer = bbotk::opt("smashy", filtor = interleaving_filtor, selector = selector,
    mu = mu, survival_fraction = survival_fraction,
    fidelity_steps = fidelity_steps + 1, synchronize_batches = batch_method == "smashy",
    filter_with_max_budget = filter_with_max_budget
  )

  optimizer$.__enclos_env__$private$.own_param_set$context_available = "inst"
  optimizer$param_set$values$sampling = sampling_fun
  optimizer$param_set$values$additional_component_sampler = additional_component_sampler

  optimizer
}

setup_oi <- function(objective, budget_limit, search_space) {
  assert_r6(objective, "Objective")  # to optimize, single- or multi-objective
  assert_number(budget_limit, lower = 0)  # Total 'budget' to optimize. Not log-transformed.
  oiclass = if (objective$codomain$length == 1) bbotk::OptimInstanceSingleCrit else bbotk::OptimInstanceMultiCrit
  oiclass$new(objective, search_space,
    terminator = bbotk::trm("budget", budget = budget_limit, aggregate = function(x) sum(exp(as.numeric(x))))  # budget in archive is in log-scale!
  )
}

get_meta_objective <- function(objective, test_objective, search_space, budget_limit, highest_budget_only = TRUE, nadir = 0) {

  multiobjective <- objective$codomain$length > 1

  function(...) {

    oi <- setup_oi(objective, budget_limit, search_space)

    optimizer <- setup_smashy(search_space, ..., multiobjective = multiobjective, highest_budget_only = highest_budget_only, mo_nadir = nadir)

    optimizer$optimize(oi)

    om <- oi$objective_multiplicator

    archdata <- oi$archive$data
    budgetparam <- oi$search_space$ids(tags = "budget")
    if (highest_budget_only) {
      archdata <- archdata[get(budgetparam) == max(get(budgetparam))]
    }

    objvalues <- archdata[, names(om), with = FALSE]
    objmat <- as.matrix(sweep(objvalues, 2, om, `*`)) * -1

    ndo <- miesmuschel::rank_nondominated(objmat)$fronts
    selarch <- ndo == 1

    if (!multiobjective) {
      selarch <- which(selarch)[[1]]
    }

    design <- archdata[selarch, x_domain]

    fitnesses <- as.matrix(sweep(test_objective$eval_many(design)[, test_objective$codomain$ids(), with = FALSE], 2, om, `*`)) * -1

    if (multiobjective) {
      miesmuschel:::domhv(fitnesses, nadir = nadir)
    } else {
      fitnesses
    }
  }
}

prepare_surrogate <- function(surrogate, budgetfactor) {

  budget_id <- surrogate$domain$ids(tags = "budget")
  fulleval_equivalents <- budgetfactor * surrogate$domain$length
  budget_limit <- fulleval_equivalents * surrogate$domain$upper[[budget_id]]


  prevtrafo <- surrogate$domain$trafo

  search_space <- surrogate$domain$clone(deep = TRUE)
  id_order <- search_space$ids()

  proto_dt <- generate_design_random(search_space, 1)$data  # a bit hacky; make sure missings have the right type TODO: maybe not necessary any more

  if (surrogate$codomain$ids() == "val_cross_entropy") {
    # lcbench special code: fix missings and round budget

    budgetlower <- search_space$params[[budget_id]]$lower
    budgetupper <- search_space$params[[budget_id]]$upper

    search_space$.__enclos_env__$private$.params[[budget_id]] <- ParamDbl$new(budget_id,
      lower = log(search_space$params[[budget_id]]$lower),
      upper = log(search_space$params[[budget_id]]$upper),
      tags = "budget"
    )

    search_space$trafo <- mlr3misc::crate(function(x, param_set) {
      x <- prevtrafo(x)
      x[[budget_id]] <- max(min(round(exp(x[[budget_id]])), budgetupper), budgetlower)
      rbind(proto_dt, x, fill = TRUE)[2]
    }, prevtrafo, budget_id, proto_dt, budgetupper, budgetlower)

  } else {
    # randombot special code: fix missings only
    search_space$params[["trainsize"]]$lower <- 3^-3

    search_space$params[[budget_id]]$lower <- log(search_space$params[[budget_id]]$lower)
    search_space$params[[budget_id]]$upper <- log(search_space$params[[budget_id]]$upper)

    surrogate$trafo_dict$logloss$retrafo <- function(x) { ret <- -log(x) ; ret[ret > 1e20] <- 1e20 ; ret }  # way faster than pmin
    search_space$trafo <- mlr3misc::crate(function(x, param_set) {
      x <- prevtrafo(x)
      x[[budget_id]] <- exp(x[[budget_id]])
      rbind(proto_dt, x, fill = TRUE)[2]
    }, prevtrafo, budget_id, proto_dt)
  }
  list(surrogate = surrogate, search_space = search_space, budget_limit = budget_limit)
}

get_meta_objective_from_surrogate <- function(surrogate, budgetfactor, highest_budget_only = TRUE) {
  x <- prepare_surrogate(surrogate, budgetfactor)
  get_meta_objective(x$surrogate, x$surrogate, x$search_space, budget_limit = x$budget_limit, highest_budget_only = highest_budget_only)
}

optimize_from_surrogate <- function(surrogate, budgetfactor, nadir = 0) {
  x <- prepare_surrogate(surrogate, budgetfactor)
  multiobjective <- x$surrogate$codomain$length > 1
  function(...) {
    oi <- setup_oi(x$surrogate, x$budget_limit, x$search_space)
    optimizer <- setup_smashy(x$search_space, ..., multiobjective = multiobjective, mo_nadir = nadir)
    optimizer$optimize(oi)
    oi
  }
}

imitate_hyperband <- function(search_space, eta = 3, budget_is_logscale = FALSE) {
  budget_param = search_space$ids(tags = "budget")
  trafo = if (budget_is_logscale) identity else log
  fidelity_steps = floor((trafo(search_space$upper[budget_param]) - trafo(search_space$lower[budget_param])) / log(eta))

  list(
    budget_log_step = log(eta),
    survival_fraction = 1 / eta,
    mu = eta ^ fidelity_steps,
    sample = "random",
    batch_method = "hb",
    random_interleave_fraction = 1,
   ## - mandatory arguments that don't have an effect with interleave fraction == 1
    filter_algorithm = "tournament",  # doesn't matter
    surrogate_learner = "ranger",  # doesn't matter
    filter_with_max_budget = FALSE,  # doesn't matter
    filter_factor_first = 1,  # doesn't matter
    random_interleave_random = FALSE  # doesn't matter
  )
}

imitate_bohb <- function(search_space, eta = 3, rho = 1 / 3, ns = 64, budget_is_logscale = FALSE) {

  budget_param = search_space$ids(tags = "budget")
  trafo = if (budget_is_logscale) identity else log
  fidelity_steps = floor((trafo(search_space$upper[budget_param]) - trafo(search_space$lower[budget_param])) / log(eta))

  list(
    budget_log_step = log(eta),
    survival_fraction = 1 / eta,
    mu = eta ^ fidelity_steps,
    sample = "bohb",
    batch_method = "hb",
    random_interleave_fraction = rho,
    filter_algorithm = "tournament",
    surrogate_learner = "bohblrn",
    filter_with_max_budget = TRUE,
    filter_factor_first = ns,
    random_interleave_random = TRUE
  )
}
