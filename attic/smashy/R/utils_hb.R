
#' @title Create Generalized Hyperband Survivors Table
#'
#' @description
#' Create a table listing the number of alive individuals at each stage of an SH bracket, as well
#' as if evaluation is necessary. Evaluation is not necessary if no individual needs to be killed at the
#' end of a stage (except at the last stage).
#'
#' @param mu (`integer(1)`)\cr
#'   Size of first generation
#' @param survival_fraction (`numeric(1)`)\cr
#'   Fraction of generation that gets killed.
#' @param num_stages (`numeric`)\cr
#'   budget steps at which individuals get evaluated
#' @return `data.table` with columns `survivors` (`integer`): number of individuals alive at the start
#' of the stage, and `evaluate` (`logical`): whether evaluation is necessary because individuals need
#' to be killed.
hb_survivors_info = function(mu, survival_fraction, num_stages) {
  assertInt(mu, lower = 1, tol = 1e-100)
  assertNumber(survival_fraction, lower = 0, upper = 1)
  assertInt(num_stages, lower = 1, tol = 1e-100)
  survivors = pmax(round(mu * survival_fraction ^ (seq_len(num_stages) - 1)), 1)
  data.table(
    survivors = survivors,
    evaluate = diff(c(survivors, 0)) < 0  # if number of survivors does not decrease in a round, don't eval.
  )
}

#' @title Calculate Generalized Hyperband Bracket Budget
#'
#' @description
#' Budget needed for evaluation of a single extended successive halving.
#'
#' @param mu (`integer(1)`)\cr
#'   Size of first generation
#' @param survival_fraction (`numeric(1)`)\cr
#'   Fraction of generation that gets killed.
#' @param budgets (`numeric`)\cr
#'   budget steps at which individuals get evaluated
#' @return `numeric(1)` total SH budget.
hb_bracket_budget = function(mu, survival_fraction, budgets) {
  assertInt(mu, lower = 1, tol = 1e-100)
  assertNumber(survival_fraction, lower = 0, upper = 1)
  assertNumeric(budgets, min.len = 1, any.missing = FALSE, finite = TRUE, lower = 0)

  hb_survivors_info(mu, survival_fraction, length(budgets))[, sum((budgets * survivors)[evaluate])]
}


#' @title Create Generalized Hyperband Evaluation Plan
#'
#' @description
#' Create a `data.table` detailing the number and fidelities of evaluations to perform.
#'
#' @param mu (`integer(1)`)\cr
#'   Size of first generation
#' @param survival_fraction (`numeric(1)`)\cr
#'   Fraction of generation that gets killed.
#' @param fidelity_lower (`numeric(1)`)\cr
#'   Lower bound of fidelity parameter
#' @param fidelity_upper (`numeric(1)`)\cr
#'   Upper bound of fidelity parameter
#' @param fidelity_steps (`integer(1)`)\cr
#'   Max number of fidelity steps, at least 1.
#' @param fidelity_budget_map (`function`)\cr
#'   Function that maps fidelity param to budget expenditure. Often this is `exp()`
#' @return `data.table` with columns `fidelity` (`numeric`): fidelity component value to evaluateat,
#'`sample_new` (`integer`) number of individuals to sample anew, and
#' `survivors` (`integer`): number of individuals kept alive from previous stage.
hb_evaluation_schedule = function(mu, survival_fraction, fidelity_lower, fidelity_upper, fidelity_steps, fidelity_budget_map = exp) {
  assertInt(mu, lower = 1, tol = 1e-100)
  assertNumber(survival_fraction, lower = 0, upper = 1)
  assertNumber(fidelity_lower, finite = TRUE)
  assertNumber(fidelity_upper, finite = TRUE)
  assertInt(fidelity_steps, lower = 1, tol = 1e-100)
  assertFunction(fidelity_budget_map)

  # use rev(seq(upper, lower)), so that with sequence length 1, we get the upper bound only.
  fidelities = rev(seq(fidelity_upper, fidelity_lower, length.out = fidelity_steps))
  budgets = assertNumeric(map_dbl(fidelities, fidelity_budget_map), any.missing = FALSE, finite = TRUE, lower = 0)
  assertNumeric(diff(budgets), lower = 0)  # need strictly increasing budgets

  base_budget = hb_bracket_budget(mu, survival_fraction, budgets)

  mus = c(mu, map_dbl(seq_len(fidelity_steps - 1), function(skip) {

    bx = budgets[-seq_len(skip)]
    lower = 1
    lb = hb_bracket_budget(lower, survival_fraction, bx)
    if (lb >= base_budget) return(lower)
    upper = ceiling(mu * fidelity_steps / length(bx))
    ub = hb_bracket_budget(upper, survival_fraction, bx)
    if (ub <= base_budget) return(upper)
    while (upper - lower > 1) {
      try = round(upper / 2 + lower / 2)
      tryb = hb_bracket_budget(try, survival_fraction, bx)
      if (tryb > base_budget) {
        upper = try
        ub = tryb
      } else {
        lower = try
        lb = tryb
      }
    }
    if (ub - base_budget > base_budget - lb) lower else upper
  }))

  rbindlist(lapply(seq_len(fidelity_steps), function(i) {
    hb_survivors_info(mus[[i]], survival_fraction, fidelity_steps - i + 1)[,
      `:=`(fidelity = fidelities[seq.int(i, fidelity_steps, 1)], sample_new = 0)][(evaluate)][1,
      `:=`(sample_new = survivors, survivors = sample_new)][, evaluate := NULL]
  }))[, c("fidelity", "sample_new", "survivors"), with = FALSE]

}

#' @title Create SMASHY Evaluation Plan
#'
#' @description
#' Create a `data.table` detailing the number and fidelities of evaluations to perform.
#'
#' @param mu (`integer(1)`)\cr
#'   Size of first generation
#' @param survival_fraction (`numeric(1)`)\cr
#'   Fraction of generation that gets killed.
#' @param fidelity_lower (`numeric(1)`)\cr
#'   Lower bound of fidelity parameter
#' @param fidelity_upper (`numeric(1)`)\cr
#'   Upper bound of fidelity parameter
#' @param fidelity_steps (`integer(1)`)\cr
#'   Max number of fidelity steps, at least 1.
#' @return `data.table` with columns `fidelity` (`numeric`): fidelity component value to evaluateat,
#'`sample_new` (`integer`) number of individuals to sample anew, and
#' `survivors` (`integer`): number of individuals kept alive from previous stage.
smashy_evaluation_schedule = function(mu, survival_fraction, fidelity_lower, fidelity_upper, fidelity_steps) {
  # use rev(seq(upper, lower)), so that with sequence length 1, we get the upper bound only.
  fidelities = rev(seq(fidelity_upper, fidelity_lower, length.out = fidelity_steps))

  survivors = max(round(survival_fraction * mu), 1)
  if (survivors == mu) {
    stop("Number of survivors equals the total population size. survival_fraction must be lower or mu must be larger.")
  }
  lambda = mu - survivors

  data.table(fidelity = fidelities, sample_new = lambda, survivors = survivors)[1,
    `:=`(sample_new = mu, survivors = 0)]
}
