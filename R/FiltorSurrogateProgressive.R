#' @title Progressive Surrogate Model Filtering
#'
#' @include FiltorSurrogate.R
#'
#' @name dict_filtors_surprog
#'
#' @description
#' Performs progressive surrogate model filtering. A surrogate model is used, as described in the parent class [`FiltorSurrogate`].
#' The filtering is "progressive" in that successive values are filtered more agressively.
#'
#' @section Algorithm:
#'
#' Given the number `n_filter` of of individuals to sample, and the desired pool size at round `i` `pool_size(i)`, progressive
#' surrogate model filtering proceeds as follows:
#' 1. Train the `surrogate_learner` [`LearnerRegr`][mlr3::LearnerRegr] on the `known_values` and their `fitnesses`.
#' 2. Take `pool_size(1)` configurations, predict their expected performance using the surrogate model, and put them
#'    into a pool `P` of configurations to consider.
#' 3. Initialize `i` to 1.
#' 4. Take the individual that is optimal according to predicted performance, remove it from `P` and add it to solution set `S`.
#' 5. If the number of solutions in `S` equals `n_filter`, quit.
#' 6. If `pool_size(i + 1)` is larger than `pool_size(i)`, take the next `pool_size(i + 1) - pool_size(i)` configurations,
#'   predict their expected performance using the surrogate model, and add them to `P`. Otherwise, remove `pool_size(i) - pool_size(i + 1)`
#'   random individuals from the pool. The size of `P` ends up being `pool_size(i + 1) - i`, as `i` individuals have also been removed and
#'   added to `S`.
#' 7. Increment `i`, jump to 4.
#'
#' (The algorithm presented here is optimized for clarity; the actual implementation does all the surrogate model prediction in one go, but is functionally
#' equivalent).
#'
#' `pool_size(i)` is calculated as `round(n_filter * pool_factor * (pool_factor_last / pool_factor) ^ (i / n_filter))`, i.e. a log-linear interpolation from
#' `pool_factor * n_filter` to `pool_factor_last * n_filter`.
#'
#' The `pool_factor` and `pool_factor_last` configuration parameters of this algorithm determine how agressively the surrogate model is used to
#' filter out sampled configurations. If the filtering is agressive (large values), then more "exploitation" at the cost of "exploration" is performed.
#' When `pool_factor` is small but `pool_factor_last` is large (or vice-versa), then different individuals are filtered with different agressiveness, potentially
#' leading to a tradeoff between "exploration" and "exploitation".
#'
#' When `pool_factor_last` is set, it defaults to `pool_factor`, with no new individuals added and no individuals removed from the filter pool during filtering.
#' It is equivalent to taking the top `n_filter` individuals out of a sample of `n_filter * pool_factor`.
#'
#' @section Configuration Parameters:
#' `FiltorSurrogateProgressive`'s configuration parameters are the hyperparameters of the [`FiltorSurrogate`] base class, as well as:
#'
#' * `filter.pool_factor` :: `numeric(1)`\cr
#'   `pool_factor` parameter of the progressive surrogate model filtering algorithm, see the corresponding section. Initialized to 1. Together with the
#'   default of `filter.pool_factor_last`, this is equivalent to random sampling new individuals.
#' * `filter.pool_factor_last` :: `numeric(1)`\cr
#'   `pool_factor_last` parameter of the progressive surrogate model filtering algorithm, see the corresponding section.
#'   When not given, it defaults to `filter.pool_factor`, equivalent to taking the top `n_filter` from `n_filter * pool_factor` individuals.
#'
#' @templateVar id surprog
#' @templateVar additional , \<surrogate_learner\> \[, \<surrogate_selector\>\]
#' @template autoinfo_prepare_ftr
#'
#' @section Supported Operand Types:
#' See [`FiltorSurrogate`] about supported operand types.
#'
#' @template autoinfo_dict
#'
#' @template param_surrogate_learner
#' @template param_surrogate_selector
#'
#' @family filtors
#'
#' @examples
#' library("mlr3")
#' library("mlr3learners")
#' fp = ftr("surprog", lrn("regr.lm"), filter.pool_first = 2)
#'
#' p = ps(x = p_dbl(-5, 5))
#' known_data = data.frame(x = 1:5)
#' fitnesses = 1:5
#' new_data = data.frame(x = c(2.5, 4.5))
#'
#' fp$prime(p)
#'
#' fp$needed_input(1)
#'
#' fp$operate(new_data, known_data, fitnesses, 1)
#'
#' @export
FiltorSurrogateProgressive = R6Class("FiltorSurrogateProgressive",
  inherit = FiltorSurrogate,
  public = list(
    initialize = function(surrogate_learner, surrogate_selector = SelectorProxy$new()) {
      own_param_set = ps(
        pool_factor = p_dbl(1, tags = "required"),
        pool_factor_last = p_dbl(1)
      )
      own_param_set$values = list(pool_factor= 1)

      super$initialize(surrogate_learner = surrogate_learner, surrogate_selector = surrogate_selector,
        own_param_set = param_set, dict_entry = "surprog"
      )
    }
  ),
  private = list(
    .filter_surrogate = function(values, surrogate_prediction, known_values, fitnesses, n_filter) {
      params = private$.own_param_set$get_values()
      poolsizes = round(exp(seq(log(params$pool_factor), log(params$pool_factor_last %??% params$pool_factor), length.out = n_filter)) * n_filter) - seq_len(n_filter) + 1
      selected = integer(n_filter)
      for (i in seq_len(n_filter)) {
        cpop = first(values[-selected], poolsizes[[i]])
        cfitness = first(surrogate_prediction[-selected, , drop = FALSE], poolsizes[[i]])
        selected[[i]] = private$.surrogate_selector$operate(cpop, cfitness)
      }
      selected
    },
    .needed_input = function(output_size) {
      params = private$.own_param_set$get_values()
      round(max(params$pool_factor, params$pool_factor_last ) * output_size)
    },
    .surrogate_learner = NULL,
    .surrogate_selector = NULL,
    .own_param_set = NULL
  )
)
dict_filtors$add("surprog", FiltorSurrogateProgressive)

