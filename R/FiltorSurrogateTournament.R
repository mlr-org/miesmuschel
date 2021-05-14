#' @title Tournament Surrogate Model Filtering
#'
#' @include FiltorSurrogate.R
#'
#' @name dict_filtors_surtourn
#'
#' @description
#' Performs tournament surrogate model filtering. A surrogate model is used, as described in the parent class [`FiltorSurrogate`].
#'
#' @section Algorithm:
#' Selects individuals from a tournament by taking the top `per_tournament` individuals, according to `surrogate_selector` and
#' as predicted by `surrogate_learner`, from a sample of `tournament_size(i)`, where `tournament_size(1)` is given by
#' `tournament_size`, `tournament_size(ceiling(n_filter / per_tournament))` is given by `tournament_size_last`, and
#' `tournament_size(i)` for `i` between these values is linearly interpolated on a log scale.
#'
#' @section Configuration Parameters:
#' `FiltorSurrogateProgressive`'s configuration parameters are the hyperparameters of the [`FiltorSurrogate`] base class, as well as:
#'
#' * `filter.per_tournament` :: `integer(1)`\cr
#'   Number of individuals to select from each tournament. If `per_tournament` is not a divider of `n_filter`, then
#'   the last tournament selects a random subset of `n_filter %% per_tournament` individuals out of the top `per_tournament` individuals.
#'   Initialized to 1.
#' * `filter.tournament_size` :: `numeric(1)`\cr
#'   Tournament size used for filtering. If `tournament_size_last` is not given, all `n_filter` individuals are selected
#'   in batches of `per_tournament` from tournaments of this size. If it is given, then the actual tournament size is interpolated
#'   between `tournament_size` and `tournament_size_last` on a logarithmic scale.
#'   Tournaments with tournament size below `per_tournament` select `per_tournament` individuals without tournament, i.e. no filtering.
#'   Initialized to 1.
#' * `filter.tournament_size_last` :: `numeric(1)`\cr
#'   Tournament size used for the last tournament, see description of `tournament_size`. Defaults to `tournament_size` when not given,
#'   i.e. all tournaments have the same size.
#'
#' @templateVar id surtourn
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
#' fp = ftr("surtourn", lrn("regr.lm"), filter.tournament_size = 2)
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
FiltorSurrogateTournament = R6Class("FiltorSurrogateTournament",
  inherit = FiltorSurrogate,
  public = list(
    initialize = function(surrogate_learner, surrogate_selector = SelectorProxy$new()) {
      own_param_set = ps(
        per_tournament = p_int(1, tags = "required"),
        tournament_size = p_dbl(1e-100, tags = "required"),
        tournament_size_last = p_dbl(1e-100)
      )
      own_param_set$values = list(per_tournament = 1, tournament_size = 1)

      super$initialize(surrogate_learner = surrogate_learner, param_set = own_param_set, surrogate_selector = surrogate_selector, dict_entry = "surprog")
    }
  ),
  private = list(
    .filter_surrogate = function(values, surrogate_prediction, known_values, fitnesses, n_filter, context) {
      params = private$.own_param_set$get_values(context = context)
      tournament_size = private$.tournament_size(n_filter, context)
      tournament_windows = split(seq_len(sum(tournament_size)), rep(seq_along(tournament_size), tournament_size))
      selected = vector("list", length(tournament_size))

      for (i in seq_along(tournament_windows)) {
        tw = tournament_windows[[i]]
        selected_from_window = private$.surrogate_selector$operate(values[tw], surrogate_prediction[tw, , drop = FALSE], params$per_tournament, context = context)
        selected[[i]] = tw[selected_from_window][sample.int(length(selected_from_window))]
      }
      first(unlist(selected, recursive = FALSE, use.names = FALSE), n_filter)
    },
    .needed_input = function(output_size, context) {
      sum(private$.tournament_size(output_size, context))
    },
    .tournament_size = function(output_size, context) {
      params = private$.own_param_set$get_values(context = context)
      number_of_tournaments = ceiling(output_size / params$per_tournament)
      tournament_size = round(exp(seq(
        log(params$tournament_size),
        log(params$tournament_size_last %??% params$tournament_size),
        length.out = number_of_tournaments
      )))
      pmax(tournament_size, params$per_tournament)
    }
  )
)
dict_filtors$add("surtour", FiltorSurrogateTournament)

