#' @title Filtor-Combination that Filters According to Two Filtors
#'
#' @include Filtor.R
#'
#' @name dict_filtor_maybe
#'
#' @description
#' [`Filtor`] that wraps two other [`Filtor`]s given during construction and chooses which operation to perform.
#' Each of the resulting `n_filter` individuals is chosen either from `$filtor`, or from `$filtor_not`.
#'
#' This makes it possible to implement filter methods such as random interleaving, where only a fraction of `p`
#' individuals were filtered and the others were not.
#'
#' Letting the number of individuals chosen by `$filtor` be `n_filter_f`, then `n_filter_f` is either fixed
#' set to `round(n_filter * p)`, (when `random_choise` is `FALSE`) or to `rbinom(1, n_filter, p)` (when `random_choice` is `TRUE`).
#'
#' When `random_choice` is `FALSE`, then `$needed_input()` is calculated directly from `$needed_input()` of `$filtor` and `$filtor_not`,
#' as well as `n_filter_f` and `n_filter - n_filter_f`.
#'
#' When `random_choice` is `TRUE`, then `$needed_input()` is considers the "worst case" from `$filtor` and `$filtor_not`, and assumes that
#' `$needed_input()` is monotonically increasing in its input argument.
#'
#' To make the worst case less extreme, the number of individuals chosen with `random_choice` set to `TRUE` is limited to
#' `qbinom(-20, n_filter, p, log.p = TRUE)` (with `lower.tail` `FALSE` and `TRUE` for `$filtor` and `$filtor_not`, respectively), which distorts the binomial
#' distribution with probability `1 - exp(-20)` or about `1 - 0.5e-9`.
#'
#' @section Configuration Parameters:
#' This operator has the configuration parameters of the [`Filtor`]s that it wraps: The configuration parameters of the operator given to the `filtor` construction argument
#' are prefixed with `"maybe."`, the configuration parameters of the operator given to the `filtor_not` construction argument are prefixed with `"maybe_not."`.
#'
#' Additional configuration parameters:
#' * `p` :: `numeric(1)` \cr
#'   Probability per individual (when `random_choise` is `TRUE`), or fraction of individuals (when `random_choice` is `FALSE`),
#'   that are chosen from `$filtor` instead of `$filtor_not`.
#' * `random_choice` :: `logical(1)` \cr
#'   Whether to sample the number of individuals chosen by `$filtor` according to `rbinom(1, n_filter, p)`, or to use a fixed fraction.
#'   Initialized to `FALSE`.
#'
#' @templateVar id maybe
#' @templateVar additional , <filtor> \[, <filtor_not>\]
#' @template autoinfo_prepare_ftr
#'
#' @section Supported Operand Types:
#'
#' Supported [`Param`][paradox::Param] classes are the set intersection of supported classes of `filtor` and `filtor_not`.
#'
#' @template autoinfo_dict
#'
#' @family filtors
#' @family filtor wrappers
#' @examples
#' library("mlr3")
#' library("mlr3learners")
#'
#' fm = ftr("maybe", ftr("surprog", lrn("regr.lm"), filter_rate_first = 2), p = 0.5)
#' p = ps(x = p_dbl(-5, 5))
#' known_data = data.frame(x = 1:5)
#' fitnesses = 1:5
#' new_data = data.frame(x = c(0.5, 1.5, 2.5, 3.5, 4.5))
#'
#' fm$prime(p)
#'
#' fm$needed_input(2)
#'
#' fm$operate(new_data, known_data, fitnesses, 2)
#'
#' fm$param_set$values$p = 0.33
#'
#' fm$needed_input(3)
#'
#' fm$operate(new_data, known_data, fitnesses, 3)
#'
#' @export
FiltorMaybe = R6Class("FiltorMaybe",
  inherit = Filtor,
  public = list(
    #' @description
    #' Initialize the `FiltorMaybe` object.
    #' @param filtor ([`Filtor`])\cr
    #'   [`Filtor`] to wrap. This operator gets run with probability `p` (Configuration parameter).\cr
    #'   The constructed object gets a *clone* of this argument.
    #' @param filtor_not ([`Filtor`])\cr
    #'   Another [`Filtor`] to wrap. This operator runs when `filtor` is not chosen. By
    #'   default, this is [`FiltorNull`], i.e. no filtering. With this default, the
    #'   `FiltorMaybe` object applies the `filtor` operation with probability / proportion `p`, and
    #'   no operation at all otherwise.\cr
    #'   The constructed object gets a *clone* of this argument.
    initialize = function(filtor, filtor_not = FiltorNull$new()) {
      private$.wrapped = assert_r6(filtor, "Filtor")$clone(deep = TRUE)
      private$.wrapped_not = assert_r6(filtor_not, "Filtor")$clone(deep = TRUE)

      private$.wrapped$param_set$set_id = "maybe"
      private$.wrapped_not$param_set$set_id = "maybe_not"
      private$.maybe_param_set = ps(p = p_dbl(0, 1, tags = "required"), random_choice = p_lgl(tags = "required"))
      private$.maybe_param_set$values = list(p = 1, random_choice = FALSE)
      super$initialize(intersect(filtor$param_classes, filtor_not$param_classes),
        alist(private$.maybe_param_set, private$.wrapped$param_set, private$.wrapped_not$param_set))
    },
    #' @description
    #' See [`MiesOperator`] method. Primes both this operator, as well as the wrapped operators
    #' given to `filtor` and `filtor_not` during construction.
    #' @param param_set ([`ParamSet`][paradox::ParamSet])\cr
    #'   Passed to [`MiesOperator`]`$prime()`.
    #' @return [invisible] `self`.
    prime = function(param_set) {
      private$.wrapped$prime(param_set)
      private$.wrapped_not$prime(param_set)
      super$prime(param_set)
      invisible(self)
    }
  ),
  private = list(
    .filter = function(values, known_values, fitnesses, n_filter) {
      params = self$param_set$get_values()

      if (params$random_choice) {
        filter_min = stats::qbinom(-20, n_filter, params$p, log.p = TRUE, lower.tail = TRUE)
        filter_max = stats::qbinom(-20, n_filter, params$p, log.p = TRUE, lower.tail = FALSE)

        filtering = stats::rbinom(1, n_filter, params$p)
        filtering = min(max(filtering, filter_min), filter_max)
      } else {
        filtering = round(n_filter * params$p)
      }
      if (filtering == 0) {
        private$.wrapped_not$operate(values, known_values, fitnesses, n_filter)
      } else if (filtering == n_filter) {
        private$.wrapped$operate(values, known_values, fitnesses, n_filter)
      } else {
        for_wrapped = seq_len(private$.wrapped$needed_input(filtering))
        for_wrapped_not = seq.int(length(for_wrapped) + 1, length.out = private$.wrapped_not$needed_input(n_filter - filtering))
        c(
            private$.wrapped$operate(values[for_wrapped], known_values, fitnesses, filtering),
            length(for_wrapped) + private$.wrapped_not$operate(values[for_wrapped_not], known_values, fitnesses, n_filter - filtering)
        )
      }
    },
    .needed_input = function(output_size) {
      params = self$param_set$get_values()
      if (params$random_choice) {
        filter_min = stats::qbinom(-20, output_size, params$p, log.p = TRUE, lower.tail = TRUE)
        filter_max = stats::qbinom(-20, output_size, params$p, log.p = TRUE, lower.tail = FALSE)
        # worst case: take filter_max from .wrapped, and take output_size - filter_min from .wrapped_not.
        #
        # This is even a bit worse than the worst case, since the needed_input arguments of both wrapped functions
        # do not sum to output_size. In theory we could iterate through filter_min:filter_max and calculate the
        # max of '.wrapped$needed_input(i) + .wrapped_not$needed_input(output_size - i)', but that is probably
        # more wasteful so we don't do that here.
        private$.wrapped$needed_input(filter_max) + private$.wrapped_not$needed_input(output_size - filter_min)
      } else {
        filtering = round(output_size * params$p)
        # we know exactly how many elements each filter needs.
        private$.wrapped$needed_input(filtering) + private$.wrapped_not$needed_input(output_size - filtering)
      }
    },
    .wrapped = NULL,
    .wrapped_not = NULL,
    .maybe_param_set = NULL
  )
)
dict_filtors$add("maybe", FiltorMaybe)
