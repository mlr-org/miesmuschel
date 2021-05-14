#' @title Run Multiple Recombinator Operations in Sequence
#'
#' @include Recombinator.R
#'
#' @name dict_recombinators_sequential
#'
#' @description
#' [`Recombinator`] that wraps multiple other [`Recombinator`]s given during construction and uses them for mutation in sequence.
#'
#' When subsequent [`Recombinator`]s have mismatching `n_indivs_out` / `n_indivs_in`, then `RecombinatorSequential` tries to
#' match them by running them multiple times. If e.g. `recombinators[[1]]$n_indivs_out` is 2 and `recombinators[[2]]$n_indivs_in` is 1, then
#' `recombinators[[2]]` is run twice, once for each output of `recombinators[[1]]`.
#'
#' When the `allow_lcm_packing` argument is `FALSE`, then an error is given if neither `n_indivs_out` of a [`Recombinator`] divides `n_indivs_in` of the
#' following [`Recombinator`], nor `n_indivs_in` of the latter divides `n_indivs_out` of the former even when considering that the former is run multiple times.
#' If `allow_lcm_packing` is `TRUE`, then both recombinators are run multiple times, according to the lowest common multiple ("lcm")
#' of the two.
#'
#' However, `allow_lcm_packing` can lead to very large values of `n_indivs_in` / `n_indivs_out`, so it may instead be preferred to add [`RecombinatorNull`] objects
#' with fitting `n_indivs_in` / `n_indivs_out` values to match subsequent recombinators.
#'
#' @section Configuration Parameters:
#' This operator has the configuration parameters of the [`Recombinator`]s that it wraps: The configuration parameters of the operator given to the `recombinators` construction
#' argument are prefixed with `"recombinator_1"`, `"recombinator_2"`, ... up to `"recombinator_#"`, where `#` is `length(recombinators)`.
#'
#' Additional configuration parameters:
#' * `shuffle_between` :: `logical(1)` \cr
#'   Whether to reorder `values` between invocations of recombinators. Initialized to `TRUE`.
#'
#' @templateVar id sequential
#' @templateVar additional , \<recombinators\>
#' @template autoinfo_prepare_rec
#'
#' @section Supported Operand Types:
#'
#' Supported [`Param`][paradox::Param] classes are the set intersection of supported classes of the [`Recombinator`]s given in `recombinators`.
#'
#' @template autoinfo_dict
#'
#' @family recombinators
#' @family recombinator wrappers
#' @examples
#' # TODO
#' @export
RecombinatorSequential = R6Class("RecombinatorSequential",
  inherit = Recombinator,
  public = list(
    #' @description
    #' Initialize the `RecombinatorSequential` object.
    #' @param recombinators (`list` of [`Recombinator`])\cr
    #'   [`Recombinator`]s to wrap. The operations are run in order given to `recombinators`.
    #'   The constructed object gets a *clone* of this argument. The `$recombinators` field will reflect this value.
    #' @param allow_lcm_packing (`logical(1)`)\cr
    #'   Whether to allow lowest common multiple packing. Default `FALSE`.
    initialize = function(recombinators, allow_lcm_packing = FALSE) {
      private$.wrapped = imap(unname(assert_list(recombinators, types = "Recombinator", min.len = 1)), function(x, i) {
        x$clone(deep = TRUE)
        x$param_set$set_id = sprintf("recombinator_%s", i)
        x
      })
      private$.own_param_set = ps(shuffle_between = p_lgl(tags = "required"))
      private$.own_param_set$values = list(shuffle_between = TRUE)
      ps_alist = c(alist(private$.own_param_set),
        lapply(seq_along(recombinators), function(i) substitute(private$.wrapped[[i]], list(i = i)))
      )

      # how often is each recombinator called?
      private$.multiplicities = numeric(length(recombinators))
      private$.multiplicities[[1]] = 1
      limit = 2 / .Machine$double.eps
      current_mul = 1
      for (i in seq_len(length(recombinators) - 1)) {
        current_out = private$.wrapped[[i]]$n_indivs_out

        current_fullout = current_mul * current_out
        if (current_fullout >= limit) stop("Overflow when trying to fit recombinator in/outputs. Use 'RecombinatorNull' to fit neighbouring operators better.")

        next_in = private$.wrapped[[i + 1]]$n_indivs_in

        if (!allow_lcm_packing && !(current_fullout %% next_in == 0) && !(next_in %% current_out == 0)) {
          stopf("recombinators[[%s]]$n_indivs_out is %s and recombinators[[%s]]$n_indivs_in is %s. None is an integer multiple of the other.
Either match input and output sizes using a RecombinatorNull, or allow rescaling of both, set 'allow_lcm_packing' to TRUE.",
            i, current_out, i + 1, current_in)
        }
        step_gcd = gcd(current_fullout, next_in)
        current_mul = current_fullout / step_gcd
        private$.multiplicities = private$.multiplicities * (next_in / step_gcd)
        private$.multiplicities[[i + 1]] = current_mul
      }
      if (any(private$.multiplicities > limit)) stop("Overflow when trying to fit recombinator in/outputs. Use 'RecombinatorNull' to fit neighbouring operators better.")
      if (any(private$.multiplicities > 1000)) warningf("At least one recombinator in RecombinatorSequential is run %s times, consider using RecombinatorNull to fit n_indivs_in / n_indivs_out to avoid too large packing.", max(private$.multiplicities))


      super$initialize(Reduce(intersect, map(private$.wrapped, "param_classes")), ps_alist,
        n_indivs_in = private$.wrapped[[1]]$n_indivs_in * private$.multiplicities[[1]],
        n_indivs_out = private$.wrapped[[length(private$.wrapped)]]$n_indivs_out * private$.multiplicities[[length(private$.multiplicities)]],
        packages = unique(unlist(map(private$.wrapped, "packages"), use.names = FALSE, recursive = FALSE)),
        dict_entry = "sequential", own_param_set = quote(private$.own_param_set))
    },
    #' @description
    #' See [`MiesOperator`] method. Primes both this operator, as well as the wrapped operators
    #' given to `recombinator` and `recombinator_not` during construction.
    #' @param param_set ([`ParamSet`][paradox::ParamSet])\cr
    #'   Passed to [`MiesOperator`]`$prime()`.
    #' @return [invisible] `self`.
    prime = function(param_set) {
      lapply(private$.wrapped, function(x) x$prime(param_set))
      super$prime(param_set)
      invisible(self)
    }
  ),
  active = list(
    #' @field recombinators (`list` of [`Recombinator`])\cr
    #' [`Recombinator`]s being wrapped. These operators get run sequentially in order.
    recombinators = function(val) {
      if (!missing(val)) stop("recombinators is read-only.")
      private$.wrapped
    }
  ),
  private = list(
    .recombine = function(values, context) {
      for (r in private$.wrapped) {
        values = r$operate(values)
      }
      values
    },
    deep_clone = function(name, value) {
      if (name == ".wrapped") {
        lapply(value, function(x) x$clone(deep = TRUE))
      } else {
        super$deep_clone(name, value)
      }
    },
    .wrapped = NULL,
    .own_param_set = NULL,
    .multiplicities = NULL
  )
)
dict_recombinators$add("sequential", RecombinatorMaybe)

gcd = function(a, b) {
  if (a > b) {
    tmp = b
    b = a
    a = tmp
  }
  while (a != 0) {
    tmp = a
    a = b %% a
    b = tmp
  }
  b
}
