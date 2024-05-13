#' @title Scalor giving Weighted Sum of Multiple Scalors
#'
#' @include Scalor.R
#'
#' @name dict_scalors_aggregate
#'
#' @description
#' [`Scalor`] that applies multiple other [`Scalor`]s and calculates their weighted sum.
#'
#' @section Configuration Parameters:
#' This operation has the configuration parameters of the [`Scalor`]s that it wraps: The configuration
#' parameters of the operator given to the `scalors` construction argument are prefixed with `"scalor_1"`,
#' `"scalor_2"`, ... up to `"scalor_#"`, where `#` is `length(scalors)`.
#'
#' Additional configuration parameters:
#' * `weight_1`, `weight_2`, ... :: `numeric(1)`\cr
#'   Weight factors of `scalors[[1]]`, `scalors[[2]]`, etc. Depending on `scaling`, the outputs of `scalors`
#'   is multiplied with this (when `scaling` is `"linear"` or `"rank"`), or ties between ranks are broken
#'   with it (when `scaling` is `"tiebreak"`). Initialized to 1.
#' * `scaling` :: `character(1)`\cr
#'   How to calculate output values, one of `"linear"`, `"rank"` or `"tiebreak"`. When `scaling` is `"linear"`,
#'   then the output is calculated as the weighted sum of the outputs of `scalors`, weighted by `weight_1`,
#'   `weight_2` etc. When `scaling` is `"rank"`, then the output is calculated as the weighted sum of the
#'   `rank()` of `scalors`, weighted by `weight_1`, `weight_2` etc., with ties broken by average. When `scaling`
#'   is `"tiebreak"`, then the output is calculated as the averaged `rank()` of the `scalors` with the
#'   highest `weight_#`, with ties broken by the average `rank()` of the second highest `weight_#`, with
#'   remaining ties broken by `scalors` with third highest `weight_#` etc. Initialized to `"linear"`.
#' * `scale_output` :: `logical(1)`\cr
#'   Whether to scale the output to lie between 0 and 1. Initialized to `FALSE`.
#'
#' @templateVar id aggregate
#' @templateVar additional , \<scalors\>
#' @template autoinfo_prepare_scl
#' @template autoinfo_dict
#'
#' @family scalors
#' @family scalor wrappers
#' @examples
#' p = ps(x = p_dbl(-5, 5))
#' data = data.frame(x = rep(0, 5))
#'
#' sa = scl("aggregate", list(
#'     scl("one", objective = 1),
#'     scl("one", objective = 2)
#' ))
#' sa$prime(p)
#'
#' (fitnesses = matrix(c(1, 5, 2, 3, 0, 3, 1, 0, 10, 8), ncol = 2))
#'
#' # to see the fitness matrix, use:
#' ## plot(fitnesses, pch = as.character(1:5))
#'
#' # default weight 1 -- sum of both objectives
#' sa$operate(data, fitnesses)
#'
#' # only first objective
#' sa$param_set$values[c("weight_1", "weight_2")] = c(1, 0)
#' sa$operate(data, fitnesses)
#'
#' # only 2 * second objective
#' sa$param_set$values[c("weight_1", "weight_2")] = c(0, 2)
#' sa$operate(data, fitnesses)
#' @export
ScalorAggregate = R6Class("ScalorAggregate",
  inherit = Scalor,
  public = list(
    #' @description
    #' Initialize the `ScalorAggregate` object.
    #' @param scalors (`list` of [`Scalor`])\cr
    #'   [`Scalor`]s to wrap. The operations are run and weighted by `weight_#` configuration parameters,
    #'   depending on the `scaling` configuration parameter.
    #'   The constructed object gets a *clone* of this argument. The `$scalors` field will reflect this value.
    initialize = function(scalors) {
      private$.wrapped = imap(unname(assert_list(scalors, types = "Scalor", min.len = 1)), function(x, i) {
        x$clone(deep = TRUE)
        if (!paradox_s3) {
          x$param_set$set_id = sprintf("scalor_%s", i)
        }
        x
      })

      pnames = sprintf("weight_%s", seq_along(scalors))

      private$.own_param_set = do.call(paradox::ps, c(
        list(scaling = p_fct(c("linear", "rank", "tiebreak")), scale_output = p_lgl(tags = "required")),
        named_list(pnames, p_dbl(tags = "required"))
      ))
      private$.own_param_set$values = c(named_list(pnames, 1), list(scaling = "linear", scale_output = FALSE))

      ps_alist = c(alist(private$.own_param_set),
        lapply(seq_along(scalors), function(i) substitute(private$.wrapped[[i]]$param_set, list(i = i)))
      )
      names(ps_alist) = sprintf("scalor_%s", seq_along(ps_alist))

      super$initialize(Reduce(intersect, map(private$.wrapped, "param_classes")), ps_alist,
        supported = Reduce(intersect, map(private$.wrapped, "supported")),
        packages = unique(unlist(map(private$.wrapped, "packages"), use.names = FALSE, recursive = FALSE)),
        dict_entry = "aggregate", own_param_set = quote(private$.own_param_set))
    },
    #' @description
    #' See [`MiesOperator`] method. Primes both this operator, as well as the wrapped operators
    #' given to `scalors` during construction.
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
    #' @field scalors (`list` of [`Scalor`])\cr
    #' [`Scalor`]s being wrapped. These operators are run and their outputs weighted.
    scalors = function(val) {
      if (!missing(val)) stop("selectors is read-only.")
      private$.wrapped
    }
  ),
  private = list(
    .scale = function(values, fitnesses) {
      params = private$.own_param_set$get_values()
      pnames = sprintf("weight_%s", seq_along(private$.wrapped))
      weights = unlist(params[pnames], use.names = FALSE, recursive = FALSE)

      outputs = lapply(private$.wrapped, function(w) w$operate(values, fitnesses))

      wsum = switch(params$scaling,
        linear = c(do.call(cbind, outputs) %*% weights),
        rank = c(do.call(cbind, lapply(outputs, rank)) %*% weights),
        tiebreak = {
          if (any(weights <= 0)) stop("Only positive weights allowed when 'scaling' is \"tiebreak\".")
          weights = rank(weights, ties.method = "first")
          weightsteps = split(seq_along(weights), weights)  # weightsteps: list going from low to high weights, contains indices of weights with these values
          ranking = rep(1, length(weights))
          # iterate from scalor(s) with lowest weights to ones with highest weights
          for (current_scalors in weightsteps) {
            # tiebreak by calling rank() twice: rank transform first,
            # then add ranks of invocations with lower weights divided by length(rank) --> adding 'ranking'
            # will not change rank of values that are not ties.
            newrank = Reduce(`+`, lapply(outputs[current_scalors], rank, ties.method = "first"))
            ranking = rank(newrank + ranking / (length(ranking) + 1))
          }
          ranking
        }
      )
      if (params$scale_output) {
        normie_scale(wsum)
      } else {
        wsum
      }
    },
    deep_clone = function(name, value) {
      if (name == ".wrapped") {
        lapply(value, function(x) x$clone(deep = TRUE))
      } else {
        super$deep_clone(name, value)
      }
    },
    .wrapped = NULL,
    .own_param_set = NULL
  )
)
dict_scalors$add("aggregate", ScalorAggregate, aux_construction_args = alist(scalors = ScalorOne$new()))
