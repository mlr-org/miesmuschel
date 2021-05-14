#' @title Run Multiple Selection Operations in Sequence
#'
#' @include Selector.R
#'
#' @name dict_selectors_sequential
#'
#' @description
#' [`Selector`] that wraps multiple other [`Selector`]s given during construction and uses them for selection in sequence.
#' This makes it possible for one [`Selector`] to discard a few individuals, followed by a second [`Selector`] to
#' discard more, etc., until `n_select` individuals are remaining.
#'
#' @section Algorithm:
#' Given that there are `nrow(values)` input individuals in an operation, and `n_select` individuals requested to be selected,
#' the operation calls `selector_i` for `i` in 1 ... `length(selectors)` to reduce the number of individuals in this pipeline.
#' The relative quantity by which the number of individuals is reduced in each step is determined by the configuration parameters
#' `reduction_1`, `reduction_2`, etc., and also dependent on the sum of these values, in the following denoted, with a slight abuse of notation, by `sum[reduction_#]`.
#'
#' Let the number of individuals passed to step `i` be denoted by `n_values[i]`, and the number of individuals requested to be
#' selected by that step be denoted as `n_select_[i]`. In particular, `n_values[1] == nrow(values)`, and `n_select_[length(selectors)] == n_select`.
#'
#' When `reduction_by_factor` is `TRUE`, then the reduction at step `i` is done by a factor, meaning that `n_values[i] / n_select_[i]` is set (up to rounding).
#' This factor is `(nrow(values) / n_select) ^ (reduction_i / sum[reduction_#])`.
#'
#' When `reduction_by_factor` is `FALSE`, then the reduction at step `i` is done by absolute differences, meaning that `n_values[i] - n_select_[i]` is set (up to rounding).
#' This difference is `(nrow(values) - n_select) * (reduction_i / sum[reduction_#])`, with `sum[reduction_#]` as above.
#'
#' In particular, this means that when all `reduction_#` values are the same and `reduction_by_factor` is `TRUE`, then each operation reduces the number of
#' individuals in the pipeline by the same factor. When `reduction_by_factor` is `FALSE`, then each operation removes the same absolute number of individuals.
#'
#' While the illustrations are done with the assumption that `nrow(values) >= n_select`, they hold equivalently with `nrow(values) < n_select`.
#'
#' @section Configuration Parameters:
#' This operator has the configuration parameters of the [`Selector`]s that it wraps: The configuration parameters of the operator given to the `selectors` construction
#' argument are prefixed with `"selector_1"`, `"selector_2"`, ... up to `"selector_#"`, where `#` is `length(selectors)`.
#'
#' Additional configuration parameters:
#' * `reduction_1`, `reduction_2`, ... :: `numeric(1)`\cr
#'   Relative reduction done by `selector_1`, `selector_2`, ..., as described in the section **Algorithm**. The values are all initialized to 1, meaning
#'   the same factor (when `reduction_by_factor` is `TRUE`) or absolute number (otherwise) of reduction by each operation.
#' * `reduction_by_factor` :: `logical(1)`\cr
#'   Whether to do reduction by factor (`TRUE`) or absolute number (`FALSE`), as described in **Algorithm**. Initialized to `TRUE`.
#'
#' @templateVar id sequential
#' @templateVar additional , \<selectors\>
#' @template autoinfo_prepare_sel
#'
#' @section Supported Operand Types:
#'
#' Supported [`Param`][paradox::Param] classes are the set intersection of supported classes of the [`Selector`]s given in `selectors`.
#'
#' @template autoinfo_dict
#'
#' @family selectors
#' @family selector wrappers
#' @examples
#' # TODO
#' @export
SelectorSequential = R6Class("SelectorSequential",
  inherit = Selector,
  public = list(
    #' @description
    #' Initialize the `SelectorSequential` object.
    #' @param selectors (`list` of [`Selector`])\cr
    #'   [`Selector`]s to wrap. The operations are run in order given to `selectors`.
    #'   The constructed object gets a *clone* of this argument. The `$selectors` field will reflect this value.
    initialize = function(selectors) {
      private$.wrapped = imap(unname(assert_list(selectors, types = "Selector", min.len = 1)), function(x, i) {
        x$clone(deep = TRUE)
        x$param_set$set_id = sprintf("selector_%s", i)
        x
      })

      pnames = sprintf("reduction_%s", seq_along(selectors))

      private$.own_param_set = do.call(paradox::ps, c(
        named_list(pnames, p_dbl(0, tags = "required")),
        list(reduction_by_factor = p_lgl(tags = "required"))
      ))
      private$.own_param_set$values = c(named_list(pnames, 1), list(reduction_by_factor = TRUE))

      ps_alist = c(alist(private$.own_param_set),
        lapply(seq_along(selectors), function(i) substitute(private$.wrapped[[i]], list(i = i)))
      )

      private$.own_param_set$values = list()
      super$initialize(Reduce(intersect, map(private$.wrapped, "param_classes")), ps_alist,
        supported = Reduce(intersect, map(private$.wrapped, "supported")),
        packages = unique(unlist(map(private$.wrapped, "packages"), use.names = FALSE, recursive = FALSE)),
        dict_entry = "sequential", own_param_set = quote(private$.own_param_set))
    },
    #' @description
    #' See [`MiesOperator`] method. Primes both this operator, as well as the wrapped operators
    #' given to `selector` and `selector_not` during construction.
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
    #' @field selectors (`list` of [`Selector`])\cr
    #' [`Selector`]s being wrapped. These operators get run sequentially in order.
    selectors = function(val) {
      if (!missing(val)) stop("selectors is read-only.")
      private$.wrapped
    }
  ),
  private = list(
    .select = function(values, fitnesses, n_select, context) {
      params = private$.own_param_set$get_values(context = context)
      pnames = sprintf("reduction_%s", seq_along(selectors))

      reductions = unlist(params[pnames], use.names = FALSE, recursive = FALSE)
      if (sum(reductions)) {
        stop("At least one of 'reduction_1', 'reduction_2', ... must be > 0.")
      }
      outputs = cumsum(reductions)
      # normalise, but take care of the possibility that cumsum() gives different result than sum() for numerics reasons
      outputs = outputs / outputs[length(outputs)]

      if (params$reduction_by_factor) {
        lognv = log(nrow(values))
        outputs = exp(outputs * (log(n_select) - lognv) + lognv)
      } else {
        outputs = outputs * (n_select - nrow(values)) + nrow(values)
      }
      outputs = round(outputs)

      # accumulate selected individuals, since the result needs to be relative to the `values` input of this call.
      outputmap = seq_len(nrow(values))

      for (i in seq_along(private$.wrapped)) {
        op = private$.wrapped[[i]]
        on = outputs[[i]]
        selected = op$operate(values, fitnesses, on, context = context)
        values = values[selected]
        fitnesses = fitnesses[selected, , drop = FALSE]
        outputmap = outputmap[selected]
      }
      outputmap
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
dict_selectors$add("sequential", SelectorMaybe)
