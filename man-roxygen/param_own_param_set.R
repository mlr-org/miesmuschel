#' @param own_param_set (`language`)\cr
#'   An expression that evaluates to a [`ParamSet`][paradox::ParamSet] indicating the configuration parameters that are entirely owned by
#'   this operator class (and not proxied from a construction argument object). This should be `quote(self$param_set)` (the default) when
#'   the `param_set` argument is not a list of expressions.
