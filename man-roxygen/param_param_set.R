#' @param param_set ([`ParamSet`][paradox::ParamSet] | `list` of `expression`)\cr
#'   Hyperparameter of the operator. This should be created by the subclass and given to `super$initialize()`.
#'   If this is a [`ParamSet`][paradox::ParamSet], it is used as the `MiesOperator`'s [`ParamSet`][paradox::ParamSet]
#'   directly. Otherwise it must be a `list` of expressions e.g. created by `alist()` that evaluate to [`ParamSet`][paradox::ParamSet]s,
#'   possibly referencing `self` and `private`.
#'   These [`ParamSet`][paradox::ParamSet] are then combined using a [`ParamSetCollection`][paradox::ParamSetCollection].
#'   Default is the empty [`ParamSet`][paradox::ParamSet].\cr
#'   The `$param_set` field will reflect this value.
