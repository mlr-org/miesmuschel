#' @param as_fitnesses (`logical(1)`)\cr
#'   Whether to transform performance values into "fitness" values that are always to be maximized.
#'   This means that values that objectives that should originally be minimized are multiplied with -1,
#'   and that parts of the objective codomain that are neither being minimized nor maximized are dropped.
#'   Default `TRUE`.
