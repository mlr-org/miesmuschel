#' @param scalor ([`Scalor`])\cr
#'   [`Scalor`] to use to generate scalar values from multiple objectives, if multi-objective optimization is performed.
#'   Initialized to [`ScalorSingleObjective`]: Doing single-objective optimization normally, throwing an error if used
#'   in multi-objective setting: In that case, a [`Scalor`] needs to be explicitly chosen.
