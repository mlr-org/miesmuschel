#' @param survivors_only (`logical(1)`)\cr
#'   Whether to ignore configurations that have `"eol"` set to the given generation, i.e. individuals that were killed during that generation.
#'   When this is `TRUE` (default), then only individuals that are alive at the *end* of a generation are considered; otherwise all individuals
#'   alive at any point of a generation are considered. If it is `TRUE`, this leads to individuals that have `"dob"` == `"eol"` being ignored.
