#' @title Nondominated Sorting Scalor
#'
#' @include Scalor.R
#'
#' @name dict_scalors_nondom
#'
#' @description
#' [`Scalor`] that returns a the rank of the pareto-front in nondominated sorting as scale. Higher ranks
#' indocate higher fitnesses and therefore "better" individuals.
#'
#' @section Configuration Parameters:
#' * `epsilon`
#' * `jitter`
#' * `scale_output`
#' * `tiebreak`
#'
#' @templateVar id nondom
#' @template autoinfo_prepare_scl
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family scalors
#' @examples
#' so = scl("nondom")
#' p = ps(x = p_dbl(-5, 5))
#' # dummy data; note that ScalorNondom does not depend on data content
#' data = data.frame(x = rep(0, 5))
#' fitnesses = matrix(c(1, 5, 2, 3, 0, 3, 1, 0, 10, 8), ncol = 2)
#'
#' so$prime(p)
#'
#' so$operate(data, fitnesses)
#' @export
ScalorNondom = R6Class("ScalorNondom",
  inherit = Scalor,
  public = list(
    #' @description
    #' Initialize the `ScalorNondom` object.
    initialize = function() {
      param_set = ps(
        epsilon = p_vct(lower = 0, tags = "required"),
        jitter = p_lgl(tags = "required"),
        scale_output = p_lgl(tags = "required"),
        tiebreak = p_fct(c("crowding-dist", "hv-contrib", "domcount", "none")))
      param_set$values = list(epsilon = 0, jitter = TRUE, scale_output = TRUE, tiebreak= "crowding-dist")
      super$initialize(param_set = param_set, dict_entry = "nondom")
    }
  ),
  private = list(
    .scale = function(values, fitnesses) {
      params = self$param_set$get_values()
      if (params$jitter) {
        fitnesses = fitnesses *
          (1 + runif(length(fitnesses)) * sqrt(.Machine$double.eps))
      }
      nadir = 0
      rnd = rank_nondominated(fitnesses, epsilon = params$epsilon)
      ranked = rnd$fronts
      if (params$tiebreak != "none") {
        if (params$tiebreak == "domcount") {
          subrank = lapply(split(rnd$domcount, ranked), function(x) rank(x) / (length(x) + 1))
        } else {
          fronts = lapply(split(as.data.frame(fitnesses), ranked), as.matrix)
          subrank = switch(params$tiebreak,
            `crowding-dist` = lapply(fronts, function(x) rank(dist_crowding(x)) / (nrow(x) + 1)),
            `hv-contrib` = lapply(fonts, function(x) rank(domhv_contribution(x, nadir = nadir, epsilon = epsilon))),
          )
        }
        for (i in seq_along(subranks)) {
          sr = subranks[[i]]
          ranked[ranked == i] = i + sr
        }
      }
      if (scale_output) {
        1 - (ranked - 1) / max(nd$fronts) # want high front values for high fitnesses, so reverse ordering here
      } else {
        max(nd$fronts) + 1 - ranked
      }
    }
  )
)
dict_scalors$add("nondom", ScalorNondom)

