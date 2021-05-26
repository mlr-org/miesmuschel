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
#' * `nadir`
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
        nadir = p_vct(tags = "required", depends = tiebreak == "hv-contrib"),
        jitter = p_lgl(tags = "required"),
        scale_output = p_lgl(tags = "required"),
        tiebreak = p_fct(c("crowdingdist", "hvcontrib", "domcount", "none")))
      param_set$values = list(epsilon = 0, jitter = TRUE, scale_output = TRUE, tiebreak= "crowdingdist")
      super$initialize(param_set = param_set, dict_entry = "nondom")
    }
  ),
  private = list(
    .scale = function(values, fitnesses, context) {
      params = self$param_set$get_values(context = context)
      if (params$jitter) {
        fitnesses = fitnesses *
          (1 + runif(length(fitnesses)) * sqrt(.Machine$double.eps))
      }
      rnd = rank_nondominated(fitnesses, epsilon = params$epsilon)
      ranked = rnd$fronts
      if (params$tiebreak != "none") {
        if (params$tiebreak == "domcount") {
          subrank = lapply(split(rnd$domcount, ranked), function(x) rank(x) / (length(x) + 1))
        } else {
          fronts = lapply(split(as.data.frame(fitnesses), ranked), as.matrix)
          subrank = switch(params$tiebreak,
            crowdingdist = lapply(fronts, function(x) rank(dist_crowding(x)) / (nrow(x) + 1)),
            hvcontrib = lapply(fonts, function(x) rank(domhv_contribution(x, nadir = params$nadir, epsilon = epsilon))),
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

