#' @title transform before applying scaling
# Takes another scalor as input, transforms fitness before applying
ScalorTransform = R6Class("ScalorTransform",
  inherit = Scalor,
  public = list(
    initialize = function(scalor) {
      # TODO
    }
  ),
  private = list(
    .scale = function(values, fitnesses) {

    }
  )
)
dict_scalors$add("transform", ScalorTransform)
