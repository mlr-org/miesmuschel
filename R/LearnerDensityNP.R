#' @title Kernel Density Estimator using the "np" Package
#'
#' @name mlr_learners_density.np
#'
#' @description
#' Perform kernel density estimation using the `np` package.
#'
#' @section Hyperparameters:
#' * `bwmethod` :: `character(1)`\cr
#'   Bandwidth selection method. One of `"cv.ml"`, `"cv.ls"`, `"normal-reference"`. Default `"cv.ml"`.
#' * `bwtype` :: `character(1)`\cr
#'   Continuous variable bandwidth type, one of `"fixed"` (default), `"generalized_nn"`, `"adaptive_nn"`.
#' * `ckertype` :: `character(1)`\cr
#'   Continuous kernel type. One of `"gaussian"` (default), `"epanechnikov"`, `"uniform"`.
#' * `ckerorder` :: `integer(1)`\cr
#'   Continuous Kernel order (only for gaussian and epanechnikov). One of 2 (default), 4, 6, 8.
#' * `ukertype` :: `character(1)`\cr
#'   Unordered categorical kernel type. One of `"aitchisonaitken"` (default) or `"liracine"`.
#' * `okertype` :: `character(1)`\cr
#'   Ordered kernel type. One of `"liracine"` (default), `"wangvanryzin"`.
#' * `nmulti` :: `integer(1)`\cr
#'   Number of random restarts for cross validation likelihood optimization. Default: number of features, at most 5. 0 disables restarts.
#' * `remin` :: `logical(1)`\cr
#'   Restart from located minima; Default `TRUE`.
#' * `itmax` :: `integer(1)`\cr
#'   Max optimization iters. Default 10000.
#' * `ftol` :: `numeric(1)`\cr
#'   Optimization y-value relative tolerance, default `10 * sqrt(.Machine$double.eps)`.
#' * `tol` :: `numeric(1)`\cr
#'   Optimization x-value relative tolerance, default `10000 * sqrt(.Machine$double.eps)`.
#' * `small` :: `numeric(1)`\cr
#'   Optimization x-value absolute tolerance (?), default `1000 * sqrt(.Machine$double.eps)`.
#' * `min_bandwidth` :: `numeric(1)`\cr
#'   Minimum bandwidth (all kerneltypes). Not part of np::npudensbw. Default 0. BOHB has this at `1e-3`.
#' * `sampling_bw_factor` :: `numeric(1)`\cr
#'   Oversmoothing bandwidth factor for sampling. Not part of np::npudensbw. Default 1. BOHB has this at 3.
#'
#' TODO: could also implement `lb{c,d}.{dir,init}`, `{c,d}fac.{dir,init}`, `dfc.dir`, `hbd.dir`, `hb{c,d}.init`, `init{c,d}.dir`, `scale.init.categorical.sample`.
#'
#' TODO: `normal-reference` seems to disable optimization, consider putting that as a dependency on the optimization-related params.
#'
#' TODO: could add dependency for ckerorder on not-uniform.
#'
#' @export
#' @family density estimation classes
LearnerDensityNP = R6Class("LearnerDensityNP", inherit = LearnerDensity,
  public = list(
    #' @description
    #' Initialize the `LearnerDensityNP` object.
    initialize = function() {
      param_set = ps(
        bwmethod = p_fct(c("cv.ml", "cv.ls", "normal-reference"), default = "cv.ml", tags = "train"),
        bwtype = p_fct(c("fixed", "generalized_nn", "adaptive_nn"), default = "fixed", tags = "train"),
        ckertype = p_fct(c("gaussian", "epanechnikov", "uniform"), default = "gaussian", tags = "train"),
        ckerorder = p_int(2, 8, default = 2, tags = "train"),
        ukertype = p_fct(c("aitchisonaitken", "liracine"), default = "aitchisonaitken", tags = "train"),
        okertype = p_fct(c("liracine", "wangvanryzin"), default = "liracine", tags = "train"),
        nmulti = p_int(0, tags = "train"),
        remin = p_lgl(default = TRUE, tags = "train"),
        itmax = p_int(0, default = 10000, tags = "train"),
        ftol = p_dbl(0, default = 1e1 * sqrt(.Machine$double.eps)),
        tol = p_dbl(0, default = 1e4 * sqrt(.Machine$double.eps)),
        small = p_dbl(0, default = 1e3 * sqrt(.Machine$double.eps)),
        min_bandwidth = p_dbl(0, tags = "train", default = 0),  # not part of the package.
        sampling_bw_factor = p_dbl(0, tags = "predict")  # oversmoothing bandwidth factor for sampling
      )
      super$initialize(
        id = "density.np",
        feature_types = c("numeric", "factor", "ordered"),
        predict_types = "prob",
        packages = "np",
        param_set = param_set,
        properties = "sample",
        man = "miesmuschel::mlr_learners_density.np"
      )
    }
  ),
  private = list(
    .train = function(task) {
      pv = self$param_set$get_values(tags = "train")
      pv$min_bandwidth <- pv$min_bandwidth %??% 0
      dat = task$data()
      np::npseed(as.integer(runif(1, -2^31 + 1, 2^31 - 1)))
      bw = invoke(np::npudensbw, dat = dat, .args = pv)
      bw$call = NULL

      bw$bw[bw$bw < pv$min_bandwidth] <- pv$min_bandwidth
      bw$bandwidth$x[bw$bandwidth$x < pv$min_bandwidth] <- pv$min_bandwidth

      list(bw = bw, dat = dat)
    },
    .predict = function(task) {
      list(prob = stats::fitted(np::npudens(bws = self$model$bw, tdat = self$model$dat, edat = task$data())))
    },
    .sample = function(n) {
      bw = self$model$bw
      pv = self$param_set$get_values(tags = "predict")
      bw_factor <- pv$sampling_bw_factor %??% 1

      bw$bw <- bw$bw * bw_factor
      bw$bandwidth$x <- bw$bandwidth$x * bw_factor

      if (!identical(bw$ckerorder, 2)) stop("Can only sample with kernel order 2.")
      # gaussian kernel: rnorm
      # epanechnikov kernel: repanechnikov
      prototypes = self$model$dat[sample.int(nrow(self$model$dat), replace = TRUE)]
      csampler = switch(bw$ckertype, epanechnikov = repanechnikov, gaussian = rnorm)
      dt = as.data.table(Map(function(dim, bandwidth, type) {
        if (type == "numeric") return(csampler(n, dim, bandwidth))
        result = dim
        cons = switch(type, factor = factor, ordered = ordered, stopf("Unsupported feature type %s", type))
        samplefrom = cons(levels(dim), levels = levels(dim))
        for (l in levels(dim)) {
          xlevel = which(dim == l)
          if (!length(xlevel)) next
          sweights = np::npksum(bw, txdat = dim[xlevel[[1]]], exdat = samplefrom)$ksum
          result[xlevel] = samplefrom[sample.int(length(xlevel), length(sweights), replace = TRUE, prob = sweights)]
        }
        result
      }, prototypes, bw$bw, self$state$train_task$col_info[colnames(prototypes)]$type))
      colnames(dt) = colnames(self$model$dat)
    }
  )
)


depanechnikov <- function(x, location = 0, scale = 1) {
  z = (x - location) / scale
  3 * (1 - z^2 / 5) / (4 * sqrt(5) * scale)
}

pepanechnikov <- function(q, location = 0, scale = 1) {
  z = (q - location) / scale
  3 * (z - z^3 / 15) / (4 * sqrt(5))
}

qepanechnikov <- function(p, location = 0, scale = 1) {
  z <- p * 2 - 1
  theta <- atan2(sqrt(1 - z^2), -z) / 3
  sqrt(5) * (sqrt(3) * sin(theta) - cos(theta)) * scale + location
}

repanechnikov <- function(n, location = 0, scale = 1) {
  qepanechnikov(runif(n), location = location, scale = scale)
}
