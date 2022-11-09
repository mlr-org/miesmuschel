#' @title Perform Nondominated Sorting
#'
#' @description
#' Assign elements of `fitnesses` to nondominated fronts.
#'
#' The first nondominated front is the set of individuals that is not dominated by any other
#' individual with respect to any fitness dimension, i.e. where no other individual exists that
#' has all fitness values greater or equal, with at least one fitness value strictly greater.
#'
#' The n'th nondominated front is the set of individuals that is not dominated by any other
#' individual that is not in any nondominated front with smaller n.
#'
#' Fitnesses are *maximized*, so the individuals in lower numbered nondominated fronts tend
#' to have higher fitness values.
#'
#' @template param_fitnesses
#' @param epsilon (`numeric`)\cr
#'   Epsilon-vaue for non-dominance. A value is epsilon-dominated by another if it is at least `epsilon` smaller than
#'   the other in all dimensions, and more than `epsilon` smaller than the other in one dimension. `epsilon` may
#'   be a scalar, in which case it is used for all dimensions or a vector, in which case its length must match
#'   the number of dimensions. Default 0.
#'
#' @return `list`: `$front`: Vector assigning each individual in `fitnesses` its nondominated front.
#'   `$domcount`: Length N vector counting the number of individuals that dominate the given individual.
#'
#' @export
rank_nondominated = function(fitnesses, epsilon = 0) {
  # this may or may not be similar to https://core.ac.uk/download/pdf/30341871.pdf ; haven't read the paper,
  # but at first glance it looks like what I'm doing here so I don't think I am doing something dumb.
  # They suggest binary search for fronts, which is probably a neat idea.

  # For large matrices this seems to be a little faster than ecr's C code
  assert_matrix(fitnesses, mode = "numeric", any.missing = FALSE, min.cols = 1, min.rows = 1)
  assert(check_number(epsilon, lower = 0, finite = TRUE), check_numeric(epsilon, lower = 0, len = ncol(fitnesses), finite = TRUE, any.missing = FALSE))
  fitnesses_t = t(fitnesses)
  ordering = do.call(order, c(as.data.frame(unname(fitnesses)), list(decreasing = TRUE)))  # ordering along first dimension, tie break 2nd dimension, tie break .. nth dimension
  fronts = list()  # matrices of nondominated fronts. The matrices here are transposed for more efficient comparison (so they have ncol(fitnesses) rows)
  frontindices = list()  # row indices of elements of nondominated fronts
  assignedfronts = numeric(length(ordering))  # assigned front to each individual
  domcount = numeric(length(ordering))  # number of individuals that dominate individuals
  lastdomcount = 1  # number of individuals from the last considered front that dominate this individual ; init to 1 so in first loop we create a front
  for (orow in ordering) {  # go through ordered rows, with highest according to 1st dimension first
    numdomd = 0  # number of individuals that dominate this individual
    rowvals = fitnesses_t[, orow]
    epsrowvals = rowvals + epsilon
    for (ifront in seq_along(fronts)) {  # go from first to last front, check if individual is nondominated there
      # dominated: *all* fitnesses greater or equal than, at least one actually greater than
      # The following line is the "hotspot" of this function and makes up ~90% of the runtime
      # 'colSums(...) > 0' is used in place of 'any()'
      lastdomcount = sum(colSums(fronts[[ifront]] < epsrowvals) == 0 & colSums(fronts[[ifront]] > epsrowvals) > 0)
      numdomd = numdomd + lastdomcount
      if (!lastdomcount) break
    }
    if (lastdomcount) {
      # dominated by all fronts --> opening a new front
      ifront = length(fronts) + 1
      fronts[[ifront]] = matrix(rowvals)
      frontindices[[ifront]] = orow
    } else {
      # nondominated by front <ifront> --> appending to this front
      curfi = frontindices[[ifront]]  # indices of individuals in curfi
      curfi[[length(curfi) + 1]] = orow  # add current index to it
      frontindices[[ifront]] = curfi  # save changed list
      fronts[[ifront]] = fitnesses_t[, curfi, drop = FALSE]  # also create the matrix
    }
    assignedfronts[[orow]] = ifront
    domcount[[orow]] = numdomd
  }
  list(fronts = assignedfronts, domcount = domcount)
}

#' @title Calculate Crowding Distance
#'
#' @description
#' Takes a `matrix` of fitness values and calculates the crowding distance for individuals in that `matrix`.
#'
#' Individuals that are minimal or maximal with respect to at least one dimension are assigned infinite
#' crowding distance.
#'
#' Individuals are assumed to be in a (epsilon-) nondominated front.
#'
#' @template param_fitnesses
#'
#' @return `numeric`: Vector of crowding distances.
#'
#' @export
dist_crowding = function(fitnesses) {
  assert_matrix(fitnesses, mode = "numeric", any.missing = FALSE, min.cols = 1, min.rows = 1)

  result = numeric(nrow(fitnesses))

  for (col in as.data.frame(unname(fitnesses))) {
    o = order(col)
    dists = diff(col[o])
    result[o] = result[o] + c(Inf, dists) + c(dists, Inf)
  }
  result
}

#' @title Calculate Hypervolume Contribution
#'
#' @description
#' Takes a `matrix` of fitness values and calculates the hypervolume contributions of individuals in that `matrix`.
#'
#' Hypervolume contribution of an individual I is the difference between the dominated hypervolume of a set of
#' individuals including I, where the fitness of I is increased by `epsilon`, and the dominated hypervolume of
#' the same set but excluding I.
#'
#' Individuals that are less than another individual more than `epsilon` in any dimension have hypervolume contribution
#' of 0.
#'
#' @template param_fitnesses
#' @param nadir (`numeric`)\cr
#'   Lowest fitness point up to which to calculate dominated hypervolume. May be a scalar, in which case
#'   it is used for all dimensions, or a vector, in which case its length must match the number of dimensions.
#'   Default 0.
#' @param epsilon (`numeric`)\cr
#'   Added to each individual before calculating its particular hypervolume contribution. `epsilon` may
#'   be a scalar, in which case it is used for all dimensions, or a vector, in which case its length must match
#'   the number of dimensions. Default 0.
#' @return `numeric`: The vector of dominated hypervolume contributions for each individual in `fitnesses`.
#'
#' @examples
#' (fitnesses = matrix(c(1, 5, 2, 3, 0, 3, 1, 0, 10, 8), ncol = 2))
#'
#' # to see the fitness matrix, use:
#' ## plot(fitnesses, pch = as.character(1:5))
#'
#' domhv_contribution(fitnesses)
#' @export
domhv_contribution = function(fitnesses, nadir = 0, epsilon = 0) {
  assert_matrix(fitnesses, mode = "numeric", any.missing = FALSE, min.cols = 1, min.rows = 1)
  assert(check_number(nadir, lower = 0, finite = TRUE), check_numeric(epsilon, lower = 0, len = ncol(fitnesses), finite = TRUE, any.missing = FALSE))
  assert(check_number(epsilon, lower = 0, finite = TRUE), check_numeric(epsilon, lower = 0, len = ncol(fitnesses), finite = TRUE, any.missing = FALSE))
  has_epsilon = any(epsilon > 0)

  nd = nondominated(fitnesses, epsilon = epsilon)
  nonzeroes = nd$front
  strongfront = fitnesses[nd$strong_front, , drop = FALSE]
  result = numeric(nrow(fitnesses))

  volume_baseline = domhv(strongfront, nadir, prefilter = FALSE)

  result[nonzeroes] = map_dbl(nonzeroes, function(nz) {
    replace = which(nz == nd$strong_front)
    if (has_epsilon) {
      if (length(replace)) {
        strongfront[replace, ] = strongfront[replace, ] + epsilon
      } else {
        strongfront = rbind(strongfront, fitnesses[nz, ] + epsilon)
      }
      volume_with = domhv(strongfront, nadir, prefilter = FALSE)
    } else {
      volume_with = volume_baseline
    }
    if (length(replace)) {
      volume_without = domhv(strongfront[-replace, , drop = FALSE], nadir, prefilter = FALSE)
    } else {
      volume_without = volume_baseline
    }
    volume_with - volume_without
  })
  result
}

#' @title Calculate Hypervolume Improvement
#'
#' @description
#' Takes a `matrix` of fitness values and calculates the hypervolume improvement of individuals in that `matrix`, one by one,
#' over the `baseline` individuals.
#'
#' The hypervolume improvement for each point is the measure of all points that have fitnesses that are
#' * greater than the respective value in `nadir` in all dimensions, and
#' * smaller than the respective value in the given point in all dimensions, and
#' * greater than all points in `baseline` in at least one dimension.
#'
#' Individuals in `fitnesses` are considered independently of each other. A possible speedup is achieved because
#' `baseline` individuals only need to be pre-filtered once.
#'
#' @template param_fitnesses
#' @param baseline (`matrix` | `NULL`)\cr
#'   Fitness-matrix with one column per objective, giving a population over which the hypervolume improvement should be calculated.
#'   If `NULL`, the hypervolume of each individual in `fitnesses` is calculated.
#' @param nadir (`numeric`)\cr
#'   Lowest fitness point up to which to calculate dominated hypervolume. May be a scalar, in which case
#'   it is used for all dimensions, or a vector, in which case its length must match the number of dimensions.
#'   Default 0.
#' @return `numeric`: The vector of dominated hypervolume contributions for each individual in `fitnesses`.
#' @examples
#' (fitnesses = matrix(c(1, 5, 2, 3, 0, 3, 1, 0, 10, 8), ncol = 2))
#'
#' # to see the fitness matrix, use:
#' ## plot(fitnesses, pch = as.character(1:5))
#'
#' domhv_improvement(fitnesses)
#'
#' domhv_improvement(fitnesses, fitnesses[1, , drop = FALSE])
#' @export
domhv_improvement = function(fitnesses, baseline = NULL, nadir = 0) {
  assert_matrix(fitnesses, mode = "numeric", any.missing = FALSE, min.cols = 1, min.rows = 1)
  assert(check_number(nadir, finite = TRUE), check_numeric(nadir, finite = TRUE, any.missing = FALSE, len = ncol(fitnesses)))

  if (is.null(baseline) || test_matrix(baseline, mode = "numeric", any.missing = FALSE, ncols = ncol(fitnesses), nrows = 0)) {
    return(apply(fitnesses, 1, function(fi) {
      prod(pmax(fi - nadir, 0))
    }))
  }

  assert_matrix(baseline, mode = "numeric", any.missing = FALSE, ncols = ncol(fitnesses))

  baseline = baseline[nondominated(baseline)$strong_front, , drop = FALSE]
  blt = t(baseline)
  blhv = domhv(baseline, nadir = nadir, prefilter = FALSE)
  apply(fitnesses, 1, function(fi) {
    # no HV improvement if:
    # * there is an individual in baseline (i.e. blt) that has all values >= the fitness individual
    # * fi is worse or equal nadir in any dimension
    if (any(colSums(blt < fi) == 0) || any(nadir >= fi)) {
      return(0)
    }
    domhv(rbind(baseline, fi), nadir = nadir, prefilter = FALSE) - blhv
  })
}

domhv = function(fitnesses, nadir = 0, prefilter = TRUE, on_worse_than_nadir = "warn") {
  assert_matrix(fitnesses, mode = "numeric", any.missing = FALSE, min.cols = 1, min.rows = 1)
  dim = ncol(fitnesses)
  assert(check_number(nadir, finite = TRUE), check_numeric(nadir, len = dim, any.missing = FALSE, finite = TRUE))
  assert_choice(on_worse_than_nadir, c("quiet", "warn", "stop"))

  if (any(t(fitnesses) < nadir)) {
    switch(on_worse_than_nadir, quiet = identity, warn = warning, stop = stop)("Found fitness worse than nadir")
    fitnesses = fitnesses[colSums(t(fitnesses) < nadir) == 0, , drop = FALSE]
  }

  if (prefilter) {
    fitnesses = fitnesses[nondominated(fitnesses)$strong_front, , drop = FALSE]
#    cat("prefiltered:\n")
#    print(fitnesses)
  }
  fitnesses_t = t(fitnesses)

  zenith = apply(fitnesses, 2, max)
  if (length(nadir) == 1) nadir = rep(nadir, dim)

  weight_lut = 2^(seq_len(dim) / dim)
  weight_lut[[1]] = 0  # first dimension is not regarded for weight
  weight_lut = c(weight_lut, weight_lut)

  domhv_recurse = function(fitnesses_t, nadir, zenith, dimension) {
    # cat(sprintf("\nFrom %s to %s dim %s:\n", str_collapse(round(nadir, 4)), str_collapse(round(zenith, 4)), dimension))
    # print(round(t(fitnesses_t), 4))

    above = colSums(fitnesses_t >= zenith)
    if (any(above == dim)) {
      # one point completely dominates the current window --> no volume
      # cat("completely dominated, returning 0\n")
      return(0)
    }
    cutting = above == dim - 1L

    cutcube = fitnesses_t[, cutting, drop = FALSE]

    # see if we need to do "simplify". if no -> done with this loop
    if (length(cutcube)) {
      # the points that go beyond zenith stay at nadir (because they are not what we care about)
      # but the points that do not indicate how much we cut away. From them we take the max, because
      # there could be multiple orthants that cut away space.
      # (We always cut away from below, because we have (-inf)^d-orthants)
      cutcube[cutcube >= zenith] = -Inf
      nadir = pmax(nadir, matrixStats::rowMaxs(cutcube))  # relatively lean dependency
      #> nadir = pmax(nadir, apply(cutcube, 1L, max))  # base R
      #> nadir = pmax(nadir, Rfast::rowMaxs(cutcube, value = TRUE))

      fitnesses_t = fitnesses_t[, !cutting, drop = FALSE]

      # for each simplify step, we need to check again if things fall out of the current window
      below = colSums(fitnesses_t <= nadir) != 0
      #> below = Rfast::colAny(fitnesses_t <= nadir)  # doesn't seem to add much

      # left out below the nadir --> no influence on current window
      if (any(below)) {
        fitnesses_t = fitnesses_t[, !below, drop = FALSE]
      }
      if (!length(fitnesses_t)) {
        # cat("nothing left, returning full\n")
        return(prod(zenith - nadir))
      }
    }

    # one point left: subtract its volume from entire volume
    if (ncol(fitnesses_t) == 1L) {
      # cat("one point left, returning its volume\n")
      return(prod(zenith - nadir) - prod(pmin(zenith, fitnesses_t) - nadir))  # I don't like what this does to numerics
    }

    repeat {
      dimpoints = fitnesses_t[dimension, ]
      which.cutpoints = which(dimpoints < zenith[dimension])
      if (length(which.cutpoints)) break
      dimension = (dimension %% dim) + 1L
    }

    cutpoints = dimpoints[which.cutpoints]

    # Maybe a different cut is more appropriate here, because more points fall out on the upper half.
    #  - points "below" are eliminated 100% points "above" are eliminated only if they have one dimension left
    #  - on the other hand the majority case could be that most points have only one dimension overlap
    #> cutat = Rfast::nth(cutpoints, (length(cutpoints) + 1L) / 2)  # not the correct way of doing this, because it doesn't really count the (d-2)-faces *within* the bounds.
    # interpolate = TRUE in the following because we never want to get the maximum element.
    if (length(cutpoints) == 1) {
      cutat = cutpoints
    } else if (length(cutpoints) == 2) {
      cutat = min(cutpoints)
    } else {

      # calculate weights; disregard dimension itself by subtracting its own weight again at the end.
      cutpointweights = colSums((fitnesses_t[, which.cutpoints, drop = FALSE] < zenith) * weight_lut[seq.int(dim + 1 - dimension, length.out = dim)])

      cutat = matrixStats::weightedMedian(cutpoints, cutpointweights, interpolate = FALSE, ties = "min")  # n log(n), apparently does sorting internally, pathetic.
      if (cutat == max(cutpoints)) {
        otherpoints = cutpoints[cutpoints != cutat]
        if (length(otherpoints)) {
          cutat = max(otherpoints)
        }
      }
    }
    #> cutat = sort(cutpoints)[(length(cutpoints) + 1L) / 2]
    ## alternative: sort partially
    #> cutrank = as.integer((length(cutpoints) + 1L) / 2)
    #> cutat = sort(cutpoints, partial = cutrank)[[cutrank]]

    cutnadir = nadir
    cutnadir[dimension] = cutat
    cutzenith = zenith
    cutzenith[dimension] = cutat

    dimension = (dimension %% dim) + 1L

    # pre-emptively drop lower part for upper half
    fitnesses_upper = fitnesses_t[, dimpoints > cutat, drop = FALSE]
    if (length(fitnesses_upper)) {
      # cat("recursing upper\n")
      result_upper = domhv_recurse(fitnesses_upper, cutnadir, zenith, dimension)
    } else {
      result_upper = prod(zenith - cutnadir)
    }
    # cat("recursing lower\n")
    result_upper + domhv_recurse(fitnesses_t, nadir, cutzenith, dimension)
  }
# problem seems to be upper of first lower: doesn't exist for clash.
  prod(zenith - nadir) - domhv_recurse(fitnesses_t, nadir, zenith, 1)
}

# similar to rank_nondominated, but return only the first front.
# @return named `list`: `front` (`integer`): indices of epsilon-nondominated front; `strong_front` (`integer`): indices of dominating front (without epsilon)
nondominated = function(fitnesses, epsilon = 0) {
  assert_matrix(fitnesses, mode = "numeric", any.missing = FALSE, min.cols = 1, min.rows = 1)
  assert(check_number(epsilon, lower = 0, finite = TRUE), check_numeric(epsilon, lower = 0, len = ncol(fitnesses), any.missing = FALSE, finite = TRUE))
  fitnesses_t = t(fitnesses)
  ordering = do.call(order, c(as.data.frame(unname(fitnesses)), list(decreasing = TRUE)))  # ordering along first dimension, tie break 2nd dimension, tie break .. nth dimension
  curfi = ordering[[1]]  # row indices of elements of nondominated fronts
  curfi_strong = ordering[[1]]  # front of dominating individuals
  front = fitnesses_t[, curfi, drop = FALSE]  # matrix of nondominated front, for quick comparison
  for (orow in ordering[-1]) {  # go through ordered rows, with highest according to 1st dimension first. (But skip the actual highest because we already added it.)
    rowvals = fitnesses_t[, orow]
    epsrowvals = rowvals + epsilon
    # dominated: *all* fitnesses greater or equal than, at least one actually greater than
    # The following line is the "hotspot" of this function and makes up ~90% of the runtime
    # 'colSums(...) > 0' is used in place of 'any()'
    dominated = any(colSums(front < epsrowvals) == 0 & colSums(front > epsrowvals) > 0)
    if (!dominated) {
      # strong dominating front
      if (all(colSums(front <= rowvals) > 0)) {
        curfi_strong[[length(curfi_strong) + 1]] = orow
        front = fitnesses_t[, curfi_strong, drop = FALSE]  # also create the matrix
      }

      # nondominated by front --> appending to front
      curfi[[length(curfi) + 1]] = orow  # add current index to it

    }
  }
  list(front = curfi, strong_front = curfi_strong)
}
