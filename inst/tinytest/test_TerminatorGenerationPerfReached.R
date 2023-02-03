
## set.seed(1)
## library("bbotk")
## lgr::threshold("warn")

## # Terminate when hypervolume with nadir `c(0, 0, ...)`
## # does not improve for 3 generations by at least 0.1:
## tg <- trm("genperfreached",
##   fitness_aggregator = function(fitnesses) domhv(fitnesses),
##   include_previous_generations = TRUE,
##   level = 1
## )

## set.seed(1)
## objective <- ObjectiveRFun$new(
##   fun = function(xs) {
##     list(y1 = xs$x1, y2 = xs$x2)
##   },
##   domain = ps(x1 = p_dbl(0, 1), x2 = p_dbl(-1, 0)),
##   codomain = ps(y1 = p_dbl(0, 1, tags = "maximize"),
##     y2 = p_dbl(-1, 0, tags = "minimize"))
## )

## oi <- OptimInstanceMultiCrit$new(objective, terminator = tg)

## op <- opt("mies",
##   lambda = 4, mu = 4,
##   mutator = mut("gauss", sdev = 0.1),
##   recombinator = rec("xounif"),
##   parent_selector = sel("random"),
##   survival_selector = sel("best", scl("hypervolume"))
## )

## op$optimize(oi)

## # the observed aggregated values:
## oi$archive$data_extra$TerminatorGenerationPerfReached

## # ... or as calculated by mies_generation_apply
## mies_generation_apply(oi$archive, function(fitnesses) {
##   domhv(fitnesses)
## }, include_previous_generations = TRUE)
