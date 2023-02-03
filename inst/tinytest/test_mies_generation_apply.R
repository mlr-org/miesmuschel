
## set.seed(1)
## objective <- ObjectiveRFun$new(
##   fun = function(xs) {
##     list(y1 = xs$x1, y2 = xs$x2)
##   },
##   domain = ps(x1 = p_dbl(0, 1), x2 = p_dbl(-1, 0)),
##   codomain = ps(y1 = p_dbl(0, 1, tags = "maximize"),
##     y2 = p_dbl(-1, 0, tags = "minimize"))
## )
## oi <- OptimInstanceMultiCrit$new(objective,
##   terminator = trm("evals", n_evals = 40))

## op <- opt("mies",
##   lambda = 4, mu = 4,
##   mutator = mut("gauss", sdev = 0.1),
##   recombinator = rec("xounif"),
##   parent_selector = sel("random"),
##   survival_selector = sel("best", scl("hypervolume"))
## )

## op$optimize(oi)

## # Aggregated hypervolume of individuals alive in each gen:
## mies_generation_apply(oi$archive, function(fitnesses) {
##   domhv(fitnesses)
## })

## # Aggregated hypervolume of all points evaluated up to each gen
## # (may be slightly more, since the domhv of more points is evaluated):
## mies_generation_apply(oi$archive, function(fitnesses) {
##   domhv(fitnesses)
## }, include_previous_generations = TRUE)

## # The following are simpler with mies_aggregate_single_generations():
## mies_generation_apply(oi$archive, function(fitnesses) {
##   apply(fitnesses, 2, mean)
## })
## # Compare:
## mies_aggregate_generations(oi, aggregations = list(mean = mean))

## mies_generation_apply(oi$archive, function(objectives_unscaled) {
##   apply(objectives_unscaled, 2, mean)
## })
## # Compare:
## mies_aggregate_generations(oi, aggregations = list(mean = mean),
##   as_fitnesses = FALSE)

