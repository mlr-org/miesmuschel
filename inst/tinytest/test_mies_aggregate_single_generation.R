


## objective <- ObjectiveRFun$new(
##   fun = function(xs) {
##     list(y1 = xs$x1, y2 = xs$x2)
##   },
##   domain = ps(x1 = p_dbl(0, 1), x2 = p_dbl(-1, 0)),
##   codomain = ps(y1 = p_dbl(0, 1, tags = "maximize"),
##     y2 = p_dbl(-1, 0, tags = "minimize"))
## )

## oi <- OptimInstanceMultiCrit$new(objective, terminator = trm("none"))

## mies_aggregate_single_generation(oi$archive, identity)

## mies_aggregate_single_generation(oi$archive, function(fitnesses) fitnesses)


## mies_init_population(oi, 2, budget_id = "x1", fidelity = .5)

## oi$archive$data

## mies_aggregate_single_generation(oi$archive, function(fitnesses) fitnesses)

## # notice how fitnesses are positive, since x2 is scaled with -1.
## # To get the original objective-values, use objectives_unscaled:
## mies_aggregate_single_generation(oi$archive,
##   function(objectives_unscaled) objectives_unscaled)

## # When `...` is used, all information is passed:
## mies_aggregate_single_generation(oi$archive, function(...) names(list(...)))

## # generation 10 is not present, but individuals with eol `NA` are still
## # considered alive:
## mies_aggregate_single_generation(oi$archive, function(fitnesses) fitnesses,
##   generation = 10)

## # re-evaluating points with higher "fidelity" (x1)
## mies_step_fidelity(oi, budget_id = "x1", fidelity = 0.7)

## oi$archive$data
## # lower-fidelity values are considered dead now, even for generation 1:
## mies_aggregate_single_generation(oi$archive, function(fitnesses) fitnesses,
##   generation = 1)

## # this adds two new alive individuals at generation 2.
## # Also the individuals from gen 1 are reevaluated with fidelity 0.8
## mies_evaluate_offspring(oi, offspring = data.table(x2 = c(-0.1, -0.2)),
##   budget_id = "x1", fidelity = 0.9, reevaluate_fidelity = 0.8)

## mies_aggregate_single_generation(oi$archive, function(fitnesses) fitnesses,
##   generation = 1)

## mies_aggregate_single_generation(oi$archive, function(fitnesses) fitnesses,
##   generation = 2)

## mies_aggregate_single_generation(oi$archive, function(fitnesses) fitnesses,
##   generation = 2, include_previous_generations = TRUE)


## # typical use-case: get dominated hypervolume
## mies_aggregate_single_generation(oi$archive, function(fitnesses) domhv(fitnesses))

## # get generation-wise mean fitness values
## mies_aggregate_single_generation(oi$archive, function(fitnesses) {
##   apply(fitnesses, 2, mean)
## })
