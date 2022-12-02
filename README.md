
# miesmuschel: Mixed Integer Evolution Strategies

[![check](https://github.com/mlr-org/miesmuschel/actions/workflows/check.yml/badge.svg)](https://github.com/mlr-org/miesmuschel/actions/workflows/check.yml)
[![Coverage](https://codecov.io/github/mlr-org/miesmuschel/branch/master/graphs/badge.svg)](https://codecov.io/github/mlr-org/miesmuschel)
[![CRAN Status Badge](https://www.r-pkg.org/badges/version/miesmuschel)](https://CRAN.R-project.org/package=miesmuschel)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/miesmuschel)](https://CRAN.R-project.org/package=miesmuschel)

## What does it do?

`miesmuschel` provides evolutionary black box optimization algorithms, building on the [`bbotk`](https://bbotk.mlr-org.com/) package. `miesmuschel` offers both ready-to-use optimization algorithms, as well as their fundamental building blocks that can be used to manually construct specialized optimization loops.

## What does it do? (poem version)

(also giving a hint on how to pronounce "`miesmuschel`")

The R software package `miesmuschel`  
Offers opt-algorithms, so crucial  
[`bbotk`](https://bbotk.mlr-org.com/) is its base, it's a powerful tool  
For optimization, it's truly no fool  

Ready-made or D-I-Y, the choice is yours  
With `miesmuschel`, your options are wide open doors  
So when you do optimize, just give it a try  
With `miesmuschel`, surely, success is nigh!

## Project Status

Although `miesmuschel` is currently still evolving, it can already be used for optimization. All exported functions are [thoroughly documented](https://mlr-org.github.io/miesmuschel/reference/index.html).

## Installation

Install the github version, using `remotes`:

```r
remotes::install_github("mlr-org/miesmuschel")
```

## Some Code to Get Started

### Preparation

```r
library("bbotk")
library("paradox")
lgr::threshold("warn")

objective <- ObjectiveRFun$new(
  fun = function(xs) {
    z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
    list(Obj = z)
  },
  domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4)),
  codomain = ps(Obj = p_dbl(tags = "maximize"))
)
```

### Using `bbotk::Optimizer` Object

This is the recommended way of using `miesmuschel`.

```r
# Get a new OptimInstance
oi <- OptimInstanceSingleCrit$new(objective,
  terminator = trm("evals", n_evals = 100)
)

library("miesmuschel")

# Get operators
op.m <- mut("gauss", sdev = 0.1)
op.r <- rec("xounif", p = .3)
op.parent <- sel("random")
op.survival <- sel("best")

# Create OptimizerMies object
mies <- opt("mies", mutator = op.m, recombinator = op.r,
  parent_selector = op.parent, survival_selector = op.survival,
  mu = 3, lambda = 2)

# mies$optimize performs MIES optimization and returns the optimum
mies$optimize(oi)
#>           x        y  x_domain      Obj
#> 1: 1.935055 1.973867 <list[2]> 1.990703
```

### Using `mies_*` Functions Directly

This gives more flexibility when designing ES algorithms, but it is also more verbose and error-prone.

```r
# Get a new OptimInstance
oi <- OptimInstanceSingleCrit$new(objective,
  terminator = trm("evals", n_evals = 100)
)

library("miesmuschel")

# Get operators
op.m <- mut("gauss", sdev = 0.1)
op.r <- rec("xounif", p = .3)
op.parent <- sel("random")
op.survival <- sel("best")

# Prime operators
mies_prime_operators(list(op.m), list(op.r), list(op.parent, op.survival),
  search_space = oi$search_space)

# Sample first generation
mies_init_population(oi, 3)

# This is the first generation
oi$archive$data[, .(x, y, Obj, dob, eol)]
#>             x          y        Obj dob eol
#> 1:  3.8516312  1.2386550 0.03633278   1  NA
#> 2: -1.6478480 -0.9080712 0.02901343   1  NA
#> 3: -0.4215587  0.8250017 0.42529339   1  NA

# Select parents, recombine, mutate
offspring <- mies_generate_offspring(oi, 2, op.parent, op.m, op.r)

# This is the first offspring population
offspring
#>            x           y
#> 1:  2.762783 -0.24885684
#> 2: -1.439780 -0.05699817

# Evaluate offspring (and append to oi archive)
mies_evaluate_offspring(oi, offspring)

# State of the archive now: Second generation has `dob` == 2 
oi$archive$data[, .(x, y, Obj, dob, eol)]
#>             x           y         Obj dob eol
#> 1:  3.8516312  1.23865501 0.036332776   1  NA
#> 2: -1.6478480 -0.90807120 0.029013430   1  NA
#> 3: -0.4215587  0.82500173 0.425293387   1  NA
#> 4:  2.7627829 -0.24885684 0.007566604   2  NA
#> 5: -1.4397798 -0.05699817 0.125404228   2  NA

# Selecto for survival
mies_survival_plus(oi, 3, op.survival)

# Survivors have `eol` NA, two individuals 'died' in generation 2
oi$archive$data[, .(x, y, Obj, dob, eol)]
#>             x           y         Obj dob eol
#> 1:  3.8516312  1.23865501 0.036332776   1  NA
#> 2: -1.6478480 -0.90807120 0.029013430   1   2
#> 3: -0.4215587  0.82500173 0.425293387   1  NA
#> 4:  2.7627829 -0.24885684 0.007566604   2   2
#> 5: -1.4397798 -0.05699817 0.125404228   2  NA

# Perform MIES loop until terminated. This gives an expected `terminated` error
repeat {
  offspring <- mies_generate_offspring(oi, 2, op.parent, op.m, op.r)
  mies_evaluate_offspring(oi, offspring)
  mies_survival_plus(oi, 3, op.survival)
}

# Best result:
oi$archive$data[which.max(Obj)]
#>           x        y dob eol      Obj  x_domain           timestamp batch_nr
#> 1: 2.021887 2.164051  46  NA 1.946115 <list[2]> 2021-02-15 01:56:23       46
```

