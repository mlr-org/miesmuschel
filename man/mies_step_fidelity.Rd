% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mies_methods.R
\name{mies_step_fidelity}
\alias{mies_step_fidelity}
\title{Re-Evaluate Existing Configurations with Higher Fidelity}
\usage{
mies_step_fidelity(
  inst,
  budget_id,
  fidelity,
  current_gen_only = FALSE,
  fidelity_monotonic = TRUE,
  additional_components = NULL
)
}
\arguments{
\item{inst}{(\code{OptimInstance})\cr
Optimization instance to evaluate.}

\item{budget_id}{(\code{character(1)})\cr
Budget component that is set to the \code{fidelity} value.}

\item{fidelity}{(\code{atomic(1)})\cr
Atomic scalar indicating the value to be assigned to the \code{budget_id} component of offspring.}

\item{current_gen_only}{(\code{logical(1)})\cr
Whether to only re-evaluate survivors individuals generated in the latest generation (\code{TRUE}), or re-evaluate all currently alive
individuals (\code{FALSE}). In any case, only individuals that were not already evaluated with the chosen fidelity are evaluated,
so this will usually only have an effect when the fidelity of surviving individuals changed between generations.}

\item{fidelity_monotonic}{(\code{logical(1)})\cr
Whether to only re-evaluate configurations for which the fidelity would increase. Default \code{TRUE}.}

\item{additional_components}{(\code{\link[paradox:ParamSet]{ParamSet}} | \code{NULL})\cr
Additional components to optimize over, not included in \code{search_space}, but possibly used for self-adaption. This must be the \code{\link[paradox:ParamSet]{ParamSet}}
of \code{mies_init_population()}'s \code{additional_component_sampler} argument.}
}
\value{
\link{invisible} \code{\link[data.table:data.table]{data.table}}: the performance values returned when evaluating the \code{offspring} values
through \code{eval_batch}.
}
\description{
As part of the "rolling-tide" multifidelity-setup, do reevaluation of configurations with
higher fidelity that have survived lower-fidelity selection. The evaluations are done as part of the
\emph{current} generation, so the \code{dob} value is not increased.

This function should only be called when doing rolling-tide multifidelity, and should not be part of the
MIES cycle otherwise.
}
\examples{
library("bbotk")
lgr::threshold("warn")

# Define the objective to optimize
objective <- ObjectiveRFun$new(
  fun = function(xs) {
    z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
    list(Obj = z)
  },
  domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4)),
  codomain = ps(Obj = p_dbl(tags = "maximize"))
)

# Get a new OptimInstance
oi <- OptimInstanceSingleCrit$new(objective,
  terminator = trm("evals", n_evals = 100)
)

budget_id = "y"

# Create an initial population with fidelity ("y") value 1
mies_init_population(oi, mu = 2, budget_id = budget_id, fidelity = 1)

oi$archive

# Re-evaluate these individuals with higher fidelity
mies_step_fidelity(oi, budget_id = budget_id, fidelity = 2)

oi$archive

# The following creates a new generation without killing the initial
# generation
offspring = data.frame(x = 0:1)
mies_evaluate_offspring(oi, offspring = offspring, budget_id = budget_id,
  fidelity = 3)

oi$archive

# Re-evaluate only individuals from last generation by setting current_gen_only
mies_step_fidelity(oi, budget_id = budget_id, fidelity = 4,
  current_gen_only = TRUE)

oi$archive

# Default: Re-evaluate all that *increase* fidelity: Only the initial
# population is re-evaluated here.
mies_step_fidelity(oi, budget_id = budget_id, fidelity = 3)

oi$archive

# To also re-evaluate individuals with *higher* fidelity, use
# 'fidelity_monotonic = FALSE'. This does not re-evaluate the points that already have
# the requested fidelity, however.
mies_step_fidelity(oi, budget_id = budget_id, fidelity = 3, fidelity_monotonic = FALSE)

oi$archive
}
\seealso{
Other mies building blocks: 
\code{\link{mies_evaluate_offspring}()},
\code{\link{mies_generate_offspring}()},
\code{\link{mies_get_fitnesses}()},
\code{\link{mies_init_population}()},
\code{\link{mies_select_from_archive}()},
\code{\link{mies_survival_comma}()},
\code{\link{mies_survival_plus}()}
}
\concept{mies building blocks}
