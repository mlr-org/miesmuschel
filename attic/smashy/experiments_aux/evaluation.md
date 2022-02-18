# How does SMASHY evaluation work?

## Relevant File Contents
 - **miesmuschel**: depends on special paradox branch: `remotes::install_github("mlr-org/paradox@expression_params")`.
 - **[optim2.R](optim2.R)**: Bridge between miesmuschel and everything else:
     - **`get_searchspace()`**: search spaces
     - **`suggested_meta_domain`**: meta-domain
     - **`setup_smashy()`**: Creates a setup `OptimizerSmashy` instance from given metaconfig
     - **`get_meta_objective()`**: Creates an objective function that returns performance values
     - **`get_meta_objective_from_surrogate()`**: Creates objective function from surrogate object, using available meta-information
 - **[load_objectives2.R](load_objectives2.R)**: Less important than it used to be.
     - **`evaluate_miesmuschel()`**: Is an objective function that takes only atomic inputs, to be called by parallelization.
 - **[mbo.R](mbo.R)**: To be called by commandline, does the optimization.
 - **[optim2demo.R](optim2demo.R)**: demo objectives
 - **[optim_experiments.R](optim_experiments.R)**: Interactive experiments with optimization

