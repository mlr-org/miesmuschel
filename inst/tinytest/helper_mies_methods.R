
library("bbotk")

adversarial_space = ps(budget_id = p_fct(c("generation", "generation_lookahead")), generation = p_int(0, 10), generation_lookahead = p_lgl(),
  current_gen = p_int(0, 10), reeval = p_lgl(), fidelity = p_int(0, 1), next_fidelity = p_int(0, 10), fidelity_monotonic = p_lgl(),
  inst = p_lgl(), rows = p_lgl(), selected = p_lgl())

objective = ObjectiveRFun$new(
  fun = function(xs) {
    z = exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
    list(Obj = z)
  },
  domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4)),
  codomain = ps(Obj = p_dbl(tags = "maximize"))
)

objective_invertible = ObjectiveRFun$new(
  fun = function(xs) {
    z = exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
    if (xs$invert) z = -z
    list(Obj = z)
  },
  domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4), invert = p_lgl()),
  codomain = ps(Obj = p_dbl(tags = "maximize"))
)

objective_budget = ObjectiveRFun$new(
  fun = function(xs) {
    z = exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
    z = z + rnorm(1, sd = 1 / sqrt(xs$b))
    list(Obj = z)
  },
  domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4),
    b = p_int(1, tags = "budget")),
  codomain = ps(Obj = p_dbl(tags = "maximize"))
)

get_objective_passthrough = function(directions, small = FALSE, extra = character(0)) {
  assert_subset(directions, c("minimize", "maximize"))
  plist <- list(
    do.call(paradox::ps, sapply(paste0("p", seq_along(directions), recycle0 = TRUE), function(x) p_dbl(-10, 10), simplify = FALSE)),
    if (!small) adversarial_space,
    do.call(paradox::ps, sapply(extra, function(x) p_dbl(-10, 10), simplify = FALSE))
  )
  plist <- Filter(function(x) !is.null(x), plist)
  ObjectiveRFun$new(
    fun = function(xs) {
      x = xs[paste0("p", seq_along(directions))]
      names(x) = paste0("pout", seq_along(directions))
      x
    },
    domain = miesmuschel:::ps_union(plist),
    codomain = do.call(paradox::ps,
      structure(
        lapply(seq_along(directions), function(x) p_dbl(-10, 10, tags = directions[[x]])),
        names = paste0("pout", seq_along(directions), recycle0 = TRUE)
      )
    )
  )
}

as_oi = function(objective, search_space = NULL, terminator = trm("none")) {
  if (length(unlist(lapply(c("minimize", "maximize"), function(x) objective$codomain$ids(tags = x)))) > 1) {
    OptimInstanceMultiCrit$new(objective, search_space = search_space, terminator = terminator)
  } else {
    OptimInstanceSingleCrit$new(objective, search_space = search_space, terminator = terminator)
  }
}

generate_increasing_sampler = function(param_set) {
  SamplerHierarchical$new(param_set,
    if (miesmuschel:::paradox_s3) {
      lapply(param_set$subspaces(), function(p) Sampler1DRfun$new(param = p, rfun = function(n) seq_len(n)))
    } else {
      lapply(param_set$params, function(p) Sampler1DRfun$new(param = p, rfun = function(n) seq_len(n)))
    }
  )
}

generate_design_increasing = function(param_set, n) {
  generate_increasing_sampler(param_set)$sample(n)
}

generate_design_random_increasing = function(param_set, n) {
  SamplerJointIndep$new(list(
    generate_increasing_sampler(if (miesmuschel:::paradox_s3) param_set$subset("p1") else ps(p1 = param_set$params$p1)),
    SamplerUnif$new(ParamSetShadow$new(param_set, "p1"))
  ))$sample(n)
}
