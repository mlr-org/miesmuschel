


library("bbotk")

lgr::threshold("warn")

objective <- ObjectiveRFun$new(
  fun = function(xs) {
    z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
    list(Obj = z)
  },
  domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4)),
  codomain = ps(Obj = p_dbl(tags = "maximize"))
)

xs <- CJ(x = seq(-2, 4, length.out = 50), y = seq(-2, 4, length.out = 50))
z <- objective$eval_dt(xs)

origin <- data.frame(x = c(1, 2), y = c(2, 1))
output <- data.frame(x = c(1.725405, 2.571289), y = c(0.6657748, 0.6890937))

ggplot(cbind(rbind(origin, output),
  Point = as.factor(1:2), phase = rep(c("Before", "After"), each = 2)),
  aes(x = x, y = y, color = Point, group = Point)) +
  theme_bw() + xlim(-2, 4) + ylim(-2, 4) + coord_fixed() +
  geom_point() +
  geom_path(color = "black", arrow = arrow(length=unit(.3, "cm")))

# ------

oi <- OptimInstanceSingleCrit$new(objective,
  terminator = trm("evals", n_evals = 100)
)

oi$eval_batch(data.table(x = c(1, 2, 3), y = c(2, 2, 1)))
oi$archive


op.m <- mut("gauss", sdev = .2)
space <- ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4))
op.m$prime(space)
data <- data.frame(x = c(1, 2), y = c(2, 1))
op.m$operate(data)

oi$archive$clear()

op.m <- mut("gauss", sdev = .2)
op.r <- rec("xounif", p = .3)
op.parent <- sel("random")
op.survival <- sel("best")

mies_prime_operators(list(op.m), list(op.r), list(op.parent, op.survival),
  oi$search_space)

mies_init_population(oi, 3)

oi$archive$data[, .(x, y, Obj, dob, eol)]

offspring <- mies_generate_offspring(oi, 2, op.parent, op.m, op.r)

offspring

mies_evaluate_offspring(oi, offspring)

oi$archive$data[, .(x, y, Obj, dob, eol)]

mies_survival_plus(oi, 3, op.survival)

oi$archive$data[, .(x, y, Obj, dob, eol)]


offspring <- mies_generate_offspring(oi, 2, op.parent, op.m, op.r)
mies_evaluate_offspring(oi, offspring)
mies_survival_plus(oi, 3, op.survival)
oi$archive$data[, .(x, y, Obj, dob, eol)]

repeat {
  offspring <- mies_generate_offspring(oi, 2, op.parent, op.m, op.r)
  mies_evaluate_offspring(oi, offspring)
  mies_survival_plus(oi, 3, op.survival)
}



library("ggplot2")

ggplot() + theme_bw() + xlim(-2, 4) + ylim(-2, 4) + coord_fixed() +
  geom_contour(data = cbind(xs, z), aes(x = x, y = y, z = Obj)) +
  geom_point(data = oi$archive$data[order(dob)], aes(x = x, y = y, color = dob))

ggplot() + theme_bw() + xlim(-2, 4) + ylim(-2, 4) + coord_fixed() +
  geom_contour(data = cbind(xs, z), aes(x = x, y = y, z = Obj)) +
  geom_path(data = oi$archive$data[order(dob)], aes(x = x, y = y, color = dob)) +
  geom_point(data = oi$archive$data[order(dob)], aes(x = x, y = y, color = dob))


# ----------

op.m <- mut("gauss", sdev = .2)
op.r <- rec("xounif", p = .3)
op.parent <- sel("random")
op.survival <- sel("best")

mies <- OptimizerMies$new(mutator = op.m, recombinator = op.r,
  parent_selector = op.parent, survival_selector = op.survival)

mies <- opt("mies", mutator = op.m, recombinator = op.r,
  parent_selector = op.parent, survival_selector = op.survival)

mies$param_set$values$mu <- 3
mies$param_set$values$lambda <- 2

objective <- ObjectiveRFun$new(
  fun = function(xs) {
    z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
    list(Obj = z)
  },
  domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4)),
  codomain = ps(Obj = p_dbl(tags = "maximize"))
)

oi <- OptimInstanceSingleCrit$new(objective,
  terminator = trm("evals", n_evals = 100)
)

mies$optimize(oi)

# ----------

objective <- ObjectiveRFun$new(
  fun = function(xs) {
    z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
    if (xs$invert) z <- -z
    list(Obj = z)
  },
  domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4), invert = p_lgl()),
  codomain = ps(Obj = p_dbl(tags = "maximize"))
)

oi <- OptimInstanceSingleCrit$new(objective,
  terminator = trm("evals", n_evals = 100)
)

mies$optimize(oi)

op.m <- mut("gauss", sdev = .2)
space <- ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4), invert = p_lgl())
op.m$prime(space)


# ----------



library("mlr3tuning")

op.m <- mut("gauss", sdev = .2)
op.r <- rec("xounif", p = .3)
op.parent <- sel("random")
op.survival <- sel("best")

mies <- tnr("mies", mutator = op.m, recombinator = op.r,
  parent_selector = op.parent, survival_selector = op.survival)


mut("unif")$param_set

mut("null")$param_set


op.mm <- mut("maybe", mutator = mut("gauss"), mutator_not = mut("null"), p = .5)

space <- ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4))
op.mm$prime(space)
data <- data.frame(x = c(1, 1, 1), y = c(1, 1, 1))
op.mm$operate(data)


op.proxy <- mut("proxy")

op.proxy$param_set$values$operation <- mut("gauss", sdev = .2)



space <- ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4))
op.proxy$prime(space)
data <- data.frame(x = c(1, 1, 1), y = c(1, 1, 1))
op.proxy$operate(data)



MutatorCombination$public_methods$initialize



objective <- ObjectiveRFun$new(
  fun = function(xs) {
    z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
    if (xs$invert) z <- -z
    list(Obj = z)
  },
  domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4), invert = p_lgl()),
  codomain = ps(Obj = p_dbl(tags = "maximize"))
)

mut("combine", operators = list(x = mut("gauss"), y = mut("gauss"), invert = mut("unif")))

mut("combine", operators = list(nums = mut("gauss"), invert = mut("unif")),
  groups = list(nums = c("x", "y")))

mut("combine", operators = list(ParamDbl = mut("gauss"), ParamLgl = mut("unif")))

mut("combine", operators = list(nums = mut("gauss"), ParamAny = mut("unif")),
  groups = list(nums = c("x", "ParamDbl")))



objective <- ObjectiveRFun$new(
  fun = function(xs) {
    z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
    if (xs$invert) z <- -z
    list(Obj = z)
  },
  domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4), invert = p_lgl()),
  codomain = ps(Obj = p_dbl(tags = "maximize"))
)

oi <- OptimInstanceSingleCrit$new(objective,
  terminator = trm("evals", n_evals = 100)
)

op.mcmb <- mut("maybe", p = 0.7,
  mutator_not = mut("null"),  # redundant
  mutator = mut("combine", operators = list(
    ParamDbl = mut("gauss", sdev = 0.1, sdev_is_relative = FALSE),
    ParamLgl = mut("cmpmaybe", p = 0.3,
      mutator = mut("unif", can_mutate_to_same = FALSE),
      mutator_not = mut("null")  # redundant
    )
  ))
)

mies <- opt("mies", mutator = op.mcmb, recombinator = op.r,
  parent_selector = op.parent, survival_selector = op.survival,
  mu = 3, lambda = 2)

mies$optimize(oi)


ggplot() + theme_bw() + xlim(-2, 4) + ylim(-2, 4) + coord_fixed() +
  geom_contour(data = cbind(xs, z), aes(x = x, y = y, z = Obj)) +
  geom_path(data = oi$archive$data[order(dob)], aes(x = x, y = y, color = invert)) +
  geom_point(data = oi$archive$data[order(dob)], aes(x = x, y = y, color = invert))

ggplot() + theme_bw() + xlim(-2, 4) + ylim(-2, 4) + coord_fixed() +
  geom_contour(data = cbind(xs, z), aes(x = x, y = y, z = Obj)) +
  geom_point(data = oi$archive$data[order(dob)], aes(x = x, y = y, shape = invert, color = dob), size = 2.5) + scale_shape_manual(values = c(16, 3)) + scale_color_gradientn(colors = c("black", "red", "yellow", "blue"))

ggplot() + theme_bw() + xlim(-2, 4) + ylim(-2, 4) + coord_fixed() +
  geom_contour(data = cbind(xs, z), aes(x = x, y = y, z = Obj)) +
  geom_point(data = oi$archive$data[order(dob)], aes(x = x, y = y, color = invert, alpha = dob)) + scale_alpha(range = c(.4, 1))



ggplot() + theme_bw() + xlim(-2, 4) + ylim(-2, 4) + coord_fixed() +
  geom_point(data = opd , aes(x = x, y = y, color = invert), alpha = 0.3)


op.m.adapt <- mut("combine",
  operators = list(
    ParamDbl = mut("gauss")
  )
)

op.m.adapt$param_set

op.m.adapt <- mut("combine",
  operators = list(
    ParamDbl = mut("gauss")
  ),
  adaptions = list(ParamDbl.sdev = function(x) exp(x$lstep))
)

op.m.adapt <- mut("combine",
  operators = list(
    ParamDbl = mut("gauss"),
    lstep = mut("gauss", sdev = .5, sdev_is_relative = FALSE)
  ),
  adaptions = list(ParamDbl.sdev = function(x) exp(x$lstep))
)

op.m.adapt$prime(ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4), lstep = p_dbl()))

operandum <- data.frame(x = c(0, 0), y = c(0, 0), lstep = log(c(.01, 1)))

operandum
op.m.adapt$operate(operandum)


additional_components <- ps(lstep = p_dbl())

additional_component_sampler <- Sampler1DRfun$new(
  param = additional_components$params[[1]],
  rfun = function(n) rep(log(.1), n)
)

additional_component_sampler$sample(4)

additional_component_sampler$param_set

# ----------

objective <- ObjectiveRFun$new(
  fun = function(xs) {
    z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
    list(Obj = z)
  },
  domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4)),
  codomain = ps(Obj = p_dbl(tags = "maximize"))
)

oi <- OptimInstanceSingleCrit$new(objective,
  terminator = trm("evals", n_evals = 100)
)

op.r <- rec("xounif", p = .3)
op.parent <- sel("random")
op.survival <- sel("best")

op.m.adapt <- mut("combine",
  operators = list(
    ParamDbl = mut("gauss"),
    lstep = mut("gauss", sdev = .5, sdev_is_relative = FALSE)
  ),
  adaptions = list(ParamDbl.sdev = function(x) exp(x$lstep))
)

mies_prime_operators(list(op.m.adapt), list(op.r), list(op.parent, op.survival),
  oi$search_space,
  additional_components = additional_component_sampler$param_set)

mies_init_population(oi, 3,
  additional_component_sampler = additional_component_sampler)

oi$archive$data[, .(x, y, lstep, Obj, dob, eol)]

offspring <- mies_generate_offspring(oi, 2, op.parent, op.m.adapt, op.r)

offspring

mies_evaluate_offspring(oi, offspring)
mies_survival_plus(oi, 3, op.survival)

oi$archive$data[, .(x, y, lstep, Obj, dob, eol)]



oi$archive$data[, .(x, y, Obj, dob, eol)]


oi$archive$clear()

oi <- OptimInstanceSingleCrit$new(objective,
  terminator = trm("evals", n_evals = 200)
)


mies <- opt("mies", mutator = op.m.adapt, recombinator = op.r,
  parent_selector = op.parent, survival_selector = op.survival,
  additional_component_sampler = additional_component_sampler,
  mu = 6, lambda = 6)

mies$optimize(oi)


ggplot() + theme_bw() + xlim(-2, 4) + ylim(-2, 4) + coord_fixed() +
  geom_contour(data = cbind(xs, z), aes(x = x, y = y, z = Obj)) +
  geom_point(data = oi$archive$data[order(dob)], aes(x = x, y = y, color = dob), size = 2.5) + scale_shape_manual(values = c(16, 3)) + scale_color_gradientn(colors = c("black", "blue", "red"))


ggplot(data = oi$archive$data[, .(mean_lstep = mean(lstep)), by = dob],
  aes(x = dob, y = mean_lstep)) + theme_bw() + geom_line()


# -----------

objective <- ObjectiveRFun$new(
  fun = function(xs) {
    z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
    z <- z + rnorm(1, sd = 1 / sqrt(xs$b))
    list(Obj = z)
  },
  domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4),
    b = p_int(1)),
  codomain = ps(Obj = p_dbl(tags = "maximize"))
)


objective <- ObjectiveRFun$new(
  fun = function(xs) {
    z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
    z <- z + rnorm(1, sd = 1 / sqrt(xs$b))
    list(Obj = z)
  },
  domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4),
    b = p_int(1, tags = "budget")),
  codomain = ps(Obj = p_dbl(tags = "maximize"))
)


oi <- OptimInstanceSingleCrit$new(objective,
  terminator = trm("evals", n_evals = 200)
)


fidelity_schedule <- data.frame(generation = c(1, 6),
  budget_new = c(1, 3), budget_survivors = c(5, 10))

budget_id <- "b"

op.m <- mut("gauss", sdev = .2)
op.r <- rec("xounif", p = .3)
op.parent <- sel("random")
op.survival <- sel("best")

mies_prime_operators(list(op.m), list(op.r), list(op.parent, op.survival),
  oi$search_space, budget_id = "b")

mies_init_population(oi, 3, fidelity_schedule = fidelity_schedule, budget_id = "b")

repeat {

  mies_step_fidelity(oi, fidelity_schedule, budget_id = "b")

  offspring <- mies_generate_offspring(oi, 2, op.parent, op.m, op.r,
    budget_id = "b")

  mies_evaluate_offspring(oi, offspring,
    fidelity_schedule = fidelity_schedule, budget_id = "b")

  mies_survival_plus(oi, 3, op.survival)

}

# --------

orig_fun <- function(xs) {
  z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
  list(Obj = z)
}



objective <- ObjectiveRFun$new(
  fun = function(xs) {
    z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
    z <- z + rnorm(1, sd = 1 / sqrt(xs$b))
    list(Obj = z)
  },
  domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4),
    b = p_int(1, tags = "budget")),
  codomain = ps(Obj = p_dbl(tags = "maximize"))
)

schedules <- list(
  n.1.s.5..n.3.s.10 = data.frame(generation = c(1, 10), budget_new = c(1, 3), budget_survivors = c(5, 10)),
  n.1.s.5 = data.frame(generation = 1, budget_new = 1, budget_survivors = 5),
  n.3.s.10 = data.frame(generation = 1, budget_new = 3, budget_survivors = 10),
  always.1 = data.frame(generation = 1, budget_new = 1, budget_survivors = 1),
  always.3 = data.frame(generation = 1, budget_new = 3, budget_survivors = 3),
  always.5 = data.frame(generation = 1, budget_new = 5, budget_survivors = 5),
  always.10 = data.frame(generation = 1, budget_new = 10, budget_survivors = 10),
  first.1.then.3 = data.frame(generation = c(1, 50), budget_new = c(1, 3), budget_survivors = c(1, 3)),
  first.5.then.10 = data.frame(generation = c(1, 10), budget_new = c(5, 10), budget_survivors = c(5, 10))
)


allresults <- list()

for (overiter in seq_len(100)) {
results <- lapply(schedules, function(fidelity_schedule) {

  oin <- OptimInstanceSingleCrit$new(objective,
    terminator = trm("budget", budget = 2000)
  )

  mies <- opt("mies", mutator = op.m, recombinator = op.r,
    parent_selector = op.parent, survival_selector = op.survival,
    multi_fidelity = TRUE, fidelity_schedule = fidelity_schedule,
    mu = 6, lambda = 6)

  mies$optimize(oin)
  oin$archive$data
})

rdt <- rbindlist(lapply(names(results), function(rn) {
  resdt <- results[[rn]]
  resdt <- cbind(resdt, True.Obj = orig_fun(resdt)[[1]])[,
    .(x, y, b, dob, eol, Obj, True.Obj)]
  resdt$i <- seq_len(nrow(resdt))
  resdt <- resdt[, .(genb = sum(b)), by = dob][, .(dob, bused = cumsum(genb))][
    resdt, on = "dob"]
  resdt$eol.bused <- resdt[, .(eol.bused = bused[1]), by = dob][
      resdt, eol.bused, on = c(dob = "eol")]
  resdt[is.na(eol.bused), eol.bused := max(resdt$bused)]
  resdt[is.na(eol), eol := max(resdt$dob) + 1]
  resdt[, cummax.Obj := cummax(Obj)]
  resdt[, cummax.True.Obj := cummax(True.Obj)]

  last.True.Obj <- resdt[1, True.Obj]
  last.Obj <- resdt[1, Obj]
  cum.True.Obj.of.max <- rep(last.True.Obj, nrow(resdt))
  for (i in seq_along(cum.True.Obj.of.max)) {
    if (resdt[i, is.na(eol) || dob != eol] && resdt[i, Obj] > last.Obj) {
      last.Obj <- resdt[i, Obj]
      last.True.Obj <- resdt[i, True.Obj]
    }
    cum.True.Obj.of.max[i] <- last.True.Obj
  }
  resdt[, cum.True.Obj.of.max := cum.True.Obj.of.max]
  resdt[, regime := rn]
}))



rdt[dob == 1]
# ggplot(data = rdt, aes(x = bused, y = cummax.Obj, color = regime)) + geom_line()

# ggplot(data = rdt, aes(x = bused, y = cummax.True.Obj, color = regime)) + geom_line()

print(ggplot(data = rdt, aes(x = bused, y = cum.True.Obj.of.max, color = regime)) + geom_line())
allresults[[overiter]] <- rdt
}


allresultsclean <- lapply(allresults, function(rdt) {
  rbindlist(lapply(names(schedules), function(rn) {
    resdt <- rdt[regime == rn]
    last.True.Obj <- resdt[1, True.Obj]
    last.Obj <- resdt[1, Obj]
    last.eol <- 1
    cum.True.Obj.of.max <- rep(last.True.Obj, nrow(resdt))
    for (i in seq_along(cum.True.Obj.of.max)) {

      if ((resdt[i, is.na(eol) || dob != eol] && resdt[i, Obj] > last.Obj) ||
          last.eol <= resdt[i, dob]) {
        last.Obj <- resdt[i, Obj]
        last.True.Obj <- resdt[i, True.Obj]
        last.eol <- resdt[i, eol]
      }
      cum.True.Obj.of.max[i] <- last.True.Obj
    }
    resdt[, cum.True.Obj.of.max := cum.True.Obj.of.max]
    resdt
  }))
})

ggplot(data = allresults[[1]], aes(x = bused, y = cum.True.Obj.of.max, color = regime)) + geom_line()



flattened <- rbindlist(lapply(seq_along(allresults), function(ia) {
  rbindlist(lapply(names(schedules), function(n) {
    res <- allresultsclean[[ia]][regime == n][, .(bused = max(bused), cum.True.Obj.of.max = last(cum.True.Obj.of.max)), by = "dob"][
        data.table(bused = 1:2000), .(bused, cum.True.Obj.of.max), roll = TRUE,
        on = "bused"][, .(ctoom = max(c(cum.True.Obj.of.max, 0), na.rm = TRUE), regime = n),
          by = bused]
    res$iter <- ia
    res
  }))
}))

# ggplot(flattened, aes(x = bused, y = ctoom, color = regime)) + geom_line()

rankflattened <- flattened[, .(regime, rctoom = rank(ctoom)), by = c("bused", "iter")]

rankqfln <- rankflattened[, as.list(quantile(rctoom, c(.25, .5, .75))), by = c("bused", "regime")]
colnames(rankqfln) <- c("bused", "regime", "lower", "median", "upper")

rankmfln <- rankflattened[, { s <- sd(rctoom) ; as.list(mean(rctoom) + c(mean = 0, lower = -s, upper = s)) }, by = c("bused", "regime")]

qfln <- flattened[, as.list(quantile(ctoom, c(.25, .5, .75))), by = c("bused", "regime")]
colnames(qfln) <- c("bused", "regime", "lower", "median", "upper")

rankmfln[regime == "high", regime := "new.3.surv.10"]
rankmfln[regime == "low", regime := "new.1.surv.5"]
rankmfln[regime == "normal", regime := "1.s.5.then.3.s.10"]
rankmfln[, budget := regime]

rankqfln[regime == "high", regime := "new.3.surv.10"]
rankqfln[regime == "low", regime := "new.1.surv.5"]
rankqfln[regime == "normal", regime := "1.s.5.then.3.s.10"]
rankqfln[, budget := regime]

qfln[regime == "high", regime := "new.3.surv.10"]
qfln[regime == "low", regime := "new.1.surv.5"]
qfln[regime == "normal", regime := "1.s.5.then.3.s.10"]
qfln[, budget := regime]

library("ggrepel")



ggplot(rankmfln[bused >= 100][, label := ifelse(bused == max(bused), budget, NA)][, labelmin := ifelse(bused == min(bused), budget, NA)], aes(x = (bused),
  y = mean, color = budget)) + theme_bw(base_size = 30) +
  geom_label_repel(aes(label = label), nudge_x = 1, na.rm = TRUE, show.legend = FALSE, size = 8) +
  geom_label_repel(aes(label = labelmin), nudge_x = -1, na.rm = TRUE, show.legend = FALSE, size = 8) +
#  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
  geom_line(size = 2) +
  scale_x_log10(limits = c(60, 3000))

ggplot(qfln[bused >= 100][, label := ifelse(bused == max(bused), budget, NA)][, labelmin := ifelse(bused == min(bused), budget, NA)], aes(x = (bused),
  y = median, color = budget)) + theme_bw(base_size = 30) +
  geom_label_repel(aes(label = label), nudge_x = 1, na.rm = TRUE, show.legend = FALSE, size = 8) +
  geom_label_repel(aes(label = labelmin), nudge_x = -1, na.rm = TRUE, show.legend = FALSE, size = 8) +
#  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
  geom_line(size = 2) +
  scale_x_log10(limits = c(60, 3000))


ggplot(rankqfln[bused >= 100], aes(x =(bused),
  y = median, color = budget)) + theme_bw() +
  scale_x_log10() +
#  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .3) +
  geom_line()

ggplot(qfln[bused >= 100], aes(x = (bused),
  y = median, color = budget)) + theme_bw() +
  scale_x_log10() +
#  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .3) +
  geom_line()


ggplot(allresults[[5]][regime == "always.10"], aes(x = (bused),
  y = cum.True.Obj.of.max, color = regime)) + theme_bw() +
  scale_x_log10() +
#  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .3) +
  geom_line() + geom_point()


ggplot(allresults[[1]][regime == "first.1.then.3"][, .(bused = max(bused), cum.True.Obj.of.max = last(cum.True.Obj.of.max)), by = "dob"][
    data.table(bused = 1:2000), .(bused, cum.True.Obj.of.max), roll = TRUE,
    on = "bused"][, .(ctoom = max(c(cum.True.Obj.of.max, 0), na.rm = TRUE), regime = "first.1.then.3"),
      by = bused], aes(x = (bused),
  y = ctoom, color = regime)) + theme_bw() +
  scale_x_log10() +
#  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .3) +
  geom_line() + geom_point()

ggplot(allresults[[5]][regime == "always.10"][
    data.table(bused = 1:2000), .(bused, cum.True.Obj.of.max), roll = TRUE,
    on = "bused"][, .(ctoom = max(c(cum.True.Obj.of.max, 0), na.rm = TRUE), regime = "always.10"),
      by = bused], aes(x = (bused),
  y = ctoom, color = regime)) + theme_bw() +
  scale_x_log10() +
#  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .3) +
  geom_line() + geom_point()


rr <- rowMeans(sapply(allresults, function(a) a[bused < 100, .(mdob = max(dob)), by = regime]$mdob))
names(rr) <- names(schedules)
rr

rr <- rowMeans(sapply(allresults, function(a) a[, .(mdob = max(dob)), by = regime]$mdob))
names(rr) <- names(schedules)
rr

rr <- rowMeans(sapply(allresults, function(a) a[bused < 300, .(mdob = max(dob)), by = regime]$mdob))
names(rr) <- names(schedules)
rr

rr <- rowMeans(sapply(allresults, function(a) a[dob == 10, .(mdob = mean(bused)), by = regime]$mdob))
names(rr) <- names(schedules)
rr


# ---------

objective <- ObjectiveRFun$new(
  fun = function(xs) {
    z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
    z <- z + rnorm(1, sd = 1 / sqrt(xs$b))
    list(Obj = z)
  },
  domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4),
    b = p_int(1, tags = "budget")),
  codomain = ps(Obj = p_dbl(tags = "maximize"))
)


oi <- OptimInstanceSingleCrit$new(objective,
  terminator = trm("evals", n_evals = 200)
)


fidelity_schedule <- data.frame(generation = c(1, 6),
  budget_new = c(1, 3), budget_survivors = c(5, 10))

mies <- opt("mies", mutator = op.m, recombinator = op.r,
  parent_selector = op.parent, survival_selector = op.survival,
  multi_fidelity = TRUE, fidelity_schedule = fidelity_schedule,
  mu = 6, lambda = 6)

mies$optimize(oi)


resdt <- cbind(oi$archive$data, True.Obj = orig_fun(oi$archive$data)[[1]])[,
  .(x, y, b, dob, eol, Obj, True.Obj)]
resdt$i <- seq_len(nrow(resdt))
resdt <- resdt[, .(genb = sum(b)), by = dob][, .(dob, bused = cumsum(genb))][
  resdt, on = "dob"]
resdt$eol.bused <- resdt[, .(eol.bused = bused[1]), by = dob][
    resdt, eol.bused, on = c(dob = "eol")]
resdt[is.na(eol.bused), eol.bused := max(resdt$bused)]
resdt[is.na(eol), eol := max(resdt$dob) + 1]



reevals = resdt[resdt[eol == dob], on = .(x, y, dob)][b != i.b]

ggplot() + theme_bw() +
  geom_segment(data = reevals, aes(x = bused, xend = bused, y = Obj, yend = i.Obj, color = sprintf("%03i", i.i))) +
  geom_segment(data = resdt[eol != dob], aes(x = bused, xend = eol.bused,
    y = Obj, yend = Obj, color = sprintf("%03i", i)))

ggplot() + theme_bw() +
  geom_segment(data = reevals, aes(x = bused, xend = bused, y = Obj, yend = i.Obj, color = i.i)) +
  geom_segment(data = resdt[eol != dob], aes(x = bused, xend = eol.bused,
    y = Obj, yend = Obj, color = (i))) +
  geom_segment(data = resdt[eol != dob], aes(x = eol.bused, xend = eol.bused, y = Obj, yend = True.Obj, color = i)) +
  scale_color_gradientn(colors = c("black", "blue", "red", "orange", "green"))

ggplot() + theme_bw() +
  geom_segment(data = resdt, aes(x = bused, xend = eol.bused, y = True.Obj, yend = True.Obj))

ggplot() + theme_bw() +
  geom_segment(data = resdt, aes(x = bused, xend = eol.bused, y = Obj, yend = Obj))

ggplot() + theme_bw() + xlim(-2, 4) + ylim(-2, 4) + coord_fixed() +
  geom_contour(data = cbind(xs, z), aes(x = x, y = y, z = Obj)) +
  geom_point(data = oi$archive$data[order(dob)], aes(x = x, y = y, color = dob), size = 2.5) + scale_shape_manual(values = c(16, 3)) + scale_color_gradientn(colors = c("black", "blue", "red"))


ggplot(data = oi$archive$data[, .(mean_lstep = mean(lstep)), by = dob],
  aes(x = dob, y = mean_lstep)) + theme_bw() + geom_line()
