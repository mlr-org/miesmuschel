


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
qoi$archive


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

op.m <- mut("gauss", sdev = .2)
op.r <- rec("xounif", p = .3)
op.parent <- sel("random")
op.survival <- sel("best")

mies$optimize(oi)

oi
