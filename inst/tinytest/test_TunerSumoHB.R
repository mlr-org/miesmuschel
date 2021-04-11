
source("setup.R", local = TRUE)

library("mlr3learners")
learner = mlr3::lrn("classif.ranger")

learner$param_set$values$mtry = to_tune(1, 4)
learner$param_set$values$max.depth= to_tune(1, 4)
learner$param_set$values$num.trees = to_tune(p_int(1, 1024, tags = "budget", logscale = TRUE))

ti = mlr3tuning::TuningInstanceSingleCrit$new(
  task = mlr3::tsk("iris"),
  learner = learner,
  resampling = mlr3::rsmp("holdout"),
  measure = mlr3::msr("classif.acc"),
  terminator = bbotk::trm("gens", generations = 10)
)

sumohb_tune <- mlr3tuning::tnr("sumohb", filtor = ftr("surprog", surrogate_learner = mlr3::lrn("regr.ranger")),
  mu = 10, survival_fraction = 0.5
)

set.seed(1)
sumohb_tune$optimize(ti)

expect_set_equal(names(ti$result_x_domain), c("mtry", "max.depth", "num.trees"))
expect_true(ti$result$classif.acc > 0.9)

ti$archive$data
