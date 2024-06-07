
source("setup.R", local = TRUE)

learner = mlr3::lrn("classif.rpart")

learner$param_set$values[c("cp", "maxdepth")] = list(to_tune())

ti = TuningInstanceSingleCrit$new(
  task = mlr3::tsk("iris"),
  learner = learner,
  resampling = mlr3::rsmp("holdout"),
  measure = mlr3::msr("classif.acc"),
  terminator = bbotk::trm("gens", generations = 10)
)

mies_tune <- mlr3tuning::tnr("mies", mutator = mut("gauss", sdev = 0.1), recombinator = RecombinatorCrossoverUniform(),
  parent_selector = SelectorRandom$new(), survival_selector = SelectorBest$new(),
  mu = 10, lambda = 5
)

set.seed(1)
mies_tune$optimize(ti)

expect_set_equal(names(ti$result_x_domain), c("cp", "maxdepth"))
expect_true(ti$result$classif.acc > 0.9)
