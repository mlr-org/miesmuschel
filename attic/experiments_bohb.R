library("paradox")
library("tinytest")
library("checkmate")
library("data.table")

library("mlr3")
library("mlr3pipelines")


devtools::document()

devtools::load_all()



ts <- tsk("faithful")

l <- lrn("density.featureless")

l$train(ts)

l$predict(ts)


l2 <- lrn("density.np")

l2$train(ts)

l2$predict(ts)


plot(ts$data())
points(l2$sample(100000), pch = ".")
points(ts$data(), col = "red", lwd = 3)


resample(ts, l2, rsmp("holdout"))$aggregate()

resample(ts, lrn("density.featureless"), rsmp("cv", folds = 5))$aggregate()

resample(ts, lrn("density.np"), rsmp("cv", folds = 5))$aggregate()

resample(ts, lrn("density.np", bwmethod = "normal-reference"), rsmp("cv", folds = 5))$aggregate()



po("densityratio")

po("densitysplit")



po()



po("densitysplit")$train(list(tsk("mtcars")))


gr <- po("densitysplit", min_size = 11) %>>% list(lrn("density.np", id = "d1"), lrn("density.np", id = "d2")) %>>% po("densityratio")

gr$train(tsk("mtcars"))

gr$predict(tsk("mtcars"))


