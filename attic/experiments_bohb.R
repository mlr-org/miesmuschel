# Notes on how BOHB is weird, and how we differ from BOHB:
# - new versionf of bohb don't respect min_points_in_model for the 'bad' model, and instead fit the 'bad' model on the complement of the 'good' model training set. We do it the way it is described in the paper. See https://github.com/automl/HpBandSter/issues/50
# - imputation for bohb KDE model: for each data row, iterate along features, and if that row & col is NA, sample another row that is not NA at the given feature. Fill in *all* the NA value of the current row from that selected row. This is probably close to sampling imputation as by mlr3pipelines, which we do.
# - sampling model and 'good' discrimination model in BOHB are identical, we fit two models (matters because imputation could be randomly different)
# - BOHB sampels wrong from Aitchison-Aitken, we do it the correct way; but this means BOHB effectively uses a smaller AA-bandwidth. However, BOHB uses 'statsmodels' KDE, which is buggy for categorical features anyway: https://github.com/statsmodels/statsmodels/issues/3790
# - https://github.com/automl/HpBandSter/commit/39fd5bda951a3fd33677a09de74a3ab73dcbce0c#diff-35ec12a27b1d7916cfe39309fa35b2750487b83e3a9102be83d675568ae8c0e2
#   effectively changes min_points_in_model from ncol + 2 to ncol. we are doing the "before" thing.


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


gr <- po("densitysplit", min_size = 11) %>>%
  list(lrn("density.np", id = "d1", bwmethod = "normal-reference"), lrn("density.np", id = "d2", bwmethod = "normal-reference")) %>>%
  po("densityratio")


gr$train(tsk("mtcars"))

gr$predict(tsk("mtcars"))


gr2 <- po("densitysplit", min_size = 11) %>>% po("multiplicityimply", 2) %>>%
  lrn("density.np", bwmethod = "normal-reference") %>>% po("multiplicityexply", 2) %>>%
  po("densityratio")

gr2$train(tsk("mtcars"))

gr2$predict(tsk("mtcars"))


po("stratify")$param_set


po("mutate", mutation = list(cyl = ~ cyl * 0.1))



archive = data.table(x = c(1, 2, 4, 3, 1), y = c(2, 3, 1, 4, 3), z = c(1, 2, 3, 4, 5))

tt = mlr3::TaskRegr$new("archive", archive, target = "z")

ss = SamplerKD$new(ps(x = p_int(0, 5), y = p_dbl(0, 5)), tt, TRUE)


plot(archive$x, archive$y, pch = as.character(archive$z), xlim = c(-1, 6), ylim = c(-1, 6))

ss$.__enclos_env__$private$.model$param_set$values$sampling_bw_factor = 0.4


sampled = ss$.__enclos_env__$private$.model$sample(1000, ss$param_set$lower, ss$param_set$upper)

points(sampled$x, sampled$y, pch = ".")

summary(iris)

irisspace <- ps(Sepal.Length = p_dbl(4, 8), Sepal.Width = p_dbl(2, 5), Petal.Length = p_dbl(1, 7), Species = p_fct(c("setosa", "versicolor", "virginica")))

iristask = mlr3::TaskRegr$new("archive", iris, target = "Petal.Width")

ss = SamplerKD$new(irisspace, iristask, TRUE, bandwidth_factor = 3, alpha = .4)
ss = SamplerKD$new(irisspace, iristask, TRUE, bandwidth_factor = 5, alpha = .4)
ss = SamplerKD$new(irisspace, iristask, FALSE, bandwidth_factor = .1, alpha = .4)

sampled = ss$sample(10000)$data

plot(iris$Petal.Length, iris$Petal.Width, xlim = c(1, 7), ylim = c(0, 6), col = as.numeric(iris$Species))
# plot(iris$Petal.Length, as.numeric(iris$Species) + iris$Petal.Width / 1, xlim = c(1, 7), ylim = c(1, 6))
points(sampled$Petal.Length, 4 + rnorm(nrow(sampled)) / 10, pch = ".", col = as.numeric(sampled$Species))


ss$.__enclos_env__$private$.model$state$model$dat


plot(iris$Petal.Length, rank(iris$Petal.Width), col = as.numeric(iris$Species))

sampled



###

library("mlr3pipelines")
library("mlr3")
library("mlr3learners")

po("stratify")$param_set


gr = po("stratify", stratify_feature = "Species", min_size = 3) %>>% list(lrn("regr.lm") %>>% po("predictionunion", collect_multiplicity = TRUE), lrn("regr.rpart")) %>>% po("predictionunion", id = "pu2")

gr$train(iristask)

# gr$pipeops$stratify$state
# gr$pipeops$regr.lm$state

gr$predict(iristask)


testdata = data.table(
  x = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 1, 2, 3, 1, 2, 1),
  Species = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 5),
  y = c(1, 2, 3, 4, 5, 2, 4, 6, 8, 3, 6, 9, 4, 8, 5)
)

gr = po("stratify", stratify_feature = "Species", min_size = 3) %>>% list(lrn("regr.lm") %>>% po("predictionunion", collect_multiplicity = TRUE), lrn("regr.lm", id = "lm2")) %>>% po("predictionunion", id = "pu2")
gr$param_set$values$stratify.min_size = 3

gr$train(TaskRegr$new("test", testdata, target = "y"))

pdata = CJ(x = 1:5, Species = 1:5)[, y := x * Species][]

library("ggplot2")
pred = as.data.table(gr$predict(TaskRegr$new("testpred", pdata, target = "y"))$pu2.output)
ggplot(cbind(pred, pdata[pred$row_ids])[order(row_ids)],
  ) + geom_line(aes(x = x, y = y, color = as.factor(Species))) +
      geom_point(aes(x = x, y = response, color = as.factor(Species)))


gr$param_set$values$stratify.predict_choice = "exact_or_less"

pred = as.data.table(gr$predict(TaskRegr$new("testpred", pdata, target = "y"))$pu2.output)
ggplot(cbind(pred, pdata[pred$row_ids])[order(row_ids)],
  ) + geom_line(aes(x = x, y = y, color = as.factor(Species))) +
      geom_point(aes(x = x, y = response, color = as.factor(Species)))

gr$param_set$values$stratify.min_size = 5

gr$train(TaskRegr$new("test", testdata, target = "y"))
pred = as.data.table(gr$predict(TaskRegr$new("testpred", pdata, target = "y"))$pu2.output)
ggplot(cbind(pred, pdata[pred$row_ids])[order(row_ids)],
  ) + geom_line(aes(x = x, y = y, color = as.factor(Species))) +
      geom_point(aes(x = x, y = response, color = as.factor(Species)))


gr$param_set$values$stratify.min_size = 6

gr$train(TaskRegr$new("test", testdata, target = "y"))
pred = as.data.table(gr$predict(TaskRegr$new("testpred", pdata, target = "y"))$pu2.output)
ggplot(cbind(pred, pdata[pred$row_ids])[order(row_ids)],
  ) + geom_line(aes(x = x, y = y, color = as.factor(Species))) +
      geom_point(aes(x = x, y = response, color = as.factor(Species)))

gl <- GraphLearner$new(gr)

as.data.table(gl$train(TaskRegr$new("test", testdata, target = "y"))$predict_newdata(pdata))

pred


ss = SamplerKD$new(ps(x = p_dbl(-10, 10), Species = p_int(1, 5)), TaskRegr$new("test", testdata, target = "y"), TRUE)

ss$sample(100)


ss = SamplerKD$new(ps(x = p_dbl(-10, 10)), TaskRegr$new("test", testdata, target = "y"), TRUE, bandwidth_factor = 0.1)

plot(ss$sample(10000)$data$x, rnorm(10000), pch = ".")


testdata


ss = SamplerKD$new(ps(x = p_dbl(-10, 10)), TaskRegr$new("test", testdata, target = "y"), TRUE, bandwidth_factor = 0.1, min_points_in_model = 5)

plot(ss$sample(10000)$data$x, rnorm(10000), pch = ".")

ss = SamplerKD$new(ps(x = p_dbl(-10, 10)), TaskRegr$new("test", testdata, target = "y"), TRUE, bandwidth_factor = 0.1, min_points_in_model = 4)

plot(ss$sample(10000)$data$x, rnorm(10000), pch = ".")

ss = SamplerKD$new(ps(x = p_dbl(-10, 10)), TaskRegr$new("test", testdata, target = "y"), FALSE, bandwidth_factor = 0.1, min_points_in_model = 4)

plot(ss$sample(10000)$data$x, rnorm(10000), pch = ".")


testdata

