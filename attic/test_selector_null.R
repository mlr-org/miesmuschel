library(miesmuschel)
library(paradox)
library(testthat)

source("setup.R", local = TRUE)

context("selector_null")

test_that("test basic behavior of SelectorNull", {
  p = ps(x = p_dbl(-5, 5))
  data = data.frame(x = c(1, 2, 3, 4, 5))
  fitnesses = c(5, 2, 3, 1, 4)

  sn <- sel("null")
  sn$prime(p)

  selected_indices <- sn$operate(data, fitnesses, 3)
  expect_equal(selected_indices, c(1, 2, 3))

  selected_indices <- sn$operate(data, fitnesses, 5)
  expect_equal(selected_indices, c(1, 2, 3, 4, 5))

  selected_indices <- sn$operate(data, fitnesses, 7)
  expect_equal(selected_indices, c(1, 2, 3, 4, 5, 1, 2))
})

test_that("test SelectorNull with non-monotonic and unordered data", {
  p_numeric = ps(x = p_dbl(1, 50))
  data_numeric = data.frame(x = c(15, 5, 25, 45, 30))

  p_factor = ps(x = p_fct(levels = c("A", "B", "C", "D", "E")))
  data_factor = data.frame(x = factor(c("E", "B", "C", "D", "A"), levels = c("A", "B", "C", "D", "E")))

  sn_numeric <- sel("null")
  sn_numeric$prime(p_numeric)

  sn_factor <- sel("null")
  sn_factor$prime(p_factor)

  fitnesses = c(5, 2, 3, 1, 4)

  selected_indices_numeric_3 <- sn_numeric$operate(data_numeric, fitnesses, 3)
  selected_indices_factor_3 <- sn_factor$operate(data_factor, fitnesses, 3)

  expect_equal(selected_indices_numeric_3, c(1, 2, 3))
  expect_equal(selected_indices_factor_3, c(1, 2, 3))

  selected_indices_numeric_5 <- sn_numeric$operate(data_numeric, fitnesses, 5)
  selected_indices_factor_5 <- sn_factor$operate(data_factor, fitnesses, 5)

  expect_equal(selected_indices_numeric_5, c(1, 2, 3, 4, 5))
  expect_equal(selected_indices_factor_5, c(1, 2, 3, 4, 5))
})




test_that("test cloning of SelectorNull", {
  sn <- sel("null")
  sn_clone <- sn$clone()

  p = ps(x = p_dbl(-5, 5))
  data = data.frame(x = c(1, 2, 3, 4, 5))
  fitnesses = c(5, 2, 3, 1, 4)

  sn$prime(p)

  selected_indices <- sn$operate(data, fitnesses, 3)
  expect_equal(selected_indices, c(1, 2, 3))

  expect_error(sn_clone$operate(data, fitnesses, 3), NA)  # sn_clone is not primed

  sn_clone$prime(p)
  selected_indices_clone <- sn_clone$operate(data, fitnesses, 3)
  expect_equal(selected_indices_clone, c(1, 2, 3))
})
