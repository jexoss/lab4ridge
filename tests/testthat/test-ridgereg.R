test_that("invalid input throws an error", {
  expect_error(ridgereg$new(Petal.Length ~ Sepal.Width, data = "not data"))
  expect_error(ridgereg$new("not a formula", data = iris))
  expect_error(ridgereg$new(Petal.Length ~ Sepal.Width, data = iris, lambda = -1))
})

test_that("lambda = 0 gives the same fitted values as lm()", {
  m   <- ridgereg$new(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = 0)
  fit <- lm(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
  expect_equal(m$predict(), unname(fitted(fit)))
})

test_that("coefficients are similar to MASS::lm.ridge()", {
  skip_if_not_installed("MASS")
  m  <- ridgereg$new(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = 1)
  mr <- MASS::lm.ridge(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = 1)
  expect_equal(unname(m$coef()[-1] / m$x_sds), unname(coef(mr)[-1]), tolerance = 1e-2)
})

test_that("predict() with newdata matches the fitted values", {
  m <- ridgereg$new(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = 1)
  expect_equal(m$predict(iris[1:5, ]), m$predict()[1:5])
})

test_that("print() outputs the call", {
  m <- ridgereg$new(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = 1)
  expect_output(m$print(), "ridgereg\\(formula = Petal.Length ~ Sepal.Width \\+ Sepal.Length, data = iris, lambda = 1\\)")
})
