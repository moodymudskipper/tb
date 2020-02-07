test_that("tb and as_tb work", {
  expect_identical(tb(a=1,b=2), as_tb(data.frame(a=1,b=2)))
  expect_error(as_tb(1), "must be a data frame")
  expect_error(1 %tb>% .[], "must be a data frame")
  expect_error(iris %tb>% foo[], "rhs should be of the form")
  expect_identical(iris %tb>% .[a=1][b=2], iris %tb>% .[a=1]$.[b=2])
  expect_identical(iris %tb>% .[a=1][["a"]], iris %tb>% .[a=1]$a)
  test <- iris
  class(test) <- c(class(iris), "grouped_df")
  expect_warning(backup_class(test))
  class(test) <- c(class(iris), "rowwise_df")
  expect_warning(backup_class(test))
})
