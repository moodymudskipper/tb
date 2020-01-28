

test_that("renaming works", {
  mtcars_tb <- as_tb(mtcars)
  res <- mtcars_tb
  names(res)[2] <- "CYL"
  x <- "cyl"
  #expect_identical(res,
  # rename using symbol
  expect_identical(res,mtcars_tb[{cyl} := "CYL"])
  # rename using string
  expect_identical(res,mtcars_tb[{"cyl"} := "CYL"])
  # rename using expr
  expect_identical(res,mtcars_tb[{paste0("c","yl")} := "CYL"])
  # rename using extra variable
  expect_identical(res,mtcars_tb[{x} := "CYL"])

  # same using dot form
  # rename using symbol
  expect_identical(res,mtcars_tb[{cyl} := toupper(.)])
  # rename using string
  expect_identical(res,mtcars_tb[{"cyl"} := toupper(.)])
  # rename using expr
  expect_identical(res,mtcars_tb[{paste0("c","yl")} := toupper(.)])
  # rename using extra variable
  expect_identical(res,mtcars_tb[{x} := toupper(.)])
})




