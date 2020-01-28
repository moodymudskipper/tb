mtcars_tb <- as_tb(mtcars)

# add column
test_that("we can add columnw with", {
  res <- transform(mtcars, x = cyl)
  expect_identical(mtcars %tb>% .[x = cyl],res)
  expect_identical(mtcars %tb>% .[,x = cyl],res)
  expect_identical(mtcars %tb>% .[,,x = cyl],res)
  expect_identical(mtcars %tb>% .[x := cyl],res)
  expect_identical(mtcars %tb>% .[,x := cyl],res)
  expect_identical(mtcars %tb>% .[,,x := cyl],res)
})

test_that("we can change a column", {
  res <- transform(mtcars, cyl = mpg)
  expect_identical(mtcars %tb>% .[cyl = mpg],res)
  expect_identical(mtcars %tb>% .[cyl := mpg],res)
  res <- transform(mtcars, cyl = max(cyl))
  expect_identical(mtcars %tb>% .[cyl = max(cyl)],res)
  expect_identical(mtcars %tb>% .[cyl := max(cyl)],res)
  expect_identical(mtcars %tb>% .[cyl = max(.)],res)
  expect_identical(mtcars %tb>% .[cyl := max(.)],res)
})

test_that("we can mutate along", {
  res <- transform(mtcars, x = ave(cyl, mpg))
  expect_identical(mtcars %tb>% .[x = mean(cyl) ~ mpg],res)
  expect_identical(mtcars %tb>% .[x := mean(cyl) ~ mpg],res)
  # mutate along with dot
  res <- transform(mtcars, cyl = ave(cyl, mpg))
  expect_identical(mtcars %tb>% .[cyl = mean(.) ~ mpg],res)
  expect_identical(mtcars %tb>% .[cyl := mean(.) ~ mpg],res)
})


test_that("we can mutate multiple values", {
  # by providing a the input as a list of same length
  res <- transform(mtcars, cyl2 = cyl, mpg2 = mpg)
  expect_identical(mtcars %tb>% .[c("cyl2", "mpg2") := .X[c("cyl","mpg")]],res)
  # by using a selection on the lhs, doesn't work now if columns don't exist
  # expect_identical(mtcars %tb>% .[s(cyl2, mpg2) := .X[c("cyl","mpg")]],res)
  # by providing an expression containing the dot
  res <- transform(mtcars, cyl = sqrt(cyl), mpg = sqrt(mpg))
  expect_identical(mtcars %tb>% .[c("cyl", "mpg") := sqrt(.)],res)
  # by using `?` for "if"
  res <- transform(iris, Species = toupper(Species))
  expect_identical(iris %tb>% .[(?is.factor) := toupper(.)],res)
  res <- transform(iris, Sepal.Length = sqrt(Sepal.Length), Sepal.Width = sqrt(Sepal.Width))
  expect_identical(iris %tb>% .[(?"^Sepal") := sqrt(.)],res)
})
