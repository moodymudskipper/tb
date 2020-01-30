mtcars_tb <- as_tb(mtcars)

# add column
test_that("we can add columnw with", {
  res <- transform(mtcars, col = cyl)
  expect_identical(mtcars %tb>% .[col = cyl],res)
  expect_identical(mtcars %tb>% .[,col = cyl],res)
  expect_identical(mtcars %tb>% .[,,col = cyl],res)
  expect_identical(mtcars %tb>% .[col := cyl],res)
  expect_identical(mtcars %tb>% .[,col := cyl],res)
  expect_identical(mtcars %tb>% .[,,col := cyl],res)
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
  res <- transform(mtcars, col = ave(cyl, mpg))
  expect_identical(mtcars %tb>% .[col = mean(cyl) ~ mpg],res)
  expect_identical(mtcars %tb>% .[col := mean(cyl) ~ mpg],res)
  # mutate along with dot
  res <- transform(mtcars, cyl = ave(cyl, mpg))
  expect_identical(mtcars %tb>% .[cyl = mean(.) ~ mpg],res)
  expect_identical(mtcars %tb>% .[cyl := mean(.) ~ mpg],res)
})


test_that("we can mutate multiple values", {
  # by providing a the input as a list of same length
  res <- transform(mtcars, cyl2 = cyl, mpg2 = mpg)
  expect_identical(mtcars %tb>% .[c("cyl2", "mpg2") := .data[c("cyl","mpg")]],res)
  # by using a selection on the lhs, doesn't work now if columns don't exist
  # expect_identical(mtcars %tb>% .[s(cyl2, mpg2) := x[c("cyl","mpg")]],res)
  # by providing an expression containing the dot
  res <- transform(mtcars, cyl = sqrt(cyl), mpg = sqrt(mpg))
  expect_identical(mtcars %tb>% .[c("cyl", "mpg") := sqrt(.)],res)
  # by using `?` for "if"
  res <- transform(iris, Species = toupper(Species))
  expect_identical(iris %tb>% .[(?is.factor) := toupper(.)],res)
  # the precdence is fixed so we don't need these parentheses
  expect_identical(iris %tb>% .[?is.factor := toupper(.)],res)
  res <- transform(iris, Sepal.Length = sqrt(Sepal.Length), Sepal.Width = sqrt(Sepal.Width))
  expect_identical(iris %tb>% .[(?"^Sepal") := sqrt(.)],res)
})



test_that("we can mutate lists and vectors", {
  expect_identical(
    structure(list(a=1:2, b=list(3,4), c = 5:6, d = head(cars,2)), class= "data.frame", row.names=1:2),
    data.frame(a=1:2) %tb>% .[b = list(3,4), c = 5:6, d = head(cars,2)]
  )
})
