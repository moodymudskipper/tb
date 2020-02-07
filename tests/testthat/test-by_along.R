mtcars_tb <- as_tb(mtcars)

test_that("different ways to input 'by' work", {
  ref <- mtcars_tb[max_mpg = max(mpg), by = "vs"]
  expect_identical(ref, mtcars_tb[max_mpg = max(mpg), by = vs])
  expect_identical(ref, mtcars_tb[max_mpg = max(mpg), by = s(vs)])
  expect_identical(ref, mtcars_tb[max_mpg = max(mpg), by = 8])
  by_lgl <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE,
                FALSE, FALSE)
  expect_identical(ref, mtcars_tb[max_mpg = max(mpg), by = .(by_lgl)])



  ref <- mtcars_tb[max_mpg = max(mpg), by = c("vs","am")]
  expect_identical(ref, mtcars_tb[max_mpg = max(mpg), by = s(vs, am)])
  expect_identical(ref, mtcars_tb[max_mpg = max(mpg), by = vs:am])
})


test_that("different ways to input 'along' work", {
  ref <- mtcars_tb[max_mpg = max(mpg) ~ vs]
  expect_identical(ref, mtcars_tb[max_mpg = max(mpg) ~ "vs"])
  expect_identical(ref, mtcars_tb[max_mpg = max(mpg) ~ s(vs)])

  ref <- mtcars_tb[max_mpg = max(mpg) ~ c("vs","am")]
  expect_identical(ref, mtcars_tb[max_mpg = max(mpg) ~ s(vs, am)])
  expect_identical(ref, mtcars_tb[max_mpg = max(mpg) ~ vs:am])

  ref <- mtcars_tb[s(max_mpg = max(mpg) ~ c("vs","am"))]
  expect_identical(ref, mtcars_tb[s(max_mpg = max(mpg) ~ s(vs, am))])
  expect_identical(ref, mtcars_tb[s(max_mpg = max(mpg) ~ vs:am)])
})
