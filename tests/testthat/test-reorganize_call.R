
test_that("missing i or j works with `=`", {
  expect_equivalent(mtcars_tb[x = 1], mtcars_tb[,x = 1])
  expect_equivalent(mtcars_tb[x = 1], mtcars_tb[,,x = 1])
})

test_that("missing i or j works with `:=`", {
  expect_equivalent(mtcars_tb[x = 1], mtcars_tb[x := 1])
  expect_equivalent(mtcars_tb[x = 1], mtcars_tb[,x := 1])
  expect_equivalent(mtcars_tb[x = 1], mtcars_tb[,,x := 1])
})


test_that("placing i or j after works with `=`", {
  expect_equivalent(mtcars_tb[1:2, x = 1], mtcars_tb[x = 1, 1:2])
  expect_equivalent(mtcars_tb[,3:4, x= 1], mtcars_tb[x = 1, ,3:4])
  expect_equivalent(mtcars_tb[1:2,3:4, x= 1], mtcars_tb[x = 1, 1:2, 3:4])
  expect_equivalent(mtcars_tb[1:2,3:4, x= 1, y = 2], mtcars_tb[1:2, x = 1, 3:4, y = 2])
})

test_that("placing i or j after works with `=`", {
  expect_equivalent(mtcars_tb[1:2, x = 1], mtcars_tb[1:2, x := 1])
  expect_equivalent(mtcars_tb[1:2, x = 1], mtcars_tb[x := 1, 1:2])
  expect_equivalent(mtcars_tb[,3:4, x= 1], mtcars_tb[x := 1, ,3:4])
  expect_equivalent(mtcars_tb[1:2,3:4, x= 1], mtcars_tb[x := 1, 1:2, 3:4])
  expect_equivalent(mtcars_tb[1:2,3:4, x= 1, y = 2], mtcars_tb[1:2, x := 1, 3:4, y := 2])
})
