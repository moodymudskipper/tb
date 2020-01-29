mtcars_tb <- as_tb(mtcars)
test_that("legal order of .i .j args works", {
  expect_error(mtcars_tb[1,1],NA)
  expect_error(mtcars_tb[a=1,  b=2,  c=3],NA)
  expect_error(mtcars_tb[a:=1, b:=2, c:=3],NA)
  expect_error(mtcars_tb[1, b=2,  c=3],NA)
  expect_error(mtcars_tb[1, b:=2, c:=3],NA)
})

test_that(".i and .j should be given first, left blank, or omitted", {
  expect_error(mtcars_tb[x= 1, 3], ".i and .j should be given first")
  expect_error(mtcars_tb[x= 1, y=2, 3], ".i and .j should be given first")
  expect_error(mtcars_tb[x:= 1, 3], ".i and .j should be given first")
  expect_error(mtcars_tb[x:= 1, y:=2, 3], ".i and .j should be given first")
  expect_error(mtcars_tb[x= 1, y:=2, 3], ".i and .j should be given first")
  expect_error(mtcars_tb[x:= 1, y=2, 3], ".i and .j should be given first")
})

test_that(".j must be given first, left blank, or omitted", {
  expect_error(mtcars_tb[1, y=2, 3], ".i and .j should be given first")
  expect_error(mtcars_tb[1, y:=2, 3], ".i and .j should be given first")
})

test_that("`=` and `:=` are equivalent", {
  expect_equivalent(mtcars_tb[x = 1], mtcars_tb[x := 1])
  expect_equivalent(mtcars_tb[,x = 1], mtcars_tb[,x := 1])
  expect_equivalent(mtcars_tb[,,x = 1], mtcars_tb[,,x := 1])
})
