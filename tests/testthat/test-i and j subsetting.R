# consistency with `[.data.frame`
# https://github.com/moodymudskipper/tb/issues/2

# .i and .j must be able to take numeric, logical, or character input and return the same object (except for class)
# In the syntax tb[foo], foo is fed to .j and drop is FALSE

sw <- as.data.frame(dplyr::starwars)
sw_tb <- as_tb(dplyr::starwars)
mtcars_tb <- as_tb(mtcars)
iris_tb <- as_tb(iris)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# basic .i subsetting

test_that("numeric i works", {
  expect_equivalent(mtcars[4:5,], mtcars_tb[4:5,])
})

test_that("logical i works", {
  expect_equivalent(mtcars[c(TRUE, FALSE),], mtcars_tb[c(TRUE, FALSE),])
})

test_that("character i works", {
  expect_equivalent(mtcars[c("Toyota Corolla", "Toyota Corona"),],
                    mtcars_tb[c("Toyota Corolla", "Toyota Corona"),])
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .i advanced subsetting

test_that("i in the context of the table works", {
  expect_equivalent(
    mtcars[mtcars$mpg < 30,],
    mtcars_tb[mpg < 30,])
})

test_that("i using .x works", {
  expect_equivalent(
    mtcars[c("Toyota Corolla", "Toyota Corona"),],
    mtcars_tb[startsWith(rownames(.x),"Toyota"),])
})

test_that("i using regex works", {
  expect_equivalent(
    mtcars[c("Toyota Corolla", "Toyota Corona"),],
    mtcars_tb[~"^Toyota",])
})

df <- data.frame(x=11:16, y = c(rep("a",3), rep("b",3)))
tb <- as_tb(df)
test_that("i along works", {
  expect_equivalent(df[c(1,2,4,5),],
                    tb[1:2 ~ y,])
})

sw_tb[mass > mean(mass, na.rm = TRUE) ~ gender,]

df2 <- data.frame(x= c(11,14:15), y = c("a","a","b"), z = "foo")
test_that("i with semi joins works", {
  expect_equivalent(df[c(1,5),],
                    tb[df2,])
})

sw_tb[mass > mean(mass, na.rm = TRUE) ~ gender,]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# basic .j subsetting

test_that("numeric j works", {
  expect_equivalent(mtcars[,4:5], mtcars_tb[,4:5])
})

test_that("logical j works", {
  expect_equivalent(mtcars[,c(TRUE, FALSE)], mtcars_tb[,c(TRUE, FALSE)])
})

test_that("character j works", {
  expect_equivalent(mtcars[,c("disp", "hp")],
                    mtcars_tb[,c("disp", "hp")])
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# when only one unnamed argument in brackets, or one and j, should be fed to .j

test_that("numeric j works", {
  expect_equivalent(mtcars[4:5], mtcars_tb[4:5])
})

test_that("numeric j works", {
  expect_equivalent(mtcars[4:5, drop = FALSE], mtcars_tb[4:5, drop = FALSE])
})

test_that("numeric j works", {
  expect_equivalent(mtcars[4], mtcars_tb[4])
})

test_that("numeric j works", {
  expect_equivalent(mtcars[,4, drop = FALSE], mtcars_tb[,4, drop = FALSE])
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .j advanced subsetting

test_that("selecting column sequences works", {
  expect_equivalent(iris[,2:4], iris_tb[,Sepal.Width:Petal.Width])
})

test_that("`?` j works with functions", {
  expect_equivalent(iris[1:4], iris_tb[?is.numeric])
})

test_that("`?` j works with regex", {
  expect_equivalent(iris[1:2], iris_tb[?"^Sepal"])
})

test_that("`?` j works with formulas", {
  expect_equivalent(iris[1:4], iris_tb[?~is.numeric(.)])
})

test_that("selection works with s()", {
  expect_equivalent(iris[,2:4], iris_tb[, s(Sepal.Width:Petal.Width)])
  expect_equivalent(iris[c("Sepal.Width", "Species")], iris_tb[s(Sepal.Width, Species)])
  expect_equivalent(iris[c("Sepal.Width", "Species")], iris_tb[s(c("Sepal.Width", "Species"))])
  expect_equivalent(iris[c("Petal.Length","Petal.Width")], iris_tb[s(?"^Petal")])
  expect_equivalent(iris[c("Species")], iris_tb[s(?is.factor)])
})

test_that("negative selection works with s()", {
  expect_equivalent(iris[,c(1,5)], iris_tb[, s(-(Sepal.Width:Petal.Width))])
  expect_equivalent(iris[c(1, 3, 4)], iris_tb[s(-Sepal.Width, -Species)])
  expect_equivalent(iris[c(1, 3, 4)], iris_tb[s(-c("Sepal.Width", "Species"))])
  expect_equivalent(iris[c(1:2,5)], iris_tb[s(-?"^Petal")])
  expect_equivalent(iris[-5], iris_tb[s(-?is.factor)])
})
