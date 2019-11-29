iris_tb <- as_tb(iris)

agg_iris <- aggregate(Sepal.Width ~ Species, iris, mean)


test_that("standard aggregation works with =",{
  expect_equivalent(
    agg_iris,
    iris_tb[Sepal.Width = mean(Sepal.Width), .by = "Species"])
  expect_equivalent(
    agg_iris,
    iris_tb[mean_SW = mean(Sepal.Width), .by = "Species"])
  expect_equivalent(
    agg_iris,
    iris_tb[Sepal.Width = mean(.), .by = "Species"])
})

test_that("standard aggregation works with :=",{
  expect_equivalent(
    agg_iris,
    iris_tb[mean_SW := mean(Sepal.Width), .by = "Species"])
  expect_equivalent(
    agg_iris,
    iris_tb[Sepal.Width := mean(.), .by = "Species"])
  expect_equivalent(
    agg_iris,
    iris_tb["Sepal.Width" = mean(.), .by = "Species"])
  expect_equivalent(
    agg_iris,
    iris_tb["Sepal.Width" := mean(.), .by = "Species"])
  x <- quote(Sepal.Width)
  expect_equivalent(
    agg_iris,
    iris_tb[.(x) := mean(.), .by = "Species"])
  x <- "Sepal.Width"
  expect_equivalent(
    agg_iris,
    iris_tb[.(x) := mean(.), .by = "Species"])
  expect_equivalent(
    agg_iris,
    iris_tb[(x) := mean(.), .by = "Species"])
  expect_equivalent(
    agg_iris,
    iris_tb[c(x) := mean(.), .by = "Species"])
  expect_equivalent(
    agg_iris,
    iris_tb[2 := mean(.), .by = "Species"])
  expect_equivalent(
    agg_iris,
    iris_tb[c(F,T,F,F,F) := mean(.), .by = "Species"])
})

iris_tb[Sepal.Width = mean(Sepal.Width), Petal.Width = mean(Petal.Width), .by = "Species"]


iris_tb[c("Sepal.Width", "Sepal.Length") := mean(.), .by = "Species"]
iris_tb[c("Sepal.Width", "Sepal.Length") := mean(.), .by = s(Species)]
iris_tb[c("Sepal.Width", "Sepal.Length") := 1, .by = "Species"]
iris_tb[c("Sepal.Width", "Sepal.Length") := list(1,2), .by = "Species"]
iris_tb[c("Sepal.Width", "Sepal.Length") := c(1,2), .by = "Species"]
iris_tb[c("Sepal.Width", "Sepal.Length") := data.frame(foo=1:3,bar=letters[1:3]), .by = "Species"]
iris_tb[c("min", "q1","q2","q3","max") := fivenum(Sepal.Length), .by = "Species"]
iris_tb[test := fivenum(Sepal.Length), .by = "Species"]
