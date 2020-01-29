iris_tb <- as_tb(iris)

agg_iris <- aggregate(Sepal.Width ~ Species, iris, mean)


test_that("standard aggregation works with =",{
  expect_equivalent(
    agg_iris,
    iris_tb[Sepal.Width = mean(Sepal.Width), by = "Species"])
  expect_equivalent(
    agg_iris,
    iris_tb[mean_SW = mean(Sepal.Width), by = "Species"])
  expect_equivalent(
    agg_iris,
    iris_tb[Sepal.Width = mean(.), by = "Species"])
})

test_that("standard aggregation works with :=",{
  expect_equivalent(
    agg_iris,
    iris_tb[mean_SW := mean(Sepal.Width), by = "Species"])
  expect_equivalent(
    agg_iris,
    iris_tb[Sepal.Width := mean(.), by = "Species"])
  expect_equivalent(
    agg_iris,
    iris_tb["Sepal.Width" = mean(.), by = "Species"])
  expect_equivalent(
    agg_iris,
    iris_tb["Sepal.Width" := mean(.), by = "Species"])
  x <- quote(Sepal.Width)
  expect_equivalent(
    agg_iris,
    iris_tb[.(x) := mean(.), by = "Species"])
  x <- "Sepal.Width"
  expect_equivalent(
    agg_iris,
    iris_tb[.(x) := mean(.), by = "Species"])
  expect_equivalent(
    agg_iris,
    iris_tb[(x) := mean(.), by = "Species"])
  expect_equivalent(
    agg_iris,
    iris_tb[c(x) := mean(.), by = "Species"])
  expect_equivalent(
    agg_iris,
    iris_tb[2 := mean(.), by = "Species"])
  expect_equivalent(
    agg_iris,
    iris_tb[c(F,T,F,F,F) := mean(.), by = "Species"])
})



test_that("We can aggregate on several colums",{
  res <- merge(
    aggregate(Sepal.Width ~ Species, iris, mean),
    aggregate(Sepal.Length ~ Species, iris, mean))
  expect_identical(
    iris %tb>% .[Sepal.Width = mean(Sepal.Width), Sepal.Length = mean(Sepal.Length), by = "Species"],
    res
  )
  expect_identical(
    iris %tb>% .[c("Sepal.Width", "Sepal.Length") := mean(.), by = "Species"],
    res
  )
  expect_identical(
    iris %tb>% .[c("Sepal.Width", "Sepal.Length") := mean(.), by = s(Species)],
    res
  )
  res <- merge(
    setNames(aggregate(Sepal.Width ~ Species, iris, min), c("Species","min")),
    setNames(aggregate(Sepal.Width ~ Species, iris, max), c("Species","max")))
  # feeding a scalar vector
  expect_identical(
    iris %tb>% .[c("min", "max") := range(Sepal.Width), by = "Species"],
    res
  )
  # feeding a data frame
  expect_identical(
    iris %tb>% .[c("min", "max") := as.data.frame(t(range(Sepal.Width))), by = "Species"],
    res
  )
  # feeding a list
  # for some reason it doesn't work with lists! need to fix this
  # expect_identical(
  #   iris %tb>% .[c("min", "max") := as.list(range(Sepal.Width)), by = "Species"],
  #   res
  # )
})
