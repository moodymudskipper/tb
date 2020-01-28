

sw_tb <- as_tb(dplyr::starwars)

sw_spread <- structure(list(
  eye_color = c("blue", "yellow", "brown"),
  max_mass_male = c(136, 136, 85),
  max_mass_female = c(75, 55, 49),
  min_height_male = c(170L, 94L, 66L),
  min_height_female = c(150L, 168L, 150L)),
  row.names = c(1L, 4L, 5L),
  class = c("data.frame"))

test_that(" = with backticked names works",{
  expect_equivalent(
    sw_spread,
    sw_tb[
      gender %in% c("male", "female") & eye_color %in% c("yellow", "blue", "brown"),
      `max_mass_{gender}` = max(mass, na.rm = TRUE),
      `min_height_{gender}` = min(height, na.rm = TRUE),
      .by = "eye_color"]
  )})

test_that("= with string litteral names works", {
  expect_equivalent(
    sw_spread,
    sw_tb[gender %in% c("male", "female") & eye_color %in% c("yellow", "blue", "brown"),
    'max_mass_{gender}' = max(mass, na.rm = TRUE),
    'min_height_{gender}' = min(height, na.rm = TRUE),
    .by = "eye_color"]
)})

test_that(":= with backticked names works", {
  expect_equivalent(
    sw_spread,
    sw_tb[
  gender %in% c("male", "female") & eye_color %in% c("yellow", "blue", "brown"),
  `max_mass_{gender}` := max(mass, na.rm = TRUE),
  `min_height_{gender}` := min(height, na.rm = TRUE),
  .by = "eye_color"]
)})


test_that(":= with string litteral names works", {
  expect_equivalent(
    sw_spread,
    sw_tb[gender %in% c("male", "female") & eye_color %in% c("yellow", "blue", "brown"),
    'max_mass_{gender}' := max(mass, na.rm = TRUE),
    'min_height_{gender}' := min(height, na.rm = TRUE),
    .by = "eye_color"]
)})

test_that(":= with a quoted variable fed using .() works",{
  x <- quote(`min_height_{gender}`)
  expect_equivalent(
    sw_spread,
    sw_tb[gender %in% c("male", "female") & eye_color %in% c("yellow", "blue", "brown"),
      `max_mass_{gender}` := max(mass, na.rm = TRUE),
      .(x) := min(height, na.rm = TRUE),
      .by = "eye_color"]
)})

test_that(":= with a string lhs fed using .()",{
  x <- "min_height_{gender}"
  expect_equivalent(
    sw_spread,
    sw_tb[gender %in% c("male", "female") & eye_color %in% c("yellow", "blue", "brown"),
      `max_mass_{gender}` := max(mass, na.rm = TRUE),
      .(x) := min(height, na.rm = TRUE),
      .by = "eye_color"]
)})

test_that(":= with a variable evaluated using {} works",{
  x <- "min_height_{gender}"
  expect_equivalent(
    sw_spread,
    sw_tb[gender %in% c("male", "female") & eye_color %in% c("yellow", "blue", "brown"),
      `max_mass_{gender}` := max(mass, na.rm = TRUE),
      {x} := min(height, na.rm = TRUE),
      .by = "eye_color"]
)})

#
#
# # multi spread with several glue variables works!
#
# dplyr::starwars %tb>%
#   .[gender %in% c("male", "female") &
#       eye_color %in% c("yellow", "blue", "brown") &
#       species %in% c("Human", "Wookiee"),
#     'max_mass_{gender}_{species}' = max(mass, na.rm = TRUE),
#     .by = "eye_color"]
#
# # also we might want to select columns in .i, so maybe using `?` on rownames is not
# # such a good idea
# iris %tb>%
#   .[order(.X[?is.numeric,])]
#
#
# dplyr::starwars %tb>% .[gender == "hermaphrodite",]
