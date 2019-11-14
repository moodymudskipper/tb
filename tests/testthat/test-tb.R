test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

sw <- as_tb(dplyr::starwars)
swdt <- data.table::as.data.table(sw)

# slicing/subsetting rows
sw[1:2,]
sw[c(T,F),]
sw[height > 220,]
sw[1:2 ~ gender, ]
sw[1:2 ~ gender, ]
sw[1:2, ~ gender + eye_color]

# subsetting columns
sw[,1:3]
sw[, hair_color:eye_color]
sw[, c(T,F)]
sw[, c("hair_color", "skin_color", "eye_color")]
sw[, ?is.numeric]

# subsetting both
sw[1:2,1:3]
sw[1:2, hair_color:eye_color]
sw[1:2, c(T,F)]
sw[1:2, c("hair_color", "skin_color", "eye_color")]
sw[1:2, ?is.numeric]


# mutate
sw[1:4, 1:3, height = height/100]
sw[1:4, 1:3, height_cm = height/100]


sw[1:4, 1:3, height_cm = height/100, .rm = TRUE]
sw[1:4, 1:3, height = height/100, .rm = TRUE]

x <- "height_cm"
sw[1:4, 1:3, {x} := height/100]
sw[1:4, 1:3, `{x}` = height/100]
sw[1:4, 1:6, height = mean(height) ~ eye_color]
x <- quote(height_cm)
sw[1:4, 1:6, `{x}` = mean(height) ~ eye_color]
sw[{x} := height/100] # works
sw[1:4, {x} := height/100]  # works
sw[1:4, `{x}` = height/100] # works
sw[1:4, height = mean(height) ~ eye_color] # works
sw[1:4, height := mean(height) ~ eye_color] #  works
sw[1:4, `{x}` = mean(height) ~ eye_color] # works


library(unglue)
glued_data <-  tibble(text=glue::glue_data(head(mtcars), "{rownames(head(mtcars))} has {hp} hp"))
class(glued_data) <- c("tb", class(glued_data))
unglue_data(glued_data, "{rownames(.)} has {hp} hp")
glued_data[,,!!!unglue_data(text, "{rownames(.)} has {hp} hp")]


# summarize
sw[,, mean(height, na.rm = TRUE)] # when no .along or .by AND unnamed, raw result

sw[,, !!!list(a=height, b = eye_color), .rm = TRUE] # when no .along or .by AND unnamed, raw result
sw[,, mean(height, na.rm = TRUE), .by = "gender"]
sw[,, mean_height = mean(height, na.rm = TRUE), .by = "gender"]


## bind columns

sw2 <- rename_all(sw, toupper)
sw[,1:2,!!!sw2[,1:2]]

## bind rows


# spread

sw[,, `mean_height_{eye_color}` = mean(height), .by = "gender"]

# gather (into nested dfs, without adding rows!!!)


sw[,, (color) := keyval(hair_color, skin_color, eye_color)]
# would benefit from select helpers here too

sw[,, `mean_height_{eye_color}` = mean(height), .by = "gender"]


### arrange, sort
sw[order(height),]



mutate(cars, !!!list(speed)) # this one doesn't work
mutate(cars, !!!alist(speed))
mutate(cars, !!!list(quote(speed)))
mutate(cars, !!!list(quote(speed)))
