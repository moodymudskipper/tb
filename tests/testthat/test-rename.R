
mtcars_tb <- as_tb(mtcars)

# rename using symbol
mtcars_tb[{cyl} := "CYL"]
# rename using string
mtcars_tb[{"cyl"} := "CYL"]
# rename using expr
mtcars_tb[{paste0("c","yl")} := "CYL"]
# rename using extra variable
x <- "cyl"
mtcars_tb[{x} := "CYL"]

# same using dot form
# rename using symbol
mtcars_tb[{cyl} := toupper(.)]
# rename using string
mtcars_tb[{"cyl"} := toupper(.)]
# rename using expr
mtcars_tb[{paste0("c","yl")} := toupper(.)]
# rename using extra variable
x <- "cyl"
mtcars_tb[{x} := toupper(.)]
