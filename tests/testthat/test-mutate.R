mtcars_tb <- as_tb(mtcars)

# add column
mtcars_tb[x = cyl]
mtcars_tb[,x = cyl]
mtcars_tb[,,x = cyl]
mtcars_tb[x := cyl]
mtcars_tb[,x := cyl]
mtcars_tb[,,x := cyl]

# change column
mtcars_tb[cyl = mpg]
mtcars_tb[cyl = max(cyl)]

# change column using dot
mtcars_tb[cyl = log(.)]

# mutate "along"
mtcars_tb[x = max(cyl) ~ gear]

# mutate along with dot
mtcars_tb[cyl = max(.) ~ gear]

# mutate multiple with values
mtcars_tb[c("cyl2", "mpg2") := .x[c("cyl","mpg")]]

# mutate multiple with function
mtcars_tb[c("cyl", "mpg") := sqrt(.)]
