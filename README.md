
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tb

An experiment to create `data.table-esque` interface integrating all
major tidyverse data transformations, but more compact, and not faster\!
:).

It is working well now and a fair amount of tests have been written, but
expect breaking changes.

## general syntax

The general syntax is `tb1[i, j, ..., by, fill, drop]`. The syntax `df
%tb>% .[i, j, ..., by, fill, drop]` converts `df` to a `tb` transforms
it and returns an object of `df`’s class. I advise to use it, not to add
to the confusion of having to worry which objects in the workspace are
standard data frames, tibbles, `data.table` objects etc.

  - `i` is for row subsetting only, similar to *data.table* but feeding
    a data.frame to it doesn’t do a **right join** but a semi join.
  - `j` is for column selection in the SQL sense, so column subsetting
    with optional column creation (similar to `dplyr::select()` and
    `dplyr::transmute()`).
  - `...` is for mutating, summarizing, joining…
  - `by` is for mentionning columns to **aggregate** by. Aggregation
    means we **consistently** get as many rows as distinct groups, to
    aggregate without groups, feed a length 0 object to `by`. **NOT**
    used to “mutate by”\!

<!-- end list -->

``` r
# setup
library(tb)
library(tidyverse, warn.conflicts = FALSE) # for comparison
#> Warning: package 'tidyr' was built under R version 3.6.2
#> Warning: package 'dplyr' was built under R version 3.6.2
```

``` r
# demo
mtcars %tb>% .[mpg > 31,]
#>                 mpg cyl disp hp drat    wt  qsec vs am gear carb
#> Fiat 128       32.4   4 78.7 66 4.08 2.200 19.47  1  1    4    1
#> Toyota Corolla 33.9   4 71.1 65 4.22 1.835 19.90  1  1    4    1
mtcars %tb>% .[disp = mean(disp), drat = mean(drat), by = "cyl"]
#>   cyl     disp     drat
#> 1   6 183.3143 3.585714
#> 2   4 105.1364 4.070909
#> 3   8 353.1000 3.229286
```

## Advanced features

  - No need to convert a data frame to a `tb`, we can just pipe the
    input into `%tb>%` for one call and we’ll get back an object of the
    same class as the input:

<!-- end list -->

``` r
mtcars %tb>% .[disp = mean(disp), drat = mean(drat), by = "cyl"]
#>   cyl     disp     drat
#> 1   6 183.3143 3.585714
#> 2   4 105.1364 4.070909
#> 3   8 353.1000 3.229286
```

  - `.` can be used in the `...` so mutating/summarizing arguments can
    refer to themselves :

<!-- end list -->

``` r
mtcars %tb>% .[disp = mean(.), drat = mean(.), by = "cyl"]
#>   cyl     disp     drat
#> 1   6 183.3143 3.585714
#> 2   4 105.1364 4.070909
#> 3   8 353.1000 3.229286
```

  - `?` can be used anywhere (`i`, `j`, `...` or `by`) to select with a
    predicate or a regular expression.

<!-- end list -->

``` r
iris %tb>% .[Sepal.Length = mean(.), Sepal.Width = mean(.), by = ?is.factor]
#>      Species Sepal.Length Sepal.Width
#> 1     setosa        5.006       3.428
#> 2 versicolor        5.936       2.770
#> 3  virginica        6.588       2.974
```

  - `:=` can be used as a replacement for `=` anywhere (if used as a
    first argument it is not fed to `i` just like it wouldn’t with `=`)
    but it supports more features, such as this usage of `.` :

<!-- end list -->

``` r
iris %tb>% .[c("Sepal.Length", "Sepal.Width") := mean(.), by = ?is.factor]
#>      Species Sepal.Length Sepal.Width
#> 1     setosa        5.006       3.428
#> 2 versicolor        5.936       2.770
#> 3  virginica        6.588       2.974
```

That makes a convenient `summarize_at` mixed with `summarize`, or
`summarize_all`

``` r
iris %tb>% .[(?"^d") := mean(.), wt = median(.), by = ?is.factor]
#>      Species   wt
#> 1     setosa NULL
#> 2 versicolor NULL
#> 3  virginica NULL
cars %tb>% .[(?"^.*") := max(.), by = c()] # or by = NULL
#>   speed dist
#> 1    25  120
```

  - As long as the lhs is not a symbol it will be evaluated

<!-- end list -->

``` r
iris %tb>% .[paste("Petal","Length", sep=".") := mean(.), by = ?is.factor]
#>      Species Petal.Length
#> 1     setosa        1.462
#> 2 versicolor        4.260
#> 3  virginica        5.552
```

  - To “mutate by”, we use a formula notation, and we prefer to say
    “along” than “by” to make the distinction clear with aggregation.
    We can easily mix it with ungrouped transformations

<!-- end list -->

``` r
mtcars %tb>% .[1:4, mean_gear = mean(gear) ~ carb, vs_and_am = vs & am]
#>                 mpg cyl disp  hp drat    wt  qsec vs am gear carb mean_gear
#> Mazda RX4      21.0   6  160 110 3.90 2.620 16.46  0  1    4    4       4.0
#> Mazda RX4 Wag  21.0   6  160 110 3.90 2.875 17.02  0  1    4    4       4.0
#> Datsun 710     22.8   4  108  93 3.85 2.320 18.61  1  1    4    1       3.5
#> Hornet 4 Drive 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1       3.5
#>                vs_and_am
#> Mazda RX4          FALSE
#> Mazda RX4 Wag      FALSE
#> Datsun 710          TRUE
#> Hornet 4 Drive     FALSE
```

  - We can also “slice along”

<!-- end list -->

``` r
mtcars %tb>% .[1:2 ~ cyl, ] # with data.table : mtcars_dt[, .SD[1:2], by = cyl]
#>                 mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4      21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag  21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Merc 450SE     16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> Merc 450SL     17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
#> Honda Civic    30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> Toyota Corolla 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
```

  - `:` can be used between col names

<!-- end list -->

``` r
mtcars %tb>% .[1:2, disp:drat]
#>               disp  hp drat
#> Mazda RX4      160 110  3.9
#> Mazda RX4 Wag  160 110  3.9
```

  - `.()` has the same meaning as in `bquote()` not as in data.table \!

<!-- end list -->

``` r
x <- "foo"
y <- c("bar", "baz")
mtcars %tb>% .[1:2, .(x) := .(y)]
#>               mpg cyl disp  hp drat    wt  qsec vs am gear carb foo
#> Mazda RX4      21   6  160 110  3.9 2.620 16.46  0  1    4    4 bar
#> Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4 baz
```

  - `s()` is a selection helper, a bit like `dplyr::vars()`, can be use
    for negative subetting or just to spare quotes.

<!-- end list -->

``` r
mtcars %tb>% .[1:2,s(-?is.numeric,drat, qsec:am)]
#>               drat  qsec vs am
#> Mazda RX4      3.9 16.46  0  1
#> Mazda RX4 Wag  3.9 17.02  0  1
```

  - We can access in any argument the current data using`.data`, and the
    subset of data with `.subset`, can be useful for advanced filtering
    :

<!-- end list -->

``` r
library(matrixStats)
#> 
#> Attaching package: 'matrixStats'
#> The following object is masked from 'package:dplyr':
#> 
#>     count
mtcars[1:10, ] %tb>% .[rowAlls(.data > 200),] # rather than `filter_all(mtcars[1:10,], all_vars(. > 200))`
#>  [1] mpg  cyl  disp hp   drat wt   qsec vs   am   gear carb
#> <0 rows> (or 0-length row.names)
mtcars[1:10, ] %tb>% .[rowAnys(.data > 200),] # rather than `filter_all(mtcars[1:10,], any_vars(. > 200))`
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
#> Duster 360        14.3   8  360 245 3.21 3.570 15.84  0  0    3    4
mtcars[1:10, ] %tb>% .[rowAnys(.data[?"^d"] %% 2 == 0),]
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Duster 360        14.3   8  360 245 3.21 3.570 15.84  0  0    3    4
# rather than `filter_at(mtcars[1:10], vars(starts_with("d")), any_vars((. %% 2) == 0))`
mtcars[1:10, ] %tb>% .[n = nrow(.subset), by = "cyl"] # counting
#>   cyl n
#> 1   6 5
#> 2   4 3
#> 3   8 2
```

  - unary `+` is used to splice, a bit like `!!!` in the *tidyverse*,
    except that it doesn’t have to be in `...`, works with any function,
    and will be evaluated in the context of the table. For example to to
    sort by all :

<!-- end list -->

``` r
mtcars %tb>% .[1:4,c(1,3,6)][order(+.data),]
#>                 mpg disp    wt
#> Mazda RX4      21.0  160 2.620
#> Mazda RX4 Wag  21.0  160 2.875
#> Hornet 4 Drive 21.4  258 3.215
#> Datsun 710     22.8  108 2.320
mtcars %tb>% .[1:4,c(1,3,6)][order(+-.data),] # descending
#>                 mpg disp    wt
#> Datsun 710     22.8  108 2.320
#> Hornet 4 Drive 21.4  258 3.215
#> Mazda RX4 Wag  21.0  160 2.875
#> Mazda RX4      21.0  160 2.620
mtcars %tb>% .[order(+.data[?"^c"]),][1:4,] # arrange_if
#>                 mpg cyl  disp hp drat    wt  qsec vs am gear carb
#> Datsun 710     22.8   4 108.0 93 3.85 2.320 18.61  1  1    4    1
#> Fiat 128       32.4   4  78.7 66 4.08 2.200 19.47  1  1    4    1
#> Toyota Corolla 33.9   4  71.1 65 4.22 1.835 19.90  1  1    4    1
#> Toyota Corona  21.5   4 120.1 97 3.70 2.465 20.01  1  0    3    1
```

  - A consequence is that `foo[+bar]` is `cbind(foo, bar)`

<!-- end list -->

``` r
head(cars, 2) %tb>% .[+head(iris,2)]
#>   speed dist Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1     4    2          5.1         3.5          1.4         0.2  setosa
#> 2     4   10          4.9         3.0          1.4         0.2  setosa
```

  - We can `rbind()` by using `++` notation in `i` so that `foo[++bar,]`
    is `rbind(foo, bar)`

<!-- end list -->

``` r
head(cars, 2) %tb>% .[++tail(cars,2),]
#>    speed dist
#> 1      4    2
#> 2      4   10
#> 49    24  120
#> 50    25   85
```

  - surounding the lhs with `((lhs))` means that the rhs variables
    should be removed

<!-- end list -->

``` r
mtcars %tb>% .[1:2,((vs_and_am)) := vs & am]
#>               mpg cyl disp  hp drat    wt  qsec gear carb vs_and_am
#> Mazda RX4      21   6  160 110  3.9 2.620 16.46    4    4     FALSE
#> Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02    4    4     FALSE
```

  - surrounding the lhs with `{}` means we’re not transforming values
    but transforming names, so renaming and mutating can be mixed.

<!-- end list -->

``` r
mtcars %tb>% .[1:2,{gear} := "GEAR"]
#>               mpg cyl disp  hp drat    wt  qsec vs am GEAR carb
#> Mazda RX4      21   6  160 110  3.9 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
mtcars %tb>% .[1:2,((GEAR)) := gear] # this is similar
#>               mpg cyl disp  hp drat    wt  qsec vs am carb GEAR
#> Mazda RX4      21   6  160 110  3.9 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
mtcars %tb>% .[1:2,{?"^d"} := toupper(.), DRAT = .*100] #rename_at + mutate
#>               mpg cyl DISP  hp DRAT    wt  qsec vs am gear carb
#> Mazda RX4      21   6  160 110  390 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag  21   6  160 110  390 2.875 17.02  0  1    4    4
mtcars %tb>% .[1:2,{?"^.*"} := toupper(.)] # rename_all
#>               MPG CYL DISP  HP DRAT    WT  QSEC VS AM GEAR CARB
#> Mazda RX4      21   6  160 110  3.9 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
```

  - Spreading is done just by giving a “glue-like” name to our variable
    :

<!-- end list -->

``` r
df <- data.frame(x = c("a", "b"), y = c(3, 4), z = c(5, 6))
df
#>   x y z
#> 1 a 3 5
#> 2 b 4 6
spread(df, x, y)
#>   z  a  b
#> 1 5  3 NA
#> 2 6 NA  4
tb1 <- as_tb(df)
tb1[`{x}` := y, by= "z"]
#>   z  a  b
#> 1 5  3 NA
#> 2 6 NA  4
tb1[`{x}` := y, by= s(-y)] # integrate "x" in by to keep the column
#>   x z  a  b
#> 1 a 5  3 NA
#> 2 b 6 NA  4
```

We can aggregate as with `reshape` or the `reshape2` package :

``` r
mtcars %tb>% .['mean_draft_for_vs_{vs}' = mean(drat), by = "am" ]
#>   am mean_draft_for_vs_0 mean_draft_for_vs_1
#> 1  1            3.935000            4.148571
#> 2  0            3.120833            3.570000
```

It’s very easy to multispread, or even combine with mutating and
renaming statement :

``` r
mtcars %tb>% .[
  'avg_draft_vs_{vs}' = mean(drat),
  'q2_mpg_cyl_{cyl}' = median(mpg),
  by = "am" ]
#>   am avg_draft_vs_0 avg_draft_vs_1 q2_mpg_cyl_6 q2_mpg_cyl_4 q2_mpg_cyl_8
#> 1  1       3.935000       4.148571        21.00        28.85         15.4
#> 2  0       3.120833       3.570000        18.65        22.80         15.2
```

  - we mostly don’t need to use functionals such as `lapply`, `mapply`,
    `map`, `pmap`, we can use the `~~` notation instead:

<!-- end list -->

``` r
# loop on values of vs, am and gear, with a constant value of na.rm
mtcars %tb>% .[1:2, 8:10, foo = mean(c(~~vs, ~~am, ~~gear), na.rm = TRUE)]
#>               vs am gear      foo
#> Mazda RX4      0  1    4 1.666667
#> Mazda RX4 Wag  0  1    4 1.666667
# rather than : 
mutate(mtcars[1:2, 8:10], foo = pmap_dbl(list(vs, am, gear), ~mean(c(...), na.rm = TRUE)))
#>   vs am gear      foo
#> 1  0  1    4 1.666667
#> 2  0  1    4 1.666667
```

  - We can use the dollar `$` to pipe, as `tb1$.` returns `tb1` by
    convention, it makes tb chains easier to spot, more efficient (less
    conversions) and faster to type :

<!-- end list -->

``` r
mtcars %tb>%
  .[1:20,4:9]$
  .[?".*" := min(.), by = s(vs,am)]
#>   vs am  hp drat    wt  qsec
#> 1  0  1 110 3.90 2.620 16.46
#> 2  1  1  52 3.85 1.615 18.52
#> 3  0  0 175 2.93 3.440 15.84
#> 4  1  0  62 2.76 3.150 18.30
```
