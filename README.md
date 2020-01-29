
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tb

An experiment to create `data.table-esque` interface integrating all
major tidyverse data transformations, but more compact, and not faster\!
:).

Not sure where it will go.

<https://github.com/moodymudskipper/tb/issues/1>

## general syntax

The general syntax is `tb1[i, j, ..., .by]` (more args will come)

  - `i` is for row subsetting only, similar to *data.table* but feeding
    a data.frame to it doesn’t do a **right join** but a semi join.
  - `j` is for column selection in the SQL sense, so column subsetting
    with optional column creation (the latter not implemented yet).
  - `...` is for mutating or summarizing expressions (and will be used
    for **left joins** too in the future)
  - `.by` is for mentionning columns to **aggregate** by. Aggregation
    means we **consistently** get as many rows as distinct groups, to
    aggregate without groups, feed a length 0 object to `.by`. **NOT**
    used to “mutate by”\!

<!-- end list -->

``` r
# setup
library(tb)
library(tidyverse, warn.conflicts = FALSE) # for comparison
mtcars_tb <- as_tb(mtcars)
mtcars_tb$cyl <- factor(mtcars_tb$cyl,ordered=TRUE)
```

``` r
# demo
mtcars_tb[mpg > 31,]
#>                 mpg cyl disp hp drat    wt  qsec vs am gear carb
#> Fiat 128       32.4   4 78.7 66 4.08 2.200 19.47  1  1    4    1
#> Toyota Corolla 33.9   4 71.1 65 4.22 1.835 19.90  1  1    4    1
mtcars_tb[disp = mean(disp), drat = mean(drat), .by = "cyl"]
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
mtcars %tb>% .[disp = mean(disp), drat = mean(drat), .by = "cyl"]
#>   cyl     disp     drat
#> 1   6 183.3143 3.585714
#> 2   4 105.1364 4.070909
#> 3   8 353.1000 3.229286
```

  - `.` can be used in the `...` so mutating/summarizing arguments can
    refer to themselves :

<!-- end list -->

``` r
mtcars_tb[disp = mean(.), drat = mean(.), .by = "cyl"]
#>   cyl     disp     drat
#> 1   6 183.3143 3.585714
#> 2   4 105.1364 4.070909
#> 3   8 353.1000 3.229286
```

  - `?` can be used anywhere (`i`, `j`, `...` or `.by`) to select with a
    predicate or a regular expression.

<!-- end list -->

``` r
mtcars_tb[disp = mean(.), drat = mean(.), .by = ?is.factor]
#>   cyl     disp     drat
#> 1   6 183.3143 3.585714
#> 2   4 105.1364 4.070909
#> 3   8 353.1000 3.229286
```

  - `:=` can be used as a replacement for `=` anywhere (if used as a
    first argument it is not fed to `i` just like it wouldn’t with `=`)
    but it supports more features, such as this usage of `.` :

<!-- end list -->

``` r
mtcars_tb[c("disp", "drat") := mean(.), .by = ?is.factor]
#>   cyl     disp     drat
#> 1   6 183.3143 3.585714
#> 2   4 105.1364 4.070909
#> 3   8 353.1000 3.229286
```

That makes a convenient `summarize_at` mixed with `summarize`, or
`summarize_all`

``` r
mtcars_tb[(?"^d") := mean(.), wt = median(.), .by = ?is.factor]
#>   cyl     disp     drat    wt
#> 1   6 183.3143 3.585714 3.215
#> 2   4 105.1364 4.070909 2.200
#> 3   8 353.1000 3.229286 3.755
mtcars_tb[(?"^.*") := max(.), .by = c()] # or .by = NULL
#>    mpg cyl disp  hp drat    wt qsec vs am gear carb
#> 1 33.9   8  472 335 4.93 5.424 22.9  1  1    5    8
```

  - As long as the lhs is not a symbol it will be evaluated

<!-- end list -->

``` r
mtcars_tb[paste0("di","sp") := mean(.), .by = ?is.factor]
#>   cyl     disp
#> 1   6 183.3143
#> 2   4 105.1364
#> 3   8 353.1000
```

  - To “mutate by”, we use a formula notation, and we prefer to say
    “along” than “by” to make the distinction clear with aggregation.
    We can easily mix it with ungrouped transformations

<!-- end list -->

``` r
mtcars_tb[1:4, mean_gear = mean(gear) ~ carb, vs_and_am = vs & am]
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
mtcars_tb[1:2 ~ cyl, ] # with data.table : mtcars_dt[, .SD[1:2], by = cyl]
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
mtcars_tb[disp:drat]
#>                      disp  hp drat
#> Mazda RX4           160.0 110 3.90
#> Mazda RX4 Wag       160.0 110 3.90
#> Datsun 710          108.0  93 3.85
#> Hornet 4 Drive      258.0 110 3.08
#> Hornet Sportabout   360.0 175 3.15
#> Valiant             225.0 105 2.76
#> Duster 360          360.0 245 3.21
#> Merc 240D           146.7  62 3.69
#> Merc 230            140.8  95 3.92
#> Merc 280            167.6 123 3.92
#> Merc 280C           167.6 123 3.92
#> Merc 450SE          275.8 180 3.07
#> Merc 450SL          275.8 180 3.07
#> Merc 450SLC         275.8 180 3.07
#> Cadillac Fleetwood  472.0 205 2.93
#> Lincoln Continental 460.0 215 3.00
#> Chrysler Imperial   440.0 230 3.23
#> Fiat 128             78.7  66 4.08
#> Honda Civic          75.7  52 4.93
#> Toyota Corolla       71.1  65 4.22
#> Toyota Corona       120.1  97 3.70
#> Dodge Challenger    318.0 150 2.76
#> AMC Javelin         304.0 150 3.15
#> Camaro Z28          350.0 245 3.73
#> Pontiac Firebird    400.0 175 3.08
#> Fiat X1-9            79.0  66 4.08
#> Porsche 914-2       120.3  91 4.43
#> Lotus Europa         95.1 113 3.77
#> Ford Pantera L      351.0 264 4.22
#> Ferrari Dino        145.0 175 3.62
#> Maserati Bora       301.0 335 3.54
#> Volvo 142E          121.0 109 4.11
```

  - `.()` has the same meaning as in `bquote()` not as in data.table \!

<!-- end list -->

``` r
x <- "foo"
y <- c("bar", "baz")
mtcars_tb[1:2, .(x) := .(y)]
#>               mpg cyl disp  hp drat    wt  qsec vs am gear carb foo
#> Mazda RX4      21   6  160 110  3.9 2.620 16.46  0  1    4    4 bar
#> Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4 baz
```

  - `s()` is a selection helper, a bit like `dplyr::vars()`, can be use
    for negative subetting or just to spare quotes.

<!-- end list -->

``` r
mtcars_tb[1:2,s(-?is.numeric,drat, qsec:am)]
#>               cyl drat  qsec vs am
#> Mazda RX4       6  3.9 16.46  0  1
#> Mazda RX4 Wag   6  3.9 17.02  0  1
```

  - We can access in any argument the source table with `.X`, the
    sliced/selected data after application of `i` and `j` with `.data`,
    and the subset of data with `.subset`, can be useful for advanced
    filtering :

<!-- end list -->

``` r
library(matrixStats)
#> 
#> Attaching package: 'matrixStats'
#> The following object is masked from 'package:dplyr':
#> 
#>     count
mtcars_tb[rowAlls(.X > 150),] # rather than `filter_all(mtcars, all_vars(. > 150))`
#>  [1] mpg  cyl  disp hp   drat wt   qsec vs   am   gear carb
#> <0 rows> (or 0-length row.names)
mtcars_tb[rowAnys(.X > 150),] # rather than `filter_all(mtcars, any_vars(. > 150))`
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
mtcars_tb[rowAnys(.X[?"^d"] %% 2 == 0),]
#>                      mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4           21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag       21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710          22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive      21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout   18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Duster 360          14.3   8  360 245 3.21 3.570 15.84  0  0    3    4
#> Cadillac Fleetwood  10.4   8  472 205 2.93 5.250 17.98  0  0    3    4
#> Lincoln Continental 10.4   8  460 215 3.00 5.424 17.82  0  0    3    4
#> Chrysler Imperial   14.7   8  440 230 3.23 5.345 17.42  0  0    3    4
#> Dodge Challenger    15.5   8  318 150 2.76 3.520 16.87  0  0    3    2
#> AMC Javelin         15.2   8  304 150 3.15 3.435 17.30  0  0    3    2
#> Camaro Z28          13.3   8  350 245 3.73 3.840 15.41  0  0    3    4
#> Pontiac Firebird    19.2   8  400 175 3.08 3.845 17.05  0  0    3    2
# rather than `filter_at(mtcars, vars(starts_with("d")), any_vars((. %% 2) == 0))`
mtcars_tb[n = nrow(.subset), .by = "cyl"] # counting
#>   cyl  n
#> 1   6  7
#> 2   4 11
#> 3   8 14
```

  - unary `+` is used to splice, a bit like `!!!` in the *tidyverse*,
    except that it doesn’t have to be in `...`, works with any function,
    and will be evaluated in the context of the table. For example to to
    sort by all :

<!-- end list -->

``` r
mtcars_tb[1:4,c(1,3,6)][order(+.X),]
#>                 mpg disp    wt
#> Mazda RX4      21.0  160 2.620
#> Mazda RX4 Wag  21.0  160 2.875
#> Hornet 4 Drive 21.4  258 3.215
#> Datsun 710     22.8  108 2.320
mtcars_tb[1:4,c(1,3,6)][order(+-.X),] # descending
#>                 mpg disp    wt
#> Datsun 710     22.8  108 2.320
#> Hornet 4 Drive 21.4  258 3.215
#> Mazda RX4 Wag  21.0  160 2.875
#> Mazda RX4      21.0  160 2.620
mtcars_tb[order(+.X[?"^c"]),][1:4,] # arrange_if
#>                 mpg cyl  disp hp drat    wt  qsec vs am gear carb
#> Datsun 710     22.8   4 108.0 93 3.85 2.320 18.61  1  1    4    1
#> Fiat 128       32.4   4  78.7 66 4.08 2.200 19.47  1  1    4    1
#> Toyota Corolla 33.9   4  71.1 65 4.22 1.835 19.90  1  1    4    1
#> Toyota Corona  21.5   4 120.1 97 3.70 2.465 20.01  1  0    3    1
```

  - surounding the lhs with `((lhs))` means that the rhs variables
    should be removed

<!-- end list -->

``` r
mtcars_tb[1:2,((vs_and_am)) := vs & am]
#>               mpg cyl disp  hp drat    wt  qsec gear carb vs_and_am
#> Mazda RX4      21   6  160 110  3.9 2.620 16.46    4    4     FALSE
#> Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02    4    4     FALSE
```

  - surrounding the lhs with `{}` means we’re not transforming values
    but transforming names, so renaming and mutating can be mixed.

<!-- end list -->

``` r
mtcars_tb[1:2,{gear} := "GEAR"]
#>               mpg cyl disp  hp drat    wt  qsec vs am GEAR carb
#> Mazda RX4      21   6  160 110  3.9 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
mtcars_tb[1:2,((GEAR)) := gear] # this is similar
#>               mpg cyl disp  hp drat    wt  qsec vs am carb GEAR
#> Mazda RX4      21   6  160 110  3.9 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
mtcars_tb[1:2,{?"^d"} := toupper(.), DRAT = .*100] #rename_at + mutate
#>               mpg cyl DISP  hp DRAT    wt  qsec vs am gear carb
#> Mazda RX4      21   6  160 110  390 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag  21   6  160 110  390 2.875 17.02  0  1    4    4
mtcars_tb[1:2,{?"^.*"} := toupper(.)] # rename_all
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
tb1[`{x}` := y, .by= "z"]
#>   z  a  b
#> 1 5  3 NA
#> 2 6 NA  4
tb1[`{x}` := y, .by= s(-y)] # integrate "x" in .by to keep the column
#>   x z  a  b
#> 1 a 5  3 NA
#> 2 b 6 NA  4
```

We can aggregate as with `reshape` or the `reshape2` package :

``` r
mtcars_tb['mean_draft_for_vs_{vs}' = mean(drat), .by = "am" ]
#>   am mean_draft_for_vs_0 mean_draft_for_vs_1
#> 1  1            3.935000            4.148571
#> 2  0            3.120833            3.570000
```

It’s very easy to multispread, or even combine with mutating and
renaming statement :

``` r
mtcars_tb[
  'avg_draft_vs_{vs}' = mean(drat),
  'q2_mpg_cyl_{cyl}' = median(mpg),
  .by = "am" ]
#>   am avg_draft_vs_0 avg_draft_vs_1 q2_mpg_cyl_6 q2_mpg_cyl_4 q2_mpg_cyl_8
#> 1  1       3.935000       4.148571        21.00        28.85         15.4
#> 2  0       3.120833       3.570000        18.65        22.80         15.2
```

  - we don’t need to use functionals such as `lapply`, `mapply`, `map`,
    `pmap`, we can use the `~~` notation instead:

<!-- end list -->

``` r
# loop on values of vs, am and gear, with a constant value of na.rm
mtcars_tb[foo = mean(c(~~vs, ~~am, ~~gear), na.rm = TRUE)]
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
#>                          foo
#> Mazda RX4           1.666667
#> Mazda RX4 Wag       1.666667
#> Datsun 710          2.000000
#> Hornet 4 Drive      1.333333
#> Hornet Sportabout   1.000000
#> Valiant             1.333333
#> Duster 360          1.000000
#> Merc 240D           1.666667
#> Merc 230            1.666667
#> Merc 280            1.666667
#> Merc 280C           1.666667
#> Merc 450SE          1.000000
#> Merc 450SL          1.000000
#> Merc 450SLC         1.000000
#> Cadillac Fleetwood  1.000000
#> Lincoln Continental 1.000000
#> Chrysler Imperial   1.000000
#> Fiat 128            2.000000
#> Honda Civic         2.000000
#> Toyota Corolla      2.000000
#> Toyota Corona       1.333333
#> Dodge Challenger    1.000000
#> AMC Javelin         1.000000
#> Camaro Z28          1.000000
#> Pontiac Firebird    1.000000
#> Fiat X1-9           2.000000
#> Porsche 914-2       2.000000
#> Lotus Europa        2.333333
#> Ford Pantera L      2.000000
#> Ferrari Dino        2.000000
#> Maserati Bora       2.000000
#> Volvo 142E          2.000000
# rather than : 
mutate(mtcars, foo = pmap_dbl(list(vs, am, gear), ~mean(c(...), na.rm = TRUE)))
#>     mpg cyl  disp  hp drat    wt  qsec vs am gear carb      foo
#> 1  21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4 1.666667
#> 2  21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4 1.666667
#> 3  22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1 2.000000
#> 4  21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1 1.333333
#> 5  18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2 1.000000
#> 6  18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1 1.333333
#> 7  14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4 1.000000
#> 8  24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2 1.666667
#> 9  22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2 1.666667
#> 10 19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4 1.666667
#> 11 17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4 1.666667
#> 12 16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3 1.000000
#> 13 17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3 1.000000
#> 14 15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3 1.000000
#> 15 10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4 1.000000
#> 16 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4 1.000000
#> 17 14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4 1.000000
#> 18 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1 2.000000
#> 19 30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2 2.000000
#> 20 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1 2.000000
#> 21 21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1 1.333333
#> 22 15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2 1.000000
#> 23 15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2 1.000000
#> 24 13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4 1.000000
#> 25 19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2 1.000000
#> 26 27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1 2.000000
#> 27 26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2 2.000000
#> 28 30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2 2.333333
#> 29 15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4 2.000000
#> 30 19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6 2.000000
#> 31 15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8 2.000000
#> 32 21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2 2.000000
```

  - We can use the dollar `$` to pipe, as `mtcars_tb$.` returns
    `mtcars_tb` by convention, it’s more efficient than a `magrittr`
    pipe, has better precedence, doesn’t require an extra package and is
    lighter and easier to type:

<!-- end list -->

``` r
mtcars_tb[1:20,]$
  .[,4:9]$
  .[(?"^.*") := min(.), .by= s(vs,am)]
#>   vs am  hp drat    wt  qsec
#> 1  0  1 110 3.90 2.620 16.46
#> 2  1  1  52 3.85 1.615 18.52
#> 3  0  0 175 2.93 3.440 15.84
#> 4  1  0  62 2.76 3.150 18.30
```
