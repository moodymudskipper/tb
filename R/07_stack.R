
#' pivot table to longer
#'
#' The `.stack` parameter of `[.tb` is used to pivot to longer, an action that
#' is often accomplished by using `tidyr::pivot_longer()`, `tidyr::gather()`,
#' `data.table::melt()`,  `reshape2::melt()`, `reshape::melt()`,
#' `stats::reshape()` (with `direction = "long"`),
#' or `utils::stack()`. The syntax we propose is generally more flexible and
#' more compact than current solutions.
#'
#'
#' @name tb_stack
#' @examples
#' ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' ## examples from ?tidyr::pivot_longer
#'
#' ## Example 1
#'
#' min_relig_income <- relig_income[1:3, 1:4]
#'
#' \dontrun{
#' min_relig_income %>%
#'   pivot_longer(-religion, names_to = "income", values_to = "count")
#' }
#'
#' min_relig_income %tb>%
#'   .[.stack = "<income>" ~ religion][{value} := "count"]
#'
#' ## Example 2
#'
#' mini_billboard <- billboard[1:4, 1:7]
#'
#' \dontrun{
#' mini_billboard %>%
#'   pivot_longer(
#'     cols = starts_with("wk"),
#'     names_to = "week",
#'     names_prefix = "wk",
#'     values_to = "rank",
#'     values_drop_na = TRUE
#'   )
#' }
#'
#' mini_billboard %tb>%
#'   .[.stack = "wk<week>"][!is.na(value), {value} := "rank"]
#'
#' ## Example 3
#'
#' \dontrun{
#' who %>%
#'   pivot_longer(
#'     cols = new_sp_m014:newrel_f65,
#'     names_to = c("diagnosis", "gender", "age"),
#'     names_pattern = "new_?(.*)_(.)(.*)",
#'     values_to = "count"
#'   )
#' }
#'
#' who %tb>%
#'   .[.stack = "new<=_?><diagnosis>_<gender=.><age>"]$
#'   .[{value} := "count"]
#'
#' ## Example 4
#'
#' \dontrun{
#' anscombe %>%
#'   pivot_longer(everything(),
#'                names_to = c(".value", "set"),
#'                names_pattern = "(.)(.)"
#'     )
#' }
#'
#' anscombe %tb>%
#'   .[.stack= "<values_to=.><set>"]
#'
#' ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' ## examples from ?stats::reshape
#'
#' ## Example 1
#'
#' mini_indometh <- data.frame(
#'   Subject   = c(1L, 3L, 6L),
#'   conc.0.25 = c(1.5, 2.03, 2.72),
#'   conc.0.5  = c(0.94, 1.63, 1.49),
#'   conc.0.75 = c(0.78, 0.71, 1.16))
#'
#' \dontrun{
#' stats::reshape(mini_indometh, idvar = "Subject", varying = list(2:4),
#'                v.names = "conc", direction = "long")
#' }
#'
#' mini_indometh %tb>%
#'   .[.stack = "<values_to>.<time>"]
#'
#' ## Example 2
#'
#' doses <- data.frame(id = 1:4, age = c(40,50,60,50), dose1 = c(1,2,1,2),
#'                     dose2 = c(2,1,2,1), dose4 = c(3,3,3,3))
#'
#' \dontrun{
#'   reshape(doses, direction = "long", varying = 3:5, sep = "")
#' }
#'
#' doses %tb>%
#'   .[.stack = "<values_to><time=\\d>"]
#'
#' ## Example 3
#'
#' mini_state.x77 <- as.data.frame(state.x77[1:3, 1:4])
#'
#' \dontrun{
#'   reshape(mini_state.x77, idvar = "state", ids = row.names(mini_state.x77),
#'           times = names(mini_state.x77), timevar = "Characteristic",
#'           varying = list(names(mini_state.x77)), direction = "long")
#' }
#' mini_state.x77 %tb>%
#'   .[state = row.names(.data), .stack = "<Characteristic>" ~ state]
#'
#' ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' ## examples from ?reshape2::melt
#'
#' mini_airquality <- airquality %tb>% .[Day %in% c(1,15) & Month %in% 5:7, 3:6]
#'
#' \dontrun{
#'   melt(airquality_wide, id = c("Month", "Day"))
#' }
#'
#' airquality_wide %tb>%
#'   .[.stack = ~ s(Month, Day)]
#'
#' ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' ## examples from ?tidyr::gather
#'
#' # Example 1
#'
#' stocks <- data.frame(
#'   time = as.Date('2009-01-01') + 0:2,
#'   X = rnorm(3, 0, 1),
#'   Y = rnorm(3, 0, 2),
#'   Z = rnorm(3, 0, 4)
#' )
#'
#' \dontrun{
#'   stocks %>% gather("stock", "price", -time)
#' }
#'
#' stocks %tb>%
#'   .[.stack = "<stock>" ~ time][{value} := "price"]
#'
#' # Example 2
#'
#' mini_iris <- iris[c(1, 51, 101), ]
#'
#' \dontrun{
#'   gather(mini_iris, key = "flower_att", value = "measurement",
#'          Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
#' }
#'
#' mini_iris %tb>%
#'   .[.stack = "<flower_att>" ~ s(-Sepal.Length, -Sepal.Width, -Petal.Length, -Petal.Width)]$
#'   .[{value} := "measurement"]
#'
#' \dontrun{
#'   gather(mini_iris, key = "flower_att", value = "measurement", -Species)
#' }
#'
#' mini_iris %tb>%
#'   .[.stack = "<flower_att>" ~ Species][{value} := "measurement"]
#'
#' ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' ## example from ?utils::stack
#'
#' pg <- unstack(PlantGrowth)
#'
#' \dontrun{
#'   stack(pg)
#' }
#'
#' pg %tb>% .[.stack = s(?".*")]
#'
#'
NULL

stack_by_ref <- function(measure_vars, mask){ # vars supports "~" along notation
  # index(mtcars, s(disp, cyl, mpg)) this seems to warn related to a bug in cbind / data.frame
  # we should extract the elements just before and test with almost identical dfs
  if (is.null(measure_vars)) return(invisible())

  rownames(mask$.data) <- NULL

  if (inherits(measure_vars, "formula")) {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # if measure_vars is a formula, break it down into actual measure_vars and id_vars

    one_sided <- length(measure_vars) == 2
    if (one_sided) {
      id_vars <- measure_vars[[2]]
    } else {
      id_vars <- measure_vars[[3]]
    }

    if (is.symbol(id_vars)) {
      id_vars <- as.character(id_vars)
      if (!id_vars %in% names(mask$.data))
        stop(sprintf(paste0(
          "The column '%s' was not found, ",
          "if you meant to evaluate the variable '%s', ",
          "use '.(%s)' or `c(%s)`instead"),
          id_vars, id_vars, id_vars, id_vars))
    } else {
      id_vars <- expand_expr(id_vars, pf)
      id_vars <- eval(id_vars, envir = mask$.data, enclos = mask)
      if (inherits(id_vars, "tb_selection")) {
        id_vars <- modify_by_ref_and_return_selected_names(id_vars, mask)
      }
    }

    if (one_sided) {
      measure_vars <- setdiff(names(mask$.data), id_vars)
    } else {
      measure_vars <- eval(measure_vars[[2]], envir = mask$.data, enclos = mask)
      if (inherits(measure_vars, "tb_selection")) {
        measure_vars <- modify_by_ref_and_return_selected_names(measure_vars, mask)
      }
    }
  }

  if (inherits(measure_vars, "tb_selection")) {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # if measure_vars is a tb_selection, sort it out and id_vars will be the remaining
    measure_vars <- modify_by_ref_and_return_selected_names(measure_vars, mask)

    id_vars <- setdiff(names(mask$.data), measure_vars)
  } else if (
    is.character(measure_vars) &&
    length(measure_vars) == 1  &&
    !measure_vars %in% names(mask$.data)) {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # handle glue string
    unglued <- unglue::unglue_data(names(mask$.data), measure_vars, open = "<", close = ">")
    if (!is.null(unglued[["values_to"]])) {
      values_to <- setdiff(unique(unglued[["values_to"]]),NA)
      if (length(values_to) > 1) {
        measure_vars_list <- split(names(mask$.data), unglued[["values_to"]])
        if (!exists("id_vars")) {
          id_vars <- names(mask$.data)[is.na(unglued[["values_to"]])]
        } else {
          unglued <- unglued[!names(mask$.data) %in% id_vars,, drop = FALSE]
          # mask$.data[is.na(unglued[["values_to"]] & !names(mask$.data) %in% id_vars] <- NULL
        }
        unglued <- unglued[!is.na(unglued$values_to),, drop = FALSE]

        # we add a temporary id_var to ensure correct merge if id_vars are not unique sets
        id_vars <- c(id_vars, "..tmp..")
        mask$.data["..tmp.."] <- seq_len(mask$.N)

        data_list <-
          lapply(values_to, function(values_to){

            unglued_i <- unglued[unglued$values_to == values_to,, drop = FALSE ]
            unglued_i[["values_to"]] <- NULL
            measure_vars <- measure_vars_list[[values_to]]
            row.names(unglued_i) <- NULL
            data.frame(
              lapply(mask$.data[id_vars],rep, each = length(measure_vars)),
              unglued_i,
              setNames(list(c(t(mask$.data[measure_vars]))), values_to), # content of measure_vars by row
              stringsAsFactors = FALSE)
          })
        data <- Reduce(function(x,y) merge(x, y, all.x = TRUE, all.y = TRUE, sort = FALSE), data_list)
        data$..tmp.. <- NULL

        mask$.data <- data
        return(invisible())
        }
      # here we should probably loop and merge if several value headers
      unglued[["values_to"]] <- NULL
    } else {
      values_to <- "value"
    }

    if (exists("id_vars")) {
      measure_vars <- setdiff(names(mask$.data)[!is.na(unglued[[1]])], id_vars)
      unglued <- unglued[!names(mask$.data) %in% id_vars,, drop = FALSE ]
      unglued <- na.omit(unglued)
      row.names(unglued) <- NULL
    } else {
      measure_vars <- names(mask$.data)[!is.na(unglued[[1]])]
      unglued <- na.omit(unglued)
      row.names(unglued) <- NULL
      id_vars <- setdiff(names(mask$.data), measure_vars)

    }
    if (!length(id_vars)) {
      mask$.data <- data.frame(
        unglued,
        setNames(list(c(t(mask$.data[measure_vars]))), values_to), # content of measure_vars by row
        stringsAsFactors = FALSE)
    } else {
    mask$.data <- data.frame(
      lapply(mask$.data[id_vars],rep, each = length(measure_vars)),
      unglued,
      setNames(list(c(t(mask$.data[measure_vars]))), values_to), # content of measure_vars by row
      stringsAsFactors = FALSE)
    }
    return(invisible())

  } else {
    measure_vars <- names(.subset(mask$.data,  measure_vars))
    if (!exists("id_vars")) id_vars      <- setdiff(names(mask$.data), measure_vars)
  }

  if (!length(id_vars)) {
    mask$.data <- data.frame(
      key = measure_vars,
      value = c(t(mask$.data[measure_vars])),
      stringsAsFactors = FALSE)
  } else {
    mask$.data <- data.frame(
      lapply(mask$.data[id_vars],rep, each = length(measure_vars)),
      key = measure_vars,
      value = c(t(mask$.data[measure_vars])), # content of measure_vars by row, this converts all non num to character
      stringsAsFactors = FALSE)
  }
  invisible()
}
