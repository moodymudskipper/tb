#' Modify a tb object
#'
#'
#' `...` can contain `foo = expr` arguments such as those
#'
#' @param x tb object
#' @param i numeric, logical, character or formula to subset rows, if a formula
#'   the lhs must evaluate to numeric and the rhs specifies the variables to
#'   slice along.
#' @param j numeric, logical or character to subset columns
#' @param ... Name-value pairs of expression to be evaluated in the context of
#'   the tb, see details
#' @param by variables to aggregate by
#' @param .stack to pivot to longer, see `?tb_stack`
#' @param drop not used
#'
#' @export
`[.tb` <- function(x, i, j, ...,
                   .by, .stack = NULL, .fill = NULL, drop = FALSE) {
  sc <- sys.call()

  #~~~~~~~~~~~~~~~~~~~----------------------
  ## deal with empty brackets right away
  empty_brackets <- length(sc) == 3 && identical(sc[[3]], substitute())
  if (empty_brackets) return(x)

  if (drop) {
    stop("`drop` should always be FALSE in `[.tb`. ",
         "The argument was only kept for compatibility")
  }
  pf <- parent.frame()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## setup mask
  mask <- new.env(parent = pf)
  #mask[["x"]] <- x
  mask[[".data"]] <- x
  mask[[".N"]] <- nrow(x)
  mask[["?"]]  <- question_mark
  mask[[":"]]  <- colon

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## get i, j. and dot args from the call and preprocess
  args <- as.list(sc)[!allNames(sc) %in% c(".by", "drop", ".fill",".stack")][c(-1, -2)]
  args <- lapply(args, expand_expr, pf)

  if (length(args)) {

  if ( has_splice_prefix(args[[1]]) && has_splice_prefix(args[[c(1, 2)]])) {
    if (length(args) == 1) stop(
      "You cannot use `++` in a unique bracket argument as it is then fed to j ",
      "(list indexing), use foo[++bar,] instead of foo[++bar]")
    mask$.data <- rbind(mask$.data, eval.parent(args[[1]]))
    args[[1]] <- substitute()
  }

  args <- lapply(args, function(x) {
    if (is.numeric(x) || is.character(x)) return(x)
    if (has_splice_prefix(x)) {
      return(eval.parent(splice_expr(x[[2]], mask)))
    }
    list(splice_expr(x, mask))
  })
  args <- unlist(args, recursive = FALSE)
  length_args <- length(args)

  specified_lgl <- allNames(args) != "" | vapply(args, is_labelled, logical(1))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## fail if wrong number of unspecified arguments
  first_unspecified_args_not_at_front <-
    length_args > 1 &&
    any(specified_lgl[1:2]) &&
    any(which(!specified_lgl) > which.max(specified_lgl))
  if (first_unspecified_args_not_at_front) {
    stop("i and j should be given first")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## assign arguments to i, j, or dots
  if (!specified_lgl[1]) {
    i <- args[[1]]
    if (length_args == 1) {
      ## use list indexing, i s used for j
      j <- expand_expr(substitute(i), pf)
      col_subset_by_ref(j, mask, .by = NULL)
      return(mask$.data)
    }
    if (!specified_lgl[2]) {
      j <- args[[2]]
      dots <- args[- (1:2)]
    } else {
      j <- substitute()
      dots <- args[-1]
    }

  } else {
    i <- substitute()
    j <- substitute()
    dots <- args
  }
  row_subset_by_ref(i, mask)
  mask[[".N"]] <- nrow(mask$.data)
  col_subset_by_ref(j, mask, .by)
  } else {
    dots <- list()
  }

  if(length(dots)){

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## mutate if `.by` is missing
  if (missing(.by)) {
    mutate_dots_by_ref(dots, mask)
    fill_by_ref(.fill, mask)
    stack_by_ref(.stack, mask)
    return(mask$.data)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # aggregate otherwise
  .by <- substitute(.by)
  if (is.symbol(.by)) {
    .by <- as.character(.by)
    if (!.by %in% names(mask$.data))
      stop(sprintf(paste0(
        "The column '%s' was not found, ",
        "if you meant to evaluate the variable '%s', ",
        "use '.(%s)' or `c(%s)`instead"),
        .by, .by, .by, .by))
  } else {
    .by <- expand_expr(.by, pf)
    .by <- eval(.by, envir = mask$.data, enclos = mask)
    if (isTRUE(is.na(.by))) {
      .by <- setdiff(names(mask$.data), unlist(lapply(dots, all.vars)))
    }
    if (inherits(.by, "tb_selection")) {
      .by <- modify_by_ref_and_return_selected_names(.by, mask)
    }
  }

  mask$.data <- summarize_dots(dots, mask, .by)
  }


  # we should place the "post processing" in another function to avoid nested ifs above
  fill_by_ref(.fill, mask)
  stack_by_ref(.stack, mask)
  as_tb(mask$.data)
}
