#' Modify a tb object
#'
#'
#' `...` can contain `foo = expr` arguments such as those
#'
#' @param .X teebee object
#' @param i numeric, logical, character or formula to subset rows, i a formula
#'   the lhs must evaluate to numeric and the rhs specifies the variables to
#'   slice along.
#' @param j numeric, logical or character to subset columns
#' @param ... Name-value pairs of expression to be evaluated in the context of
#'   the teebee, see details
#' @param .by variables to aggregate by
#' @param .rm if TRUE columns not created by the call or part of .by cols will be
#'   removed
#' @param drop
#' @param .unchop
#'
#' @return
#' @export
#'
#' @examples
`[.tb` <- function(.X, i, j, ...,
                   .by, .along,
                   drop = FALSE){
  sc <- sys.call()

  #~~~~~~~~~~~~~~~~~~~----------------------
  ## deal with empty brackets right away
  empty_brackets <- length(sc) == 3 && identical(sc[[3]], substitute())
  if(empty_brackets) return(.X)

  if(drop) {
    stop("`drop` should always be FALSE in `[.tb`. ",
         "The argument was only kept for compatibility")
  }
  pf <- parent.frame()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## setup mask
  mask <- new.env(parent = pf)
  mask[[".X"]] <- .X
  mask[[".data"]] <- .X
  mask[["?"]] <- question_mark
  mask[[":"]] <- colon

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## get i, j. and dot args from the call and preprocess
  args <- as.list(sc)[!allNames(sc) %in% c(".by",".along", "drop")][c(-1,-2)]
  args      <- lapply(args, expand_expr, pf)
  args      <- lapply(args, splice_expr, mask)
  length_args <- length(args)

  specified_lgl <- allNames(args) != "" | vapply(args, is_labelled, logical(1))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## fail if wrong number of unspecified arguments
  first_unspecified_args_not_at_front <-
    length_args > 1 &&
    any(specified_lgl[1:2]) &&
    any(which(!specified_lgl) > which.max(specified_lgl))
  if(first_unspecified_args_not_at_front) {
    stop("i and j should be given first")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## assign arguments to i, j, or dots
  if(!specified_lgl[1]) {
    i <- args[[1]]
    if(length_args==1) {
      ## use list indexing, i s used for j
      j <- expand_expr(substitute(i), pf)
      col_subset_by_ref(j, mask, .by = NULL)
      return(mask$.data)
    }
    if(!specified_lgl[2]) {
      j <- args[[2]]
      dots <- args[-(1:2)]
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
  col_subset_by_ref(j, mask, .by)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## mutate if `.by` is missing
  if(missing(.by)){
    mutate_dots_by_ref(dots, mask)
    return(mask$.data)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # aggregate otherwise
  .by <- eval(substitute(.by), envir = mask$.data, enclos = mask)
  if(inherits(.by, "tb_selection")) {
    .by <- modify_by_ref_and_return_selected_names(.by, mask)
  }
  summarize_dots(dots, mask, .by)
}



