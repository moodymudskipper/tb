#' Modify a tb object
#'
#'
#' `...` can contain `foo = expr` arguments such as those
#'
#' @param .X teebee object
#' @param .i numeric, logical, character or formula to subset rows, i a formula
#'   the lhs must evaluate to numeric and the rhs specifies the variables to
#'   slice along.
#' @param .j numeric, logical or character to subset columns
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
`[.tb` <- function(.X, .i, .j, ...,
                   .by, .along,
                   .rm = FALSE, drop = FALSE,
                   .unchop = FALSE){
  if(drop) {
    stop("`drop` should always be FALSE in `[.tb`. ",
         "The argument was only kept for compatibility")
  }

  pf <- parent.frame()
  sc <- as.list(sys.call())
  # if call is tb[], return tb
  no_arg_between_brackets <- length(sc) == 3 && identical(sc[[3]], substitute())
  if(no_arg_between_brackets) return(.X)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## setup mask
  mask <- new.env(parent = pf)
  mask[[".X"]] <- .X
  mask[[".data"]] <- .X
  mask[["?"]] <- question_mark
  mask[[":"]] <- colon

  ## detect necessity of list indexing
  bracket_arg_is_unique <- (nargs() - !missing(drop)) == 2
  if(bracket_arg_is_unique){
    bracket_arg <- sc[3]
    use_list_indexing_lgl <- !is_specified(bracket_arg)
    if(use_list_indexing_lgl) {
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## list indexing
      .j <- expand_expr(substitute(.i), pf)
      .i <- substitute()
      col_subset_by_ref(.j, mask, .by = NULL)
      return(mask$.data)
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## fetch dot expressions
  dots <- eval(substitute(alist(...)))
  dots <- subset_select_by_ref_and_return_dots(dots, .by, sc, pf, mask)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # no aggregation (.by and .along are NULL)
  if(missing(.by)){
    mutate_dots_by_ref(dots, mask)
    return(mask$.data)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # aggregate
  .by <- eval(substitute(.by), envir = mask$.data, enclos = mask)
  if(inherits(.by, "tb_selection")) {
    .by <- modify_by_ref_and_return_selected_names(.by, mask)
  }
  summarize_dots(dots, mask, .by)
}



