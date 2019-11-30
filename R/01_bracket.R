#' Modify a tb object
#'
#'
#' `...` can contain `foo = expr` arguments such as those
#'
#' @param .x teebee object
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
`[.tb` <- function(.x, .i, .j, ...,
                   .by = NULL, .along = NULL,
                   .rm = FALSE, drop = FALSE,
                   .unchop = FALSE){
  if(drop) {
    stop("`drop` should always be FALSE in `[.tb`. ",
         "The argument was only kept for compatibility")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## setup mask
  pf <- parent.frame()
  mask <- new.env(parent = pf)
  mask[[".x"]] <- .x
  mask[[".data"]] <- .x
  mask[["?"]] <- question_mark
  mask[[":"]] <- colon

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## fetch dot expressions and expand
  dots <- eval(substitute(alist(...)))
  dots <- lapply(dots, expand_expr, pf)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## subset, and deal with := being used in i j as if it was =
  # so as if it was not fed to i or j

  # if only one arg is fed besides .x and drop and that this arg is unnamed,
  # feed to .j


  ## detect necessity of list indexing
  bracket_arg_is_unique <- (nargs() - !missing(drop)) == 2
  if(bracket_arg_is_unique){
    bracket_arg <- sys.call()[3]
    use_list_indexing_lgl <- !is_specified(bracket_arg)
  } else {
    use_list_indexing_lgl <- FALSE
  }

  if(use_list_indexing_lgl) {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## list indexing
    .j <- expand_expr(substitute(.i), pf)
    .i <- substitute()
    col_subset_by_ref(.j, mask, .by)
  } else {
    ## reorganize call if labelled args are fed to .i or .j
    dots0 <- list()
    unspecified_dots <- dots[!vapply(split(dots,seq_along(dots)), is_specified, logical(1))]

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## slice or filter
    if(missing(.i)) {
      .i <- substitute()
    } else {
      .i <-expand_expr(substitute(.i), pf)
      if(is_labelled(.i)) {
        dots0 <- .i
        if(length(unspecified_dots)) {
          .i <- unspecified_dots[[1]]
          .i <- splice_expr(.i, mask)
          row_subset_by_ref(.i, mask)
          unspecified_dots[[1]] <- NULL
        } else {
          .i <- substitute()
        }
      } else {
        .i <- splice_expr(.i, mask)
        row_subset_by_ref(.i, mask)
      }
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## select or transmute
    if(missing(.j))  .j <- substitute() else {
      .j <-expand_expr(substitute(.j), pf)

      if(is_labelled(.j)) {
        dots0 <- c(dots0,.j)
        if(length(unspecified_dots)) {
          .j <- unspecified_dots[[1]]
          .j <- splice_expr(.j, mask)
          col_subset_by_ref(.j, mask, .by)
        } else {
          .j <- substitute()
        }
      } else {
        .j <- splice_expr(.j, mask)
        col_subset_by_ref(.j, mask, .by)
      }

    }
    dots <- c(dots0, dots)
  }

  ## splice dots
  dots <- lapply(dots, function(x) {
    if(is.numeric(x) || is.character(x)) return(list(x))
    if(has_splice_prefix(x)){
      return(eval(x[[2]], envir = mask$.data, enclos = mask))
    }
    x
  })
  dots <- unlist(dots, recursive = FALSE)
  dots <- lapply(dots, reparse_dbl_tilde)

  if(!missing(.by)){
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # aggregate
    .by <- eval(substitute(.by), envir = mask$.data, enclos = mask)
    if(inherits(.by, "tb_selection")) {
      #data  <- tb_transmute(data, .j, env = caller_env)
      .by <- tb_select_by_ref(.by, mask)
    }
    mask$.data <- aggregate_tb(dots, mask, .by)

  } else {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # no aggregation (.by and .along are NULL)

    # we remove support of unnamed dots
    # if(is_unique_and_unnamed(dots)){
    #   return(eval(dots[[1]], envir = build_mask(x), enclos = env))
    # }

    for(i in seq_along(dots)){
      ## setup loop
      expr <- dots[[i]]
      nm <- names(dots)[i]
      .data <- mask$.data
      to_remove <- NULL

      # TO DO: we should be able to have parenthesized glue name too!!!
      if(is_labelled(expr)) {
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # labelled dot expressions

        lhs <- expr[[2]]
        expr <- expr[[3]]

        if(is_curly_expr(lhs)){
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # Renaming
          rename_by_ref(lhs, expr, mask)
          next
        }

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Mutating

        if(is_parenthesized_twice(lhs)){
          ## adapt lhs and setup variables to delete at end of loop
          lhs <- lhs[[2]][[2]]
          data_nms <- names(.data)
          to_remove <- intersect(all.vars(expr), data_nms)
        }

        if(is.symbol(lhs)){
          nm <- as.character(lhs)
        } else {
          nm <- eval(lhs, envir = .data, enclos = mask)
          if(inherits(nm, "tb_selection")) {
            nm <- tb_select_by_ref(nm, mask)
          }
        }

        if(length(nm) > 1){

          if("." %in% all.vars(expr)) {
            mask$.data[nm] <-
              lapply(nm, transform2, expr, mask)
          } else {
            mask$.data[nm] <- eval(expr, envir = .data, enclos = mask)
          }
          mask$.data[to_remove] <- NULL
          next
        }
      }

      #~~~~~~~~~~
      # =
      if (is_glue_name(nm)){
        stop("A .by argument argument is required to spread/cast/pivot_wider")
      } else {
        ## regular column name
        # we should handle "along" notation in there too
        mask$.data[nm] <- transform2(nm, expr, mask)
        mask$.data[to_remove] <- NULL
      }
    }
  }

  mask$.data
}



