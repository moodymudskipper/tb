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
  sc <- as.list(sys.call())
  # if call is tb[], return tb
  if(length(sc) == 3 && identical(sc[[3]], substitute())) return(.X)

  mask <- new.env(parent = pf)
  mask[[".X"]] <- .X
  mask[[".data"]] <- .X
  mask[["?"]] <- question_mark
  mask[[":"]] <- colon

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## fetch dot expressions and expand
  dots <- eval(substitute(alist(...)))
  dots <- lapply(dots, expand_expr, pf)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## subset, and deal with := being used in i j as if it was =
  # so as if it was not fed to i or j

  # if only one arg is fed besides .X and drop and that this arg is unnamed,
  # feed to .j


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

  ## reorganize call if labelled args are fed to .i or .j

  dots0 <- list()
  # if(any(tb_names(sc, env = pf) %in% c(".i", ".j")))
  #   stop("`.i` and `.j` should not be named")

  if(is_specified(sc[3])){
    # if 1st bracket arg is specified, .i and .j can only be missing and we sort out the call
    other_args <- sc[-(1:3)]
    .i <- substitute()
    .j <- substitute()
    any_dot_is_unspecified <-
      any(!vapply(split(other_args, seq_along(other_args)), is_specified, logical(1)))
    if(any_dot_is_unspecified)
      stop(".i must be given first, left blank, or omitted, but cannot be given after another specified argument")

    # if .i and/or .j are labelled, we must take their content and add it to the dots
    if(is_labelled(sc[[3]])) {
      dots0[[1]] <- expand_expr(sc[[3]], pf)
    }
    if(length(sc) > 3 && is_labelled(sc[[4]])) {
      dots0 <- c(dots0, expand_expr(sc[[4]], pf))
    }
  } else {
    # if 1st bracket arg is NOT specified, it's a proper .i, either missing or not
    if(missing(.i)) {
      .i <- substitute(.i)
    } else {
      .i <-expand_expr(substitute(.i), pf)
      .i <- splice_expr(.i, mask)
      row_subset_by_ref(.i, mask)
    }


    # if 2nd bracket arg is specified, .j can only be missing and we sort out the call
    if(length(sc) >3 && is_specified(sc[4])){
      .j <- substitute()
      other_args <- sc[-(1:4)]
      any_dot_is_unspecified <- any (!vapply(split(other_args,seq_along(other_args)), is_specified, logical(1)))
      if(any_dot_is_unspecified)
        stop(".j must be given second, left blank, or omitted, but cannot be given after another specified argument")
      if(is_labelled(sc[[4]])) {
        dots0[[1]] <- expand_expr(sc[[4]], pf)
      }
    } else {
      # if 2nd bracket arg is NOT specified, it's a proper .j, either missing or not
      if(missing(.j)) {
        .j <- substitute(.j)
      } else {
        .j <-expand_expr(substitute(.j), pf)
        .j <- splice_expr(.j, mask)
        col_subset_by_ref(.j, mask, .by)
      }
    }
  }

  dots <- c(dots0, dots)

  ## splice dots
  dots <- lapply(dots, function(x) {
    if(is.numeric(x) || is.character(x)) return(list(x))
    if(has_splice_prefix(x)){
      return(eval(x[[2]], envir = mask$.data, enclos = mask))
    }
    x
  })
  dots <- unlist(dots, recursive = FALSE)

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
        lhs <- reparse_dbl_tilde(lhs)
        expr <- reparse_dbl_tilde(expr)

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
          if(is.logical(nm)) {
            nm <- names(.data)[nm]
          }
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
      } else {
        expr <- reparse_dbl_tilde(expr)
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



