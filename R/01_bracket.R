

# note : we must also deal with :
# mtc[(`{x}`) := mpg * cyl, (bar) := disp + hp + drat + mpg, wt > 3]
# meaning 2 first unnamed parameters should be brought first
# our call should actually really start with a call to a reformatting function
# Probably better to IMPOSE that all args are named except i and j, unnamed arguments are ok
# for [[
# maybe rename to `*i*´ and `*j*` too, and discourage in the doc to name arguments
# calling those

# we could actually just forbid unnamed arguments that would not be given FIRST
# This would impose readable syntax

#' Title
#'
#' desc
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

          # remove `(` from lhs, compute transformation and add relevant variables to to_remove
          #
          # sym <- lhs[[2]][[2]]
          # nm <- as.character(sym)
          # nms <- names(.data)
          # expr <- do.call(substitute, list(expr, list(. = sym)))
          # mask$.data[[nm]] <- eval(expr, envir = .data, enclos = mask)
          # to_remove <- intersect(all.vars(expr), nms)
          # mask$.data[to_remove] <- NULL
          # next
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
        # else {
        #   if(is.symbol(dots[[i]][[2]])){
        #     nm <- as.character(dots[[i]][[2]])
        #   } else
        #     nm <- eval.parent(dots[[i]][[2]])
        #   rhs <- do.call(substitute, list(rhs, list(.. = dots[[i]][[2]])))
        #   data <- transform2(data, rhs, nm, pf)
        #   new_vars <- c(new_vars,nm)
        # }
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

expand_expr <- function(expr, where) {
  # taken right from bquote's code
  unquote <- function(e) if (is.pairlist(e))
    as.pairlist(lapply(e, unquote))
  else if (length(e) <= 1L)
    e
  else if (e[[1L]] == as.name("."))
    eval(e[[2L]], where)
  else as.call(lapply(e, unquote))
  unquote(expr)
}


#'
#'
#'
#' # note : we must also deal with :
#' # mtc[(`{x}`) := mpg * cyl, (bar) := disp + hp + drat + mpg, wt > 3]
#' # meaning 2 first unnamed parameters should be brought first
#' # our call should actually really start with a call to a reformatting function
#' # Probably better to IMPOSE that all args are named except i and j, unnamed arguments are ok
#' # for [[
#' # maybe rename to `*i*´ and `*j*` too, and discourage in the doc to name arguments
#' # calling those
#'
#' # we could actually just forbid unnamed arguments that would not be given FIRST
#' # This would impose readable syntax
#'
#' #' Title
#' #'
#' #' desc
#' #'
#' #' `...` can contain `foo = expr` arguments such as those
#' #'
#' #' @param .x teebee object
#' #' @param .i numeric, logical, character or formula to subset rows, i a formula
#' #'   the lhs must evaluate to numeric and the rhs specifies the variables to
#' #'   slice along.
#' #' @param .j numeric, logical or character to subset columns
#' #' @param ... Name-value pairs of expression to be evaluated in the context of
#' #'   the teebee, see details
#' #' @param .by variables to aggregate by
#' #' @param .rm if TRUE columns not created by the call or part of .by cols will be
#' #'   removed
#' #' @param drop
#' #' @param .unchop
#'
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' `[.tb` <- function(.x, .i, .j, ...,
#'                    .by = NULL, .along = NULL,
#'                    .rm = FALSE, drop = FALSE,
#'                    .unchop = FALSE){
#'   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'   ## setup
#'   caller_env <- parent.frame()
#'   env <- environment()
#'   # we copy .x so we can still access the original
#'   data <- as.data.frame(.x)
#'   .x <- .x
#'
#'   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'   ## subset, and deal with := being used in i j as if it was =
#'   # so as if it was not fed to i or j
#'
#'   # if only one arg is fed besides .x and drop and that this arg is unnamed,
#'   # feed to .j
#'   one_unnamed_arg_besides_.x_lgl <-
#'     (nargs() - !missing(drop)) == 2 &&
#'     is.null(names(arg <- sys.call()[3])) &&
#'     !(is.call(arg[[1]]) && arg[[1]][[1]] == quote(`:=`))
#'
#'   if(one_unnamed_arg_besides_.x_lgl){
#'     ## assign .i's quoted input to .j, dealing with .(), and make .i empty
#'     .j <- eval(substitute(
#'       bquote(X, where = caller_env),
#'       list(X = substitute(.i))))
#'     .i <- substitute()
#'   } else {
#'     ## assign to .i and .j their quoted input , dealing with .()
#'   .i <- eval(substitute(
#'     bquote(X, where = caller_env),
#'     list(X = substitute(.i))))
#'   .j <- eval(substitute(
#'     bquote(X, where = caller_env),
#'     list(X = substitute(.j))))
#'   }
#'   if(!missing(.i)){
#'     if(is_labelled(.i)) {
#'       mc <- reorganize_call_i(match.call(), .i, .j)
#'       return(eval.parent(mc))
#'     } else {
#'       # evaluate .i in the context of the data frame, handle .by if given
#'       .i <- simplify_i(data, .i, env = parent.frame())
#'     }
#'   }
#'
#'   if(!missing(.j)){
#'     if(is_labelled(.j)) {
#'       mc <- reorganize_call_j(match.call(), .i, .j)
#'       return(eval.parent(mc))
#'     }
#'     .j   <- simplify_j(data, substitute(.j), .by, env = caller_env)
#'     if (inherits(.j, "tb_selection")){
#'
#'       data  <- tb_transmute(data, .j, env = caller_env)
#'       .j <- tb_select(data, .j, env = caller_env)
#'     }
#'   }
#'
#'   ## apply subsetting prior to other operations
#'   data <- data[.i,.j, drop = FALSE]
#'
#'   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'   ## fetch the quoted dot inputs, dealing with .()
#'   dots <- eval(substitute(alist(...)))
#'   dots <- lapply(dots, function(x) {
#'     eval(substitute(
#'       bquote(X, where = caller_env),
#'       list(X = x)))
#'   })
#'
#'   # variables defined by `foo = ...` or `bar := ...`
#'   new_vars <- character(0)
#'   # variables removed by the use of `(foo) := ...`
#'   to_remove <- character(0)
#'
#'   if(!is.null(.by)){
#'     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'     # using .by
#'     data <- aggregate_tb(data, dots, .by, env, .x)
#'
#'   } else {
#'     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'     # no aggregation (.by and .along are NULL)
#'
#'     # we remove support of unnamed dots
#'     # if(is_unique_and_unnamed(dots)){
#'     #   return(eval(dots[[1]], envir = build_mask(x), enclos = env))
#'     # }
#'
#'     for(i in seq_along(dots)){
#'       expr <- dots[[i]]
#'       nm <- names(dots)[i]
#'
#'       # TO DO: we should be able to have parenthesized glue name too!!!
#'       if(is_labelled(expr)) {
#'         #~~~~~~~~~~
#'         # :=
#'         rhs <- dots[[i]][[3]]
#'         if(has_parenthesised_lhs_symbol(expr)){
#'           # remove `(` from lhs, compute transformation and add relevant variables to to_remove
#'           nm <- as.character(dots[[i]][[2]][[2]])
#'           rhs <- do.call(substitute, list(rhs, list(.. = dots[[i]][[2]][[2]])))
#'           data[[nm]] <- eval(rhs, envir = build_mask(data), enclos = env)
#'           to_remove <- c(to_remove,intersect(all.vars(rhs), names(data)))
#'           new_vars <- c(new_vars,nm)
#'         } else {
#'           if(is.symbol(dots[[i]][[2]])){
#'             nm <- as.character(dots[[i]][[2]])
#'           } else
#'             nm <- eval.parent(dots[[i]][[2]])
#'           rhs <- do.call(substitute, list(rhs, list(.. = dots[[i]][[2]])))
#'           data <- transform2(data, rhs, nm, caller_env)
#'           new_vars <- c(new_vars,nm)
#'         }
#'       } else {
#'         #~~~~~~~~~~
#'         # =
#'         if (is_glue_name(nm)){
#'           # if we have a `foo{bar}baz` name
#'           matches <- gregexpr("\\{.*?\\}", nm, perl = T)
#'           exprs <- regmatches(nm, matches)[[1]]  # curly braces content substrings
#'           exprs <- gsub("[{}]","", exprs)
#'           col_nms <- intersect(exprs, names(data)) # substrings that are also col names
#'           l <- length(col_nms)
#'           if(!l) {
#'             # if no substring are column names :
#'             # * evaluate all these substrings and
#'             # * make sure they are scalars
#'             # * replace the bracket expressions by the relevant values
#'             # * append the list of new variables
#'             vals <- sapply(parse(text=exprs), eval ,envir= parent.frame())
#'             if(!all(lengths(vals) == 1))
#'               stop("Cannot substitute non scalars in new column names unless they're columns used to spread.")
#'             regmatches(nm, matches)[[1]] <- vals
#'             data <- transform2(data, expr, nm, caller_env)
#'             new_vars <- c(new_vars,nm)
#'           } else {
#'             stop("spread currently not supported if no .by argument")
#'           }
#'
#'           #nms <- eval(bquote(glue::glue(.(nm))), envir = build_mask(data))
#'         } else {
#'           # if the column name is regular, just evaluate expression in right env
#'           # and append the list of new variables
#'           data <- transform2(data, expr, nm, caller_env)
#'           new_vars <- c(new_vars,nm)
#'         }
#'       }
#'     }
#'   }
#'
#'   # remove parenthesized vars
#'   to_remove <- unique(to_remove)
#'   data[to_remove] <- NULL
#'
#'   # if we transmute, keep only new vars
#'   if(.rm) data <- data[new_vars]
#'
#'   # recover class
#'   class(data) <- class(.x)
#'
#'   data
#' }
#'
#'
#' # by splicing with + we will evaluate them in context
#'
#'
