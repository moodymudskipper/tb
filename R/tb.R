
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
  caller_env <- parent.frame()
  env <- environment()
  class_bkp <- class(.x)
  .x <- as.data.frame(.x)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # subset, and deal with := being used in i j as if it was =
  # so as if it was not fed to i or j
  .i <- substitute(.i)
  .j <- substitute(.j)
  if(!missing(.i)){
    if(has_colon_equal(.i)) {
      mc <- reorganize_call_i(match.call(), .i, .j)
      return(eval.parent(mc))
    } else {
      #.i <- simplify_i(.x, substitute(.i), .by, .along, env = parent.frame())
      .i <- simplify_i(.x, .i, env = parent.frame())
    }
  }

  if(!missing(.j)){
    if(has_colon_equal(.j)) {
      mc <- reorganize_call_j(match.call(), .i, .j)
      return(eval.parent(mc))
    }
    .j <- simplify_j(.x, substitute(.j), .by, env = parent.frame())
  }

  # apply subsetting prior to other operations
  .x <- .x[.i,.j, drop = FALSE]

  # then we go through the dots to apply the modifs

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # initiate variables before tranformations
  dots <- eval(substitute(alist(...)))
  # variables defined by `foo = ...` or `bar := ...`
  new_vars <- character(0)
  # variables removed by the use of `(foo) := ...`
  to_remove <- character(0)

  if(!is.null(.by)){
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # using .by
    .x <- aggregate_tb(.x, dots, .by, env)

  } else {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # no aggregation (.by and .along are NULL)

    # we remove support of unnamed dots
    # if(is_unique_and_unnamed(dots)){
    #   return(eval(dots[[1]], envir = build_mask(.x), enclos = env))
    # }

    for(i in seq_along(dots)){
      expr <- dots[[i]]
      nm <- names(dots)[i]

      # TO DO: we should be able to have parenthesized glue name too!!!
      if(has_colon_equal(expr)) {
        #~~~~~~~~~~
        # :=
        rhs <- dots[[i]][[3]]
        if(has_parenthesised_lhs_symbol(expr)){
          # remove `(` from lhs, compute transformation and add relevant variables to to_remove
          nm <- as.character(dots[[i]][[2]][[2]])
          rhs <- do.call(substitute, list(rhs, list(.. = dots[[i]][[2]][[2]])))
          .x[[nm]] <- eval(rhs, envir = build_mask(.x), enclos = env)
          to_remove <- c(to_remove,intersect(all.vars(rhs), names(.x)))
          new_vars <- c(new_vars,nm)
        } else {
          if(is.symbol(dots[[i]][[2]])){
            nm <- as.character(dots[[i]][[2]])
          } else
            nm <- eval.parent(dots[[i]][[2]])
          rhs <- do.call(substitute, list(rhs, list(.. = dots[[i]][[2]])))
          .x <- transform2(.x, rhs, nm, caller_env)
          new_vars <- c(new_vars,nm)
        }
      } else {
        #~~~~~~~~~~
        # =
        if (is_glue_name(nm)){
          # if we have a `foo{bar}baz` name
          matches <- gregexpr("\\{.*?\\}", nm, perl = T)
          exprs <- regmatches(nm, matches)[[1]]  # curly braces content substrings
          exprs <- gsub("[{}]","", exprs)
          col_nms <- intersect(exprs, names(.x)) # substrings that are also col names
          l <- length(col_nms)
          if(!l) {
            # if no substring are column names :
            # * evaluate all these substrings and
            # * make sure they are scalars
            # * replace the bracket expressions by the relevant values
            # * append the list of new variables
            vals <- sapply(parse(text=exprs), eval ,envir= parent.frame())
            if(!all(lengths(vals) == 1))
              stop("Cannot substitute non scalars in new column names unless they're columns used to spread.")
            regmatches(nm, matches)[[1]] <- vals
            .x <- transform2(.x, expr, nm, caller_env)
            new_vars <- c(new_vars,nm)
          } else {
            stop("spread currently not supported if no .by argument")
          }

          #nms <- eval(bquote(glue::glue(.(nm))), envir = build_mask(.x))
        } else {
          # if the column name is regular, just evaluate expression in right env
          # and append the list of new variables
          .x <- transform2(.x, expr, nm, caller_env)
          new_vars <- c(new_vars,nm)
        }
      }
    }
  }

  # remove parenthesized vars
  to_remove <- unique(to_remove)
  .x[to_remove] <- NULL

  # if we transmute, keep only new vars
  if(.rm) .x <- .x[new_vars]

  # recover class
  class(.x) <- class_bkp

  .x
}


# by splicing with + we will evaluate them in context


