# does transmuting by ref but returns column names!
tb_select_by_ref <- function(.j, mask){
  .data <- mask$.data
  data_nms <- names(.data)
  arg_nms <- tb_names(.j)
  s_nms <- character(0)
  for(i in seq_along(.j)){
    arg <- .j[[i]]

    ## deal with named arguments
    arg_is_named <- arg_nms[[i]] != ""
    if(arg_is_named){
      ## append name of argument to s_nms
      s_nms <- append(s_nms, arg_nms[[i]])
      ## operate transmute call
      # TODO
      next
    }

    ## recognize `-` prefix
    has_minus_prefix <- is.call(arg) && identical(arg[[1]], quote(`-`))
    if(has_minus_prefix){
      arg <- arg[[2]]
      negative <- TRUE
      if(i == 1) s_nms <- data_nms
    } else {
      negative <- FALSE
    }

    if(is.symbol(arg)) {
      arg_nm <- as.character(arg)
      if(arg_nm %in% data_nms) {
        if(negative) {
          s_nms <- setdiff(s_nms, arg_nm)
        } else {
          s_nms <- append(s_nms, arg_nm)
        }
        next
      }
    }
    ## evaluate arg in the context of df and mask
    arg <- eval(arg, envir= .data, enclos= mask)
    if(is.numeric(arg)) {
      if(all(arg >= 0)) {
        nms <- data_nms[arg]
        if(negative) {
          s_nms <- setdiff(s_nms, nms)
        } else {
          s_nms <- append(s_nms, nms)
        }
        next
      }
      if(all(arg <= 0)) {
        nms <- data_nms[-arg]
        if(negative) stop("You used a negative prefix on an expression that returned a negative numeric")
        if(i == 1) s_nms <- data_nms
        s_nms <- setdiff(s_nms, nms)
        next
      }
      stop("only 0's may be mixed with negative subscripts")
    }


    if(is.logical(arg)) {
      nms <- data_nms[arg]
      if(negative) {
        ## remove variable(s) as argument was prefixed with `-`
        s_nms <- setdiff(s_nms, nms)
      } else {
        ## add variable(s)
        s_nms <- append(s_nms, nms)
      }
      next
    }

    if(is.character(arg)) {
      if(negative) {
        ## remove variable(s) as argument was prefixed with `-`
        s_nms <- setdiff(s_nms, arg)
      } else {
        ## add variable(s)
        s_nms <- append(s_nms, arg)
      }
      next
    }
    stop("argument type is not supported")
  }
  unique(s_nms)
}



#' select and transform columns
#'
#' This reproduces the functionality of SQL select, `dplyr::transmute()` or
#' *data.table*'s use of `.()` in `.j`.
#'
#' @param ... expressions
#' @export
s <- function(...){
  args <- as.list(match.call()[-1])
  class(args) <- "tb_selection"
  args
}

# sequence notation was not implemented yet!

# tb_select <- function(.x, .j, env){
#   arg_nms <- tb_names(.j)
#   s_nms <- character(0)
#   for(i in seq_along(.j)){
#     arg <- .j[[i]]
#     if(arg_nms[[i]] != ""){
#       ## append name of argument to s_nms
#       s_nms <- append(s_nms, arg_nms[[i]])
#     } else {
#       if(is.symbol(arg)) {
#         ## append deparsed symbol to s_nms
#         s_nms <- append(s_nms, deparse(arg))
#       } else if(is.call(arg) &&
#                 identical(arg[[1]], quote(`:`)) &&
#                 is.symbol(arg[[2]]) &&
#                 is.symbol(arg[[3]])){
#         ## replace .j call by a numeric sequence
#         nms <- names(.x)
#         .j1 <- match(as.character(arg[[2]]), nms)
#         .j2 <- match(as.character(arg[[3]]), nms)
#         .j <- s_nms <- nms[.j1:.j2]
#       } else {
#         ## evaluate arg in the context of df and mask
#         arg <- eval(arg, envir=c(as.list(.x), list(.x = .x, `?` = question_mark)), enclos= env)
#
#         if (inherits(arg, "formula")){
#           one_sided_formula_lgl <- length(arg) == 2
#           if(one_sided_formula_lgl) {
#             ## use regex on col names
#             arg_lgl <- grepl(eval(arg[[2]], env), names(.x))
#             arg <- names(.x)[arg_lgl]
#
#           } else {
#             stop("unnamed 2 sided formulas are not supported in `s()`")
#           }
#         }
#         ## append to s_nms
#         s_nms <- append(s_nms, arg)
#       }
#     }
#   }
#   unique(s_nms)
# }

tb_names <- function(args, env){
  nms <- allNames(args)
  unnamed <- nms == ""
  nms[unnamed] <- sapply(args[unnamed], function(x) {
    if(is.call(x) && x[[1]] == quote(`:=`)) {
      if(is.symbol(x[[2]])) deparse(x[[2]]) else eval(x[[2]], env)
    } else ""
  })
  nms
}

tb_args <- function(args, env){
  nms <- allNames(args)
  unnamed <- nms == ""
  nms[unnamed] <- sapply(args[unnamed], `[[`, 2)
  exprs <- unname(args)
  exprs[unnamed] <- sapply(args[unnamed], `[[`, 3)
  list(names = nms, exprs = exprs)
}

tb_transmute <- function(.x, .j, env){
  .x
  # implement mutate first
}

# we should deal with case where names are parenthesized
