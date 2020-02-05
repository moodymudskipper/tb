question_mark <- function(expr) {
  ## setup
  sc <- sys.calls()
  i <- 1
  found <- FALSE
  for (i in rev(seq_along(sc))) {
    ## walk up the call stack to find occurence of last `[.tb` call
    if (is.name(sc[[c(i, 1)]]) &&  sc[[c(i, 1)]] == quote(`[.tb`)) {
      found <- TRUE
      break
    }
  }
  ## Fail if not found, this shouldn't happen
  if (!found) stop("The `[.tb` call wasn't found in the call stack")
  ## create env as `[.tb`'s environment, containing .data
  env <- sys.frame(i)
  .data <- env$mask$.data

  expr <- substitute(expr)
  expression_contains_dots <- "." %in% all.vars(expr)
  if (expression_contains_dots) {
    expr_fun <- as.function(c(alist(. =), expr))
    nms_lgl <- eval(bquote(sapply(
      .data,  .(expr_fun))), envir = env$mask, enclos = parent.env(env))
    return(names(.data)[nms_lgl])
  }
  ## evaluate the expression
  expr <- eval(expr, parent.env(env))
  if (is.function(expr)) {
    nms_lgl <- sapply(.data,  expr)
    if (!is.logical(nms_lgl))
      stop("The function used after `?` should return a logical output")
    names(.data)[nms_lgl]
  } else if (is.character(expr)) {
    if (length(expr) > 1)
      stop("`?` can't be followed by a character vector of length > 1")
    nms_lgl <- grepl(expr, names(.data))
    names(.data)[nms_lgl]
  } else if (inherits(expr, "formula")) {
    if (length(expr) > 2) stop("`?` can't be followed by a 2 sided formula")
    fun <- as.function(c(alist(.=), expr[[2]]))
    nms_lgl <- sapply(.data, fun)
    if (!is.logical(nms_lgl))
      stop("The formula used after `?` should return a logical output")
    names(.data)[nms_lgl]
  }
}

colon <- function(e1, e2) {
  e1 <- substitute(e1)
  e2 <- substitute(e2)
  if (!(is.symbol(e1) && is.symbol(e2))) {
    # if we don't have numbers we return without losingg further ressources
    return(eval.parent(bquote(base::`:`(.(e1), .(e2)))))
  }
  ## setup
  sc <- sys.calls()
  i <- 1
  found <- FALSE
  for (i in rev(seq_along(sc))) {
    ## walk up the call stack to find occurence of last `[.tb` call
    if (is.name(sc[[c(i, 1)]]) &&  sc[[c(i, 1)]] == quote(`[.tb`)) {
      found <- TRUE
      break
    }
  }
  ## Fail if not found, this shouldn't happen
  if (!found) stop("The `[.tb` call wasn't found in the call stack")
  ## create env as `[.tb`'s envir, containing .data, and caller_env its parent
  env <- sys.frame(i)
  nms <- names(env$mask$.data)
  if (is.symbol(e1)) {
    e1_ <- match(as.character(e1), nms)
    if (!is.na(e1_)) e1 <- e1_
  }
  if (is.symbol(e2)) {
    e2_ <- match(as.character(e2), nms)
    if (!is.na(e2_)) e2 <- e2_
  }
  eval.parent(bquote(base::`:`(.(e1), .(e2))))
}
