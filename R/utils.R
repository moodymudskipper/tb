transform2 <- function(.x, expr, nm, env){
  res <- eval(expr,envir = build_mask(.x), enclos = env)
  if(inherits(res, "formula")){
    along_vars <- get_all_vars(res[-2], .x)
    expr <- substitute(with(., EXPR), list(EXPR = res[[2]]))
    fun <- as.function(c(alist(.=), expr))
    environment(fun) <- env
    if(is.null(.x[[nm]])) .x[[nm]] <- NA # add column if relevant
    split(.x[[nm]], along_vars) <- lapply(split(.x, along_vars), fun)
  } else {
    .x[[nm]] <- res
  }
  .x
}

reorganize_call_i <- function(mc, .i, .j){
  mc <- as.list(mc)
  mc_i <- mc[[".i"]]
  names(mc)[names(mc)==".i"] <- ""
  if(!missing(.j)) {
    if(has_colon_equal(.j)){
      # if we have := in both .i and .j
      # mc_j <- mc[[".j"]]
      names(mc)[names(mc)==".j"] <- ""
      mc <- append(mc,c(substitute(),substitute()),2)
    } else {
      # if we have := in .i and a legit j
      mc[[".i"]] <- NULL
      mc <- append(mc,substitute(),2)
      mc <- append(mc,mc_i,4)
    }
  } else {
    # if we have := in .i and missing .j
    mc <- append(mc,c(substitute(),substitute()),2)
  }
  mc <- as.call(mc)
}

reorganize_call_j <- function(mc, .i, .j){
  mc <- as.list(mc)
  names(mc)[names(mc)==".j"] <- ""
  if(missing(.i)){
  mc <- append(mc, c(substitute(), substitute()), 2)
  } else {
    mc <- append(mc, substitute(), 3)
  }
  mc <- as.call(mc)
}



has_colon_equal <- function(x){
  # expr should be a call
  # expr[[1]] should be `:=`
  is.call(x) && identical(x[[1]], quote(`:=`))
}

is_unique_and_unnamed <- function(x){
  length(x) == 1 &&
    allNames(x) == "" &&
    (!is.call(x[[1]]) || !identical(x[[1]][[1]], quote(`:=`)))
}

is_function_symbol_or_formula <- function(x){
  (is.call(x) && x[[1]] == quote(`~`)) || is.symbol(x) && !is.null(get0(as.character(x), mode = "function"))
}

as_function2 <- function(f){
  f <- eval(f)
  if(inherits(f, "formula")){
    if(length(f) > 2) stop("The formula notation requires a one-sided formula")
    as.function(c(alist(.=),f[[2]]))
  } else f
}

summarize_all2 <- function(df, f, by){
  x <- df
  x[by] <- NULL
  aggregate.data.frame(x, df[by], f)
}


# * handles the .by argument
# * turns NA to FALSE
# * ...
# simplify_i <- function(.x, .i, .by, .along, env = parent.frame()){
#   # evaluate it in the context of data frame
#   .i <- eval(.i, envir=.x, enclos= env)
#   if(is.null(.by) && is.null(.along)){
#     # turn NAs to FALSE so we keep only TRUE indices
#     if(is.logical(.i)) .i[is.na(.i)] <- FALSE
#   } else {
#     if(!is.numeric(.i)) {
#       stop("if using `.by` and `.i` non missing, `.i` must be numeric")
#     }
#     # build ave call
#     call <- as.call(c(
#       quote(ave),                         # ave(
#       list(x = seq(nrow(.x))),            #   seq(nrow(.x)),
#       lapply(c(.by, .along), as.symbol),  #   grpvar1, grpvar2, ...,
#       list(FUN = seq_along)))             #   Fun = seq_along)
#     .i <- eval(call, envir = .x) %in% .i
#   }
#   .i
# }


simplify_i <- function(.x, .i, env = parent.frame()){
  # evaluate it in the context of data frame
  .i <- eval(.i, envir=.x, enclos= env)
  if(!inherits(.i, "formula")){
    # turn NAs to FALSE so we keep only TRUE indices
    if(is.logical(.i)) .i[is.na(.i)] <- FALSE
  } else {
    along_vars <- get_all_vars(.i[-2], .x)
    .i <- eval(.i[[2]], envir=.x, enclos= env)
    if(!is.numeric(.i)) {
      stop("if using `.by` and `.i` non missing, `.i` must be numeric")
    }
    # build ave call
    call <- as.call(c(
      quote(ave),                         # ave(
      list(x = seq(nrow(.x))),            #   seq(nrow(.x)),
      along_vars,  #   grpvar1, grpvar2, ...,
      list(FUN = seq_along)))             #   Fun = seq_along)
    .i <- eval(call, envir = .x) %in% .i
  }
  .i
}

simplify_j <- function(.x, .j, .by, env = parent.frame()){
  if(is.call(.j)){
    if(identical(.j[[1]], quote(`?`))){
      # if .j starts with `?`
      .j <- eval(bquote(sapply(.x, .(.j[[2]]))))
    } else if(identical(.j[[1]], quote(`:`)) &&
              is.symbol(.j[[2]]) &&
              is.symbol(.j[[3]])){
      # if .j is of form col1:col2
      nms <- names(.x)
      .j1 <- match(as.character(.j[[2]]), nms)
      .j2 <- match(as.character(.j[[3]]), nms)
      .j <- .j1:.j2
    } else {
      # else evaluate in context of df
      .j <- eval(.j, envir=.x, enclos= env)
    }
  } else {
    .j <- eval(.j, envir=.x, enclos= env)
  }
  # turn NAs to FALSE so we keep only TRUE indices
  if(is.logical(.j)) .j[is.na(.j)] <- FALSE
  if(!is.character(.j)) .j <- names(.x)[.j]
  .j <- unique(c(.by, .j))
}

starts_with_bbb <- function(expr){
  is.call(expr) &&
    identical(expr[[1]], quote(`!`)) &&
    is.call(expr[[2]]) &&
    identical(expr[[2]][[1]], quote(`!`)) &&
    is.call(expr[[2]][[2]]) &&
    identical(expr[[2]][[2]][[1]], quote(`!`))
}

has_parenthesised_lhs_symbol <- function(expr){
    # the lhs expr[[2]] should be a call
    is.call(expr[[2]]) &&
    # expr[[2]][[1]] should be `(`
    expr[[2]][[1]] == quote(`(`) &&
    # The parenthesised expr expr[[2]][[1]] should be a symbol
    is.symbol(expr[[2]][[2]])
}


is_glue_name <- function(x){
  grepl("\\{.*?\\}",x)
}

build_mask <- function(x){
  c(as.list(x), list(. = x))
}

keyval <- function(..., .key = "key", .value = "value", rm = TRUE) {
  x <- tibble(...)
  nms <- names(x)
  x <- split(x, seq(nrow(x)))
  x <- lapply(x, function(x) setNames(stack(x)[2:1],c(.key,.value)))
  x
}

as_tb <- function(x){
  class(x) <- c("tb", class(x))
  x
}
