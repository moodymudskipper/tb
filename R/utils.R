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

reparse_dbl_tilde <- function(expr){
  ## counter of arguments to iterate on
  i <- 0
  ## content of arguments to iterate on
  all_iter_args <- list()

  is_prefixed <- function(x) {
    is.call(x) &&
      identical(x[[1]], quote(`~`)) &&
      is.call(x[[2]]) &&
      identical(x[[2]][[1]], quote(`~`))
  }

  reparse0 <- function(call){
    if(!is.call(call)) return(call)
    prefixed_lgl <- sapply(call, is_prefixed)
    if(any(prefixed_lgl)) {
      iter_args <-  lapply(call[prefixed_lgl], `[[`,c(2,2))
      n_iter_args    <-  length(iter_args)
      all_iter_args      <<- append(all_iter_args, iter_args)
      arg_nms   <-  paste0("*",seq(i+1, i <<- i + n_iter_args))
      arg_syms  <-  lapply(arg_nms, as.symbol)
      call[prefixed_lgl] <- arg_syms
    }
    call[] <- lapply(call, reparse0)
    call
  }
  body <- reparse0(expr)

  if(!i) return(expr)
  arg_nms   <-  paste0("*",seq_len(i))
  fun_iter_args <- setNames(replicate(i, substitute()), arg_nms)
  fun <- as.function(c(fun_iter_args, body),envir = parent.frame())
  as.call(c(quote(mapply),fun, all_iter_args))
}

splice_expr <- function(expr, mask){
  if(!is.call(expr)) return(expr)
  expr <- as.list(expr)
  expr <- lapply(expr, function(x) {
    if(is.symbol(x)) return(x)
    if(is.numeric(x) || is.character(x) || is.logical(x)) return(list(x))
    if(has_splice_prefix(x)){
      return(eval(x[[2]], envir = mask$.data, enclos = mask))
    }
    splice_expr(x)
  })
  expr <- unlist(expr,recursive = FALSE)
  expr <- as.call(expr)
  expr
}

has_splice_prefix <- function(x){
  is.call(x) && length(x) == 2 && identical(x[[1]], quote(`+`))
}

is_specified <- function(arg) {
  !is.null(names(arg)) || is_labelled(arg[[1]])
}

# a deparse that doesnt choke on `{`
deparse2 <- function(x){
  paste(deparse(x), collapse ="")
}

transform2 <- function(nm, expr, mask){
  .data <- mask$.data
  mask$. <- .data[[nm]]
  res <- eval(expr,envir = .data, enclos = mask)
  if(inherits(res, "formula")){
    ## mutating along
    along_vars <- get_all_vars(res[-2], .data)
    expr <- res[[2]]
    #expr <- substitute(with(., EXPR), list(EXPR = res[[2]]))
    sub_dfs <- split(as.data.frame(.data), along_vars)

    transformation_fun <- function(sub_df, expr) {
      mask$.subset <- as_tb(sub_df)
      mask$. <- sub_df[[nm]]
      eval(expr, envir = sub_df, enclos = mask)
    }

    res <- rep(NA, nrow(.data))
    split(res, along_vars) <- lapply(sub_dfs, transformation_fun, expr)
    mask$. <- NULL
    mask$.subset <- NULL
  } else {
    # regular mutating
    #.data[[nm]] <- res
  }
  res
}

reorganize_call_i <- function(mc, .i, .j){
  mc <- as.list(mc)
  mc_i <- mc[[".i"]]
  names(mc)[names(mc)==".i"] <- ""
  if(!missing(.j)) {
    if(is_labelled(.j)){
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

is_labelled <- function(x){
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

starts_with_bbb <- function(expr){
  is.call(expr) &&
    identical(expr[[1]], quote(`!`)) &&
    is.call(expr[[2]]) &&
    identical(expr[[2]][[1]], quote(`!`)) &&
    is.call(expr[[2]][[2]]) &&
    identical(expr[[2]][[2]][[1]], quote(`!`))
}

# should be done with double parens ((foo)) and not reserved to
is_parenthesized_twice <- function(expr){
  # the lhs expr[[2]] should be a call
  is.call(expr) &&
    # expr[[2]][[1]] should be `(`
    identical(expr[[1]], quote(`(`)) &&
    is.call(expr[[2]]) &&
    identical(expr[[2]][[1]], quote(`(`))
}

is_curly_expr <- function(expr){
  is.call(expr) && expr[[1]] == quote(`{`)
}


is_glue_name <- function(x){
  grepl("\\{.*?\\}",x)
}

keyval <- function(..., .key = "key", .value = "value", rm = TRUE) {
  x <- tibble(...)
  nms <- names(x)
  x <- split(x, seq(nrow(x)))
  x <- lapply(x, function(x) setNames(stack(x)[2:1],c(.key,.value)))
  x
}
