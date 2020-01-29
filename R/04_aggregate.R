# create summarize_labelled_by_ref and summarize_named_by_ref and modularize aggregate_tb
# harmonize these functions so they look consistent to the mutate counterpart, maybe change the latter too
# see if we can either :
# * rename aggregate_tb into `summarize_dots_by_ref` (and make it work by ref!) AND create mutate_dots_by_ref too
# * or integrate what's left after modularization into bracket function


summarize_dots <- function(dots, mask, by){
  .data <- mask$.data
  .data_df <- .data
  data_nms <- names(.data)

  if(length(by)){
    ## convert to factor to keep groups with NAs
    # this handles by even if it its a selection objects or if it creates anew column
    by_cols_as_factors <- lapply(
      subset_j(.data, by),
      function(x) factor(x, as.character(unique(x)), exclude = NULL))

    ## split data along by columns
    sub_dfs <- split(.data_df, by_cols_as_factors,drop=TRUE)
    sub_dfs <- lapply(sub_dfs, as_tb)
    ## initiate with unique values of by columns
    # we ll append it with aggregations
    output <- unique(.data_df[by])
  } else {
    sub_dfs <- list(.data_df)
    output <- structure(list(), row.names = 1L, class = c("tb", "data.frame"))
  }
  rownames(output) <- NULL

  nms <- allNames(dots)
  for(i in seq_along(dots)){
    expr <- dots[[i]]
    nm <- nms[[i]]
    if(is_labelled(expr)) {
      output <- summarize_labelled(expr, nm, output, sub_dfs, by, mask)
    } else {
      output <- summarize_named(expr, nm, output, sub_dfs, by, mask)
    }
  }
output
}

summarize_named <- function(expr, nm, output, sub_dfs, by, mask){
  if(nm == "") {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## unnamed arguments are not supported
    stop("All arguments should be named (using `=`), or labelled (using `:=`)")
  }
  expr <- reparse_dbl_tilde(expr)

  if (is_glue_name(nm)){
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## spread
    res <- tb_spread(nm, expr, sub_dfs, mask, by)
    output[names(res)] <- res
  } else {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Standard case
    output[[nm]] <- tb_standard_summarize(nm, expr, sub_dfs, mask)
  }
  output
}


summarize_labelled <- function(expr, nm, output, sub_dfs, by, mask){
  data_nms <- names(mask$.data)
  if(nm != "") {
    stop("An argument of `[.tb` can't be both named (using `=`) and labelled (using `:=`).")
  }
  nm   <- reparse_dbl_tilde(expr[[2]])
  expr <- reparse_dbl_tilde(expr[[3]])

  if (is_curly_expr(nm)) {
    stop("Renaming expressions, using syntax `{var} := expr`, are not supported when aggregating with `by`")
  }
  if (is_parenthesized_twice(nm)) {
    stop("Morphing expressions, using syntax `((var)) := expr`, are not supported when aggregating with `by`")
  }
  if(is.symbol(nm)){
    nm <- as.character(nm)
  } else {
    nm <- eval(nm, envir = mask$.data, enclos = mask)
    if (is.numeric(nm) || is.logical(nm)){
      nm <- data_nms[nm]
    } else if (inherits(nm, "tb_selection")) {
      nm <- modify_by_ref_and_return_selected_names(nm, mask)
    } else if (!is.character(nm)){
      stop("the lhs `", deparse2(arg), "` evaluates to an unsupported type")
    }
  }
  if (length(nm) == 1) {
    return(summarize_named(expr, nm, output, sub_dfs, by, mask))
  }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Handle character lhs of length > 1
  if("." %in% all.vars(expr)){
    output[nm] <- lapply(nm, tb_standard_summarize, expr, sub_dfs, mask)
  } else {
    output[nm] <- do.call("rbind", tb_standard_summarize2(expr, sub_dfs, mask))
  }
  output
}


tb_spread <- function(nm, arg, sub_dfs, mask, by){
  data_nms <- names(mask$.data)
  matches <- gregexpr("\\{.*?\\}", nm, perl = T)
  exprs <- regmatches(nm, matches)[[1]]  # curly braces content substrings
  exprs <- gsub("[{}]","", exprs)
  col_nms <- intersect(exprs, data_nms) # substrings that are also col names

  transformation_fun0 <- function(sub_sub_df, expr){
    mask$.subset <- sub_sub_df
    eval(expr, envir = sub_sub_df, enclos = mask)
  }

  transformation_fun <- function(sub_df, expr) {

    ## convert to factor to keep groups with NAs
    spread_cols_as_factors <- lapply(
      subset_j(sub_df, col_nms),
      function(x) factor(x, as.character(unique(x)), exclude = NULL))
    # spread_output
    spread_output <- unique(subset_j(sub_df,by))
    ## split data along spread columns
    #sub_sub_dfs <- split(as.data.frame(sub_df), spread_cols_as_factors,drop = TRUE)
    sub_sub_dfs <- split(sub_df, spread_cols_as_factors,drop = TRUE)
    names(sub_sub_dfs) <- glue::glue_data(unique(sub_df[col_nms]), nm)

    cols <- lapply(sub_sub_dfs, transformation_fun0, expr)
    spread_output[names(cols)] <- cols
    spread_output

  }
  res <- lapply(sub_dfs, transformation_fun, arg)
  res <- as_tb(data.table::rbindlist(res,fill=TRUE))
  spread_nms <- setdiff(names(res), by)
  res <- res[spread_nms]
}

tb_standard_summarize <- function(nm, arg, sub_dfs, mask){
  ## define transformation_fun which applies the transformation given by
  ## a dot expression through the mask
  transformation_fun <- function(sub_df, expr) {
    mask$.subset <- sub_df
    mask$. <- sub_df[[nm]]
    eval(expr, envir = sub_df, enclos = mask)
  }
  ## compute this transformation for all subdfs
  res <- lapply(sub_dfs, transformation_fun, arg)
  mask$.subset <- NULL
  mask$. <-  NULL
  ## simplify if all results are scalar
  if(all(sapply(res, is.atomic) & lengths(res) == 1))
    res <- unlist(res)
  ## append the data
  res
}


tb_standard_summarize2 <- function(arg, sub_dfs, mask){
  ## define transformation_fun which applies the transformation given by
  ## a dot expression through the mask
  transformation_fun <- function(sub_df, expr) {
    mask$.subset <- sub_df
    eval(expr, envir = sub_df, enclos = mask)
  }
  ## compute this transformation for all subdfs
  res <- lapply(sub_dfs, transformation_fun, arg)
  mask$.subset <- NULL
  mask$. <-  NULL
  ## simplify if all results are scalar
  if(all(sapply(res, is.atomic) & lengths(res) == 1))
    res <- unlist(res)
  ## append the data
  res
}
