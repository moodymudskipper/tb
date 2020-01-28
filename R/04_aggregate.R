# Note : the mask should be an environment, not a list
# it should be defined in `[` and passed as argument (it has ref semantics!)
# then should be modified adhoc with use cases

aggregate_tb <- function(dots, mask, .by){
  .data <- mask$.data
  .data_df <- .data #as.data.frame(.data) # we could use data.table to set class by ref
  data_nms <- names(.data)

  if(length(.by)){
    ## convert to factor to keep groups with NAs
    # this handles .by even if it its a selection objects or if it creates anew column
    by_cols_as_factors <- lapply(
      subset_j(.data, .by),
      function(x) factor(x, unique(x), exclude = NULL))

    ## split data along by columns
    sub_dfs <- split(.data_df, by_cols_as_factors,drop=TRUE)
    sub_dfs <- lapply(sub_dfs, as_tb)
    ## initiate with unique values of .by columns
    # we ll append it with aggregations
    output <- unique(.data_df[.by])
  } else {
    sub_dfs <- list(.data_df)
    output <- structure(list(), row.names = 1L, class = c("tb", "data.frame"))
  }

  rownames(output) <- NULL

  nms <- allNames(dots)
  for(i in seq_along(dots)){
    nm <- nms[[i]]
    element_is_named <- nm != ""

    if (is_labelled(dots[[i]])){
      if(element_is_named) {
        stop("An argument of `[.tb` can't be both named (using `=`) and labelled (using `:=`).")
      }
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Handle labelled arguments
      nm <- dots[[i]][[2]] # unevaluated_name

      nm <- reparse_dbl_tilde(nm)
      expr <- dots[[i]][[3]]
      expr <- reparse_dbl_tilde(expr)
      if (is_curly_expr(nm)) {
        "Renaming expressions, using syntax `{var} := expr`, are not supported when aggregating with `.by`"
      }
      if (is_parenthesized_twice(nm)) {
        "Morphing expressions, using syntax `((var)) := expr`, are not supported when aggregating with `.by`"
      }
      if(is.symbol(nm)){
        nm <- as.character(nm)
      } else {
        nm <- eval(nm, envir = .data, enclos = mask)
        if (is.numeric(nm) || is.logical(nm)){
          nm <- data_nms[nm]
        } else if (inherits(nm, "tb_selection")) {
          nm <- tb_select_by_ref(nm, mask)
        } else if (!is.character(nm)){
          stop("the lhs `", deparse2(arg), "` evaluates to an unsupported type")
        }
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ## Handle character lhs of length > 1
        if (length(nm) > 1) {
          if("." %in% all.vars(expr)){
            output[nm] <- lapply(nm, tb_standard_summarize, expr, sub_dfs, mask)
          } else {
            output[nm] <- do.call("rbind", tb_standard_summarize2(expr, sub_dfs, mask))
          }
          next
        }
      }

    } else {
      if(!element_is_named) {
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## unnamed arguments are not supported
      stop("All arguments should be named (using `=`), or labelled (using `:=`)")
      }
      expr <- dots[[i]]
      expr <- reparse_dbl_tilde(expr)
    }


    if (is_glue_name(nm)){
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## spread
      res <- tb_spread(nm, expr, sub_dfs, mask, .by)
      output[names(res)] <- res
    } else {
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Standard case
      output[[nm]] <- tb_standard_summarize(nm, expr, sub_dfs, mask)
    }
  }
output
}


# if(is_unique_and_unnamed(dots)){
#   # deal with unnamed dots : maybe it should just fail and this should be done
#   # in tb[[...]] ?
#   # On the other hand splicing must be handled and is done with unnamed args!
#   # or maybe we should always splice unnamed args ????
#   stop("unnamed arguments are not supported! when they will be, unnamed arguments will be spliced AFTER COMPUTATION")
#   # if(is_function_symbol_or_formula(dots[[1]])) {
#   #   f <- as_function2(dots[[1]])
#   #   data <- summarize_all2(data, f , .by)
#   #   class(data) <- class_bkp
#   # } else {
#   #   fun <- function(sub_df, expr, env) {
#   #     eval(expr, envir = build_mask(sub_df), enclos = env)
#   #   }
#   #   #browser()
#   #   sub_dfs <- split(data, lapply(data[.by], function(x) factor(x, unique(x), exclude = NULL)))
#   #   data <- sapply(sub_dfs, fun, dots[[1]], env)
#   # }
#   # return(data)
# }




tb_spread <- function(nm, arg, sub_dfs, mask, .by){
  #browser()
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
      function(x) factor(x, unique(x), exclude = NULL))
    # spread_output
    spread_output <- unique(subset_j(sub_df,.by))
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
  spread_nms <- setdiff(names(res), .by)
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
    #mask$. <- sub_df[[nm]]
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


# tb_standard_summarize <- function(data, nm, arg, sub_dfs, .X, env, .by){
#   ## define transformation_fun which applies the transformation given by
#   ## a dot expression through the mask
#   transformation_fun <- function(sub_df, expr) {
#     mask <- c(as.list(sub_df), list(
#       .sd = sub_df,
#       .data = data,
#       . = sub_df[[nm]],
#       .nm = nm,
#       .X = .X,
#       `?` = question_mark))
#     eval(expr, envir = mask, enclos = env)
#   }
#   ## compute this transformation for all subdfs
#   res <- lapply(sub_dfs, transformation_fun, arg)
#   ## simplify if all results are scalar
#   if(all(sapply(res, is.atomic) & lengths(res) == 1))
#     res <- unlist(res)
#   ## append the data
# }



# tb_spread <- function(data, nm, arg, sub_dfs, .X, env, .by){
#   matches <- gregexpr("\\{.*?\\}", nm, perl = T)
#   exprs <- regmatches(nm, matches)[[1]]  # curly braces content substrings
#   exprs <- gsub("[{}]","", exprs)
#   col_nms <- intersect(exprs, names(data)) # substrings that are also col names
#
#   transformation_fun0 <- function(sub_sub_df, expr){
#     mask <- c(as.list(sub_sub_df), list(
#       .sd = sub_sub_df,
#       .data = data,
#       .X = .X,
#       `?` = question_mark))
#     eval(expr, envir = mask, enclos = env)
#   }
#   transformation_fun <- function(sub_df, expr) {
#     ## convert to factor to keep groups with NAs
#     spread_cols_as_factors <- lapply(sub_df[,col_nms, drop=FALSE], function(x) factor(x, unique(x), exclude = NULL))
#     # spread_output
#     spread_output <- unique(sub_df[.by])
#     ## split data along spread columns
#     sub_sub_dfs <- split(sub_df, spread_cols_as_factors,drop = TRUE)
#     names(sub_sub_dfs) <- glue::glue_data(unique(sub_df[col_nms]), nm)
#
#     cols <- lapply(sub_sub_dfs, transformation_fun0, expr)
#     spread_output[names(cols)] <- cols
#     spread_output
#
#   }
#   res <- lapply(sub_dfs, transformation_fun, arg)
#   res <- as_tb(data.table::rbindlist(res,fill=TRUE))
#   spread_nms <- setdiff(names(res), .by)
#   res <- res[spread_nms]
# }
