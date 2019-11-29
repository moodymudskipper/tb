# # Note : the mask should be an environment, not a list
# # it should be defined in `[` and passed as argument (it has ref semantics!)
# # then should be modified adhoc with use cases
#
# aggregate_tb <- function(data, dots, .by, env, .x){
#
#   ## convert to factor to keep groups with NAs
#   by_cols_as_factors <- lapply(data[,.by, drop=FALSE], function(x) factor(x, unique(x), exclude = NULL))
#
#   ## split data along by columns
#   sub_dfs <- split(data, by_cols_as_factors,drop=TRUE)
#
#   ## override data with unique values of by columns, we ll append it with aggregations
#   output <- unique(data[.by])
#
#   nms <- allNames(dots)
#   for(i in seq_along(dots)){
#     nm <- nms[[i]]
#     element_is_named <- nm != ""
#
#     if(element_is_named){
#       if (is_glue_name(nm)){
#         #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#         ## spread
#         res <- tb_spread(data, nm, dots[[i]], sub_dfs, .x, env, .by)
#         output[names(res)] <- res
#       } else {
#         #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#         ## Standard case
#         output[[nm]] <- tb_standard_summarize(
#           data, nm, dots[[i]], sub_dfs, .x, env, .by)
#       }
#     } else if (is_labelled(dots[[i]])){
#       #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#       ## Handle labelled arguments
#       arg <- dots[[i]][[3]]
#       nm <- dots[[i]][[2]]
#       if(is.symbol(nm)){
#         nm <- deparse(nm)
#       } else {
#
#         # TODO the following should use the mask
#         nm <- eval(nm, envir = env)
#         if(is.numeric(nm) || is.logical(nm)){
#           nm <- names(data)[nm]
#         } else if (inherits(nm, "tb_selection")){
#           # TODO
#           nm <- make_it_a_character()
#         }
#       }
#       if(is.character(nm)) {
#         if (length(nm == 1)){
#
#           if (is_glue_name(nm)){
#             #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#             ## spread
#             res <- tb_spread(data, nm, arg, sub_dfs, .x, env, .by)
#             output[names(res)] <- res
#           } else {
#             #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#             ## Standard case
#             output[[nm]] <- tb_standard_summarize(
#               data, nm, arg, sub_dfs, .x, env, .by)
#           }
#         } else {
#           #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#           ## Handle character lhs of length > 1
#           if("." %in% all.vars(arg)){
#             ## long lhs with expression on lhs
#
#           } else {
#             eval(arg, env)
#           }
#
#
#         }
#       }  else {
#         stop("the lhs `", deparse2(arg), "` evaluates to an unsupported type")
#       }
#     } else {
#     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     ## unnamed arguments are not supported
#     stop("All arguments should be named (using `=`), or labelled (using `:=`)")
#     }
#   }
#   output
# }
#
#
# # if(is_unique_and_unnamed(dots)){
# #   # deal with unnamed dots : maybe it should just fail and this should be done
# #   # in tb[[...]] ?
# #   # On the other hand splicing must be handled and is done with unnamed args!
# #   # or maybe we should always splice unnamed args ????
# #   stop("unnamed arguments are not supported! when they will be, unnamed arguments will be spliced AFTER COMPUTATION")
# #   # if(is_function_symbol_or_formula(dots[[1]])) {
# #   #   f <- as_function2(dots[[1]])
# #   #   data <- summarize_all2(data, f , .by)
# #   #   class(data) <- class_bkp
# #   # } else {
# #   #   fun <- function(sub_df, expr, env) {
# #   #     eval(expr, envir = build_mask(sub_df), enclos = env)
# #   #   }
# #   #   #browser()
# #   #   sub_dfs <- split(data, lapply(data[.by], function(x) factor(x, unique(x), exclude = NULL)))
# #   #   data <- sapply(sub_dfs, fun, dots[[1]], env)
# #   # }
# #   # return(data)
# # }
#
#
# tb_spread <- function(data, nm, arg, sub_dfs, .x, env, .by){
#   matches <- gregexpr("\\{.*?\\}", nm, perl = T)
#   exprs <- regmatches(nm, matches)[[1]]  # curly braces content substrings
#   exprs <- gsub("[{}]","", exprs)
#   col_nms <- intersect(exprs, names(data)) # substrings that are also col names
#
#   transformation_fun0 <- function(sub_sub_df, expr){
#     mask <- c(as.list(sub_sub_df), list(
#       .sd = sub_sub_df,
#       .data = data,
#       .x = .x,
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
#
# tb_standard_summarize <- function(data, nm, arg, sub_dfs, .x, env, .by){
#   ## define transformation_fun which applies the transformation given by
#   ## a dot expression through the mask
#   transformation_fun <- function(sub_df, expr) {
#     mask <- c(as.list(sub_df), list(
#       .sd = sub_df,
#       .data = data,
#       . = sub_df[[nm]],
#       .nm = nm,
#       .x = .x,
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
