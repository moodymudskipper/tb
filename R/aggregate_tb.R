aggregate_tb <- function(.x, dots, .by, env){
  if(is_unique_and_unnamed(dots)){
    # deal with unnamed dots : maybe it should just fail and this should be done
    # in tb[[...]] ?
    # On the other hand splicing must be handled and is done with unnamed args!
    # or maybe we should always splice unnamed args ????
    stop("unnamed arguments are not supported! when they will be, unnamed arguments will be spliced AFTER COMPUTATION")
    # if(is_function_symbol_or_formula(dots[[1]])) {
    #   f <- as_function2(dots[[1]])
    #   .x <- summarize_all2(.x, f , .by)
    #   class(.x) <- class_bkp
    # } else {
    #   fun <- function(sub_df, expr, env) {
    #     eval(expr, envir = build_mask(sub_df), enclos = env)
    #   }
    #   #browser()
    #   sub_dfs <- split(.x, lapply(.x[.by], function(x) factor(x, unique(x), exclude = NULL)))
    #   .x <- sapply(sub_dfs, fun, dots[[1]], env)
    # }
    # return(.x)
  }

  # convert to factor to keep groups with NAs
  sub_dfs <- split(.x, lapply(.x[.by], function(x) factor(x, unique(x), exclude = NULL)))

  # override .x as all info is in sub_dfs
  .x <- unique(.x[.by])
  for(i in seq_along(dots)){
    # standard situation

    fun <- function(sub_df, expr, env) {
      eval(expr, envir = build_mask(sub_df), enclos = env)
    }
    #browser()
    res <- lapply(sub_dfs, fun, dots[[i]], env)
    if(all(sapply(res, is.atomic) & lengths(res) ==1))
      res <- unlist(res)
    .x[[names(dots)[i]]] <- res
    #new_vars <- c(new_vars,names(dots)[[i]])
  }
  .x
}
