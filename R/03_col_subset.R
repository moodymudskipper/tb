col_subset_by_ref <- function(.j, mask, .by){
  if(missing(.j)) return(invisible())
  if(missing(.by)) .by <- NULL
  .data <- mask$.data
  if(is.call(.j)){
    ## evaluate .j call in the context of df and mask
    .j <- eval(.j, envir=.data, enclos= mask)
    if(inherits(.j, "tb_selection")) {
      #data  <- tb_transmute(data, .j, env = caller_env)
      .j <- modify_by_ref_and_return_selected_names(.j, mask)
    } #return(.j)
  } else {
    ## evaluate .j symbol in the context of df and mask
    .j <- eval(.j, envir=.data, enclos= mask)
  }

  if(is.logical(.j)) {
    ## turn NAs to FALSE so we keep only TRUE indices
    .j[is.na(.j)] <- FALSE
  }
  if(!is.character(.j)) {
    ## convert indices to character col names
    .j <- names(.data)[.j]
  }
  ## return union of .by and .j columns
  .j <- unique(c(.by, .j))
  mask$.data <- subset_j(.data, .j)
  invisible()
}


#
# simplify_j <- function(.X, .j, .by, env = parent.frame()){
#   if(is.call(.j)){
#     ## detect if j is of form var1:var2
#     j_is_col_sequence <-
#       identical(.j[[1]], quote(`:`)) &&
#       is.symbol(.j[[2]]) &&
#       is.symbol(.j[[3]])
#     if(j_is_col_sequence){
#       ## replace .j call by a numeric sequence
#       nms <- names(.X)
#       .j1 <- match(as.character(.j[[2]]), nms)
#       .j2 <- match(as.character(.j[[3]]), nms)
#       .j <- .j1:.j2
#     } else {
#       ## evaluate .j call in the context of df and mask
#       .j <- eval(.j, envir=c(as.list(.X), list(.X = .X, `?` = question_mark)), enclos= env)
#       if(inherits(.j, "tb_selection")) return(.j)
#     }
#   } else {
#     ## evaluate .j symbol in the context of df and mask
#     .j <- eval(.j, envir=c(as.list(.X), list(.X = .X, `?` = question_mark)), enclos= env)
#   }
#
#   if(is.logical(.j)) {
#     ## turn NAs to FALSE so we keep only TRUE indices
#     .j[is.na(.j)] <- FALSE
#   }
#   if(!is.character(.j)) {
#     ## convert indices to character col names
#     .j <- names(.X)[.j]
#   }
#   ## return union of .by and .j columns
#   .j <- unique(c(.by, .j))
# }
