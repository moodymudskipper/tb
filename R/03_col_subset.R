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
