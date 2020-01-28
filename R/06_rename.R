rename_by_ref <- function(lhs, expr, mask){
  .data <- mask$.data
  nms <- names(.data)
  lhs <- lhs[[2]]

  if(is.symbol(lhs)) {
    lhs0 <- as.character(lhs)
    if(lhs0 %in% nms) {
      old_nms <- lhs0
    } else {
      old_nms <- eval(lhs, envir = .data, enclos = mask)
      if(!all(old_nms %in% nms)) {
        stop("Tried to rename an inexistent column")
      }
    }
  } else {
    old_nms <- eval(lhs, envir = .data, enclos = mask)
    if(inherits(old_nms, "tb_selection")) {
      old_nms <- modify_by_ref_and_return_selected_names(old_nms, mask)
    }
    if(is.logical(old_nms) || is.numeric(old_nms)) {
      old_nms <- nms[old_nms]
    }
    if(!all(old_nms %in% nms)) {
      stop("Tried to rename an inexistent column")
    }
  }

  if("." %in% all.vars(expr)){
    rename_fun <- as.function(c(alist(.=),expr))
    new_nms <- vapply(old_nms, rename_fun,character(1))
  } else {
    new_nms <- eval(expr, envir = .data, enclos = mask)
  }

  if(length(old_nms) != length(new_nms)) {
    stop("Number of old names different from number of new names")
  }
  names(mask$.data)[nms %in% old_nms] <- new_nms
  invisible()
}
