
#' Create tb object
#'
#' `tb()` works a lot like `data.frame()`
#' @export
tb <- function(...){
  # we don't wrap data.frame because it's not nice with list or data frame columns
  x <- list(...)
  structure(x, class=c("tb","data.frame"), row.names = c(NA, - max(lengths(x))))
}

#' @export
as_tb <- function(x){
  x <- as.data.frame(x)
  class(x) <- c("tb", class(x))
  x
}

#' @export
`%tb>%` <- function(lhs ,rhs) {
  if(!is.data.frame(lhs))
    stop("lhs must be a data frame")
  rhs <- substitute(rhs)
  # make sure call has the right format
  if(substr(deparse(rhs)[1],1,2) != ".[")
    stop("rhs should be of the form `.[i, j, ...]`")
  # backup class and check for grouped_df and rowwise_df classes
  class_ <- backup_class(lhs)
  # evaluate the call and trigger error if withDT.lock
  # is TRUE and syntax of assignment by ref is used
  res <- eval(rhs, envir = list(. = as_tb(lhs)), enclos = parent.frame())
  # set back original class and remove ".internal.selfref" attribute
  class(res) <- class_
  res
}

#' Subset `tb` objects or pipe `[.tb` calls
#'
#' `some_tb$.` simply returns `some_tb`, which makes piping easy. Piping with
#' `magrittr`'s `%>%` operator is possible as well but requires another package,
#' is less efficient, more verbose, and has less convenient precedence.
#'
#' @export
`$.tb` <- function(e1,e2){
  if(substitute(e2) == quote(.)) return(e1)
  NextMethod()
}


backup_class <- function(x){
  class_ <- class(x)
  if("grouped_df" %in% class_){
    warning("grouped_df class and groups attribute were removed")
    class_ <- setdiff(class_, "grouped_df")
  }
  if("rowwise_df" %in% class_){
    warning("rowwise_df class was removed")
    class_ <- setdiff(class_, "rowwise_df")
  }
  class_
}

#withDT:::restore_attr

# it will do for now!
print.tb <- function(x) {
  cat("# A tb:", nrow(x), "x", ncol(x), "\n")
  n <- nrow(x)
  print.data.frame(head(x,10))
  if(n > 10) cat("# ... with ", n-10," more rows")
  invisible(x)
}
