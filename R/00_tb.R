
#' Create tb object
#'
#' `tb()` works a lot like `data.frame()`, but allows for `list` or `data.frame`
#' columns, and outputs a data frame with the additional class `tb`
#' @export
tb <- function(...){
  dots <- eval(substitute(alist(...)))
  # dots <- lapply(dots, function(x) {
  #   if(is.numeric(x) || is.character(x)) return(list(x))
  #   if(has_splice_prefix(x)){
  #     return(eval.parent(x[[2]]))
  #   }
  #   x
  # })
  # dots <- unlist(dots, recursive = FALSE))

  ## deal with names
  dots_chr <- as.character(dots)
  x <- list(...)
  empty_nms_lgl <- allNames(x) == ""
  names(x)[empty_nms_lgl] <- dots_chr[empty_nms_lgl]
  ## recycle or fail
  lengths_ <- vapply(x, NROW, integer(1))
  n <- max(lengths_)
  if (!all(lengths_ %in% c(1, n)))
    stop("columns must have consistent lengths, only values of length one are recycled")
  for(i in which(lengths_ != n)){
    if (is.data.frame(x[[i]])) {
      x[[i]] <- do.call("rbind",replicate(n, x[[i]], FALSE))
    } else if (is.list(x)){
      x[[i]] <- do.call("c",replicate(n, x[[i]], FALSE))
    } else {
      x[[i]] <-rep(x[[i]],n)
    }
  }
  ## create object
  structure(x, class=c("tb","data.frame"), row.names = c(NA, - n))
}

#' @export
as_tb <- function(x){
  if(!is.data.frame(x))
    stop("`x` must be a data frame, apply as.data.frame on the object first if necessary")
  class(x) <- c("tb", "data.frame")
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

# it will do for now!
print.tb <- function(x, n = 10) {
  cat("# A tb:", nrow(x), "x", ncol(x), "\n")
  nr <- nrow(x)
  n_min <- min(nr, n)
  converter <- function(x) {
    sapply(x, function(elt){
    if(is.data.frame(elt)) {
      sprintf("[%s x %s]", nrow(elt), ncol(elt))
    } else elt
    })
  }

  x_edited <- x
  is_list_lgl <- sapply(x, is.list)
  x_edited[is_list_lgl] <- lapply(x[is_list_lgl], converter)
  #x_edited <- x[is.list(~~.data) := converter(~~.)]

  print.data.frame(x_edited[seq_len(n_min),])
  if(nr > n_min) cat("# ... with ", nr-n_min, " more rows")
  invisible(x)
}
