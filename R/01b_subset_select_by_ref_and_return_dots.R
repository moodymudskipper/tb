subset_select_by_ref_and_return_dots <- function(dots, .by, sc, pf, mask){
  # expand dot expressions
  dots <- lapply(dots, expand_expr, pf)
  dots0 <- list() # will contain if relevant, the labelled expr passed to .i and .j

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## reorganize call if labelled args are fed to .i or .j, and splice/expand on the way


  if(is_specified(sc[3])){
    # if 1st bracket arg is specified, .i and .j can only be missing and we sort out the call
    other_args <- sc[-(1:3)]
    .i <- substitute()
    .j <- substitute()
    any_other_arg_is_unspecified <-
      any(!vapply(split(other_args, seq_along(other_args)), is_specified, logical(1)))
    if(any_other_arg_is_unspecified)
      stop(".i must be given first, left blank, or omitted, but cannot be given after another specified argument")

    # if .i and/or .j are labelled, we must take their content and add it to the dots
    if(is_labelled(sc[[3]])) {
      dots0[[1]] <- expand_expr(sc[[3]], pf)
    }
    if(length(sc) > 3 && is_labelled(sc[[4]])) {
      dots0 <- c(dots0, expand_expr(sc[[4]], pf))
    }
  } else {
    # if 1st bracket arg is NOT specified, it's a proper .i, possibly empty
    .i <- sc[[3]]
    if(missing(.i)) {
      .i <- substitute()
    } else {
      .i <-expand_expr(sc[[3]], pf)
      .i <- splice_expr(.i, mask)
    }

    # if 2nd bracket arg is specified, .j can only be missing and we sort out the call
    if(length(sc) >3) {
      if(is_specified(sc[4])){
        .j <- substitute()
        other_args <- sc[-(1:4)]
        any_other_arg_is_unspecified <- any (!vapply(split(other_args,seq_along(other_args)), is_specified, logical(1)))
        if(any_other_arg_is_unspecified)
          stop(".j must be given second, left blank, or omitted, but cannot be given after another specified argument")
        if(is_labelled(sc[[4]])) {
          dots0[[1]] <- expand_expr(sc[[4]], pf)
        }
      } else {
        # if 2nd bracket arg is NOT specified, it's a proper .j, either missing or not
        .j <- sc[[4]]
        if(missing(.j)) {
          .j <- substitute()
        } else {
          .j <-expand_expr(.j, pf)
          .j <- splice_expr(.j, mask)
        }
      }
    }
  }
  dots <- c(dots0, dots)

  ## splice dots
  dots <- lapply(dots, function(x) {
    if(is.numeric(x) || is.character(x)) return(list(x))
    if(has_splice_prefix(x)){
      return(eval(x[[2]], envir = mask$.data, enclos = mask))
    }
    x
  })
  dots <- unlist(dots, recursive = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # subset
  row_subset_by_ref(.i, mask)
  col_subset_by_ref(.j, mask, .by)
  dots
}
