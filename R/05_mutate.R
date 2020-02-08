mutate_dots_by_ref <- function(dots, mask) {
  for (i in seq_along(dots)) {
    ## setup loop
    expr <- dots[[i]]
    if (is_labelled(expr)) {
      mutate_labelled_by_ref(expr, mask)
    } else {
      mutate_named_by_ref(expr, nm = names(dots)[i], mask)
    }
  }
  invisible()
}

mutate_labelled_by_ref <- function(expr, mask) {
  to_remove <- NULL
  on.exit(mask$.data[to_remove] <- NULL)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # labelled dot expressions

  lhs <- expr[[2]]
  expr <- expr[[3]]
  lhs <- reparse_dbl_tilde(lhs)
  expr <- reparse_dbl_tilde(expr)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Renaming
  if (is_curly_expr(lhs)) {
    rename_by_ref(lhs, expr, mask)
    return(invisible())
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Mutating

  if (is_parenthesized_twice(lhs)) {
    ## adapt lhs and setup variables to delete at end of loop
    lhs <- lhs[[2]][[2]]
    data_nms <- names(mask$.data)
    to_remove <- intersect(all.vars(expr), data_nms)
  }

  if (is.symbol(lhs)) {
    nm <- as.character(lhs)
  } else {
    nm <- eval(lhs, envir = mask$.data, enclos = mask)
    if (is.logical(nm)) {
      nm <- names(mask$.data)[nm]
    }
    if (inherits(nm, "tb_selection")) {
      nm <- modify_by_ref_and_return_selected_names(nm, mask)
    }
  }

  if (length(nm) == 1) {
    mutate_named_by_ref(expr, nm, mask)
    return(invisible())
  }

  if ("." %in% all.vars(expr)) {
    mask$.data[nm] <- lapply(nm, transform2, expr, mask)
  } else {
    mask$.data[nm] <- eval(expr, envir = mask$.data, enclos = mask)
  }

  invisible()
}

mutate_named_by_ref <- function(expr, nm, mask) {
  to_remove <- NULL
  expr <- reparse_dbl_tilde(expr)
  #~~~~~~~~~~
  # =
  if (is_glue_name(nm)) {
    stop("A `by` argument argument is required to spread/cast/pivot_wider")
  } else {
    ## regular column name
    # we should handle "along" notation in there too
    mask$.data[nm] <- list(transform2(nm, expr, mask))
    mask$.data[to_remove] <- NULL
  }
  invisible()
}
