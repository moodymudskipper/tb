fill_by_ref <- function(fill, mask) {
  if (!is.null(fill)) {
    if (is.list(fill)) {
      mask$.data[names(fill)] <- Map(function(x, y)
        replace(x, is.na(x), y), mask$.data[names(fill)], fill)
    } else {
      mask$.data[is.na(mask$.data)] <- fill
    }
  }
  invisible()
}

splt <- function(x, into, sep = "[^[:alnum:]]+",
                 convert = FALSE, extra = "warn", fill = "warn") {
  x <- as.character(x)
  x2 <- strsplit(x, sep, perl = TRUE)
  if (missing(into)) into <- paste0("V", seq_len(max(lengths(x2))))
  n_into <- length(into)
  x2 <- lapply(x2, function(elt) {
    if (length(elt) > n_into) {
      elt <- switch(
        extra,
        warn = {
          warning("extra elts are ignored")
          elt[seq_len(n_into)]
        },
        drop = elt[seq_len(n_into)],
        merge = c(elt[seq_len(n_into - 1)],
                  paste(elt[n_into:length(elt)], collapse = ""))
      )
    } else if (length(elt) < n_into) {
      elt <- switch(
        fill,
        warn = {
          warning("adding NAs")
          c(elt, rep(NA, n_into - length(elt)))
        },
        right = c(elt, rep(NA, n_into - length(elt))),
        left = c(rep(NA, n_into - length(elt)), elt))
    }
    names(elt) <- into
    elt
  })
  res <- as.data.frame(do.call("rbind", x2))
  if (convert) res <- type.convert(res, asis = TRUE)
  res
}

# a faster and convenient way to do `[.data.frame(x,i,,drop=FALSE)`
# it also doesn't upset Rstudio as it has no consecutive comas
subset_i <- function(x, i) {
  if (is.character(i)) i <- row.names(x) %in% i
  subset_col <-
    function(col) {
      if (is.data.frame(col))
        subset_i(col, i)
      else
        col[i]
    }
  res <- lapply(x, subset_col)
  attributes(res) <- attributes(x)
  attr(res, "row.names") <- attr(x, "row.names")[i]
  res
}

# a faster and more compact way to do `[.data.frame(x,j)`
subset_j <- function(x, i) {
  attr_ <- attributes(x)
  x <- unclass(x)[i]
  attr(x, "class") <- attr_$class
  attr(x, "names") <- if (is.character(i)) i else  attr_$names[i]
  attr(x, "row.names") <- attr_$row.names
  x
}

expand_expr <- function(expr, where) {
  if (identical(expr, substitute())) return(substitute())
  # taken right from bquote's code
  unquote <- function(e) if (is.pairlist(e))
    as.pairlist(lapply(e, unquote))
  else if (length(e) <= 1L)
    e
  else if (e[[1L]] == as.name("."))
    eval(e[[2L]], where)
  else as.call(lapply(e, unquote))
  expr <- unquote(expr)
  # change precedence if `?` is used
  if (is_qm_labelled(expr)) {
    expr <- expr[[2]]
    expr[[2]] <- as.call(c(quote(`?`), expr[[2]]))
  }
  expr
}

reparse_dbl_tilde <- function(expr) {
  ## counter of arguments to iterate on
  i <- 0
  ## content of arguments to iterate on
  all_iter_args <- list()

  is_prefixed <- function(x) {
    is.call(x) &&
      identical(x[[1]], quote(`~`)) &&
      is.call(x[[2]]) &&
      identical(x[[2]][[1]], quote(`~`))
  }

  reparse0 <- function(call) {
    if (!is.call(call)) return(call)
    prefixed_lgl <- sapply(call, is_prefixed)
    if (any(prefixed_lgl)) {
      iter_args <-  lapply(call[prefixed_lgl], `[[`, c(2, 2))
      n_iter_args    <-  length(iter_args)
      all_iter_args      <<- append(all_iter_args, iter_args)
      arg_nms   <-  paste0("*", seq(i + 1, i <<- i + n_iter_args))
      arg_syms  <-  lapply(arg_nms, as.symbol)
      call[prefixed_lgl] <- arg_syms
    }
    call[] <- lapply(call, reparse0)
    call
  }
  body <- reparse0(expr)

  if (!i) return(expr)
  arg_nms   <-  paste0("*", seq_len(i))
  fun_iter_args <- setNames(replicate(i, substitute()), arg_nms)
  fun <- as.function(c(fun_iter_args, body), envir = parent.frame())
  as.call(c(quote(mapply2), fun, all_iter_args))
}

splice_expr <- function(expr, mask) {
  if (!is.call(expr)) return(expr)
  expr <- as.list(expr)
  expr <- lapply(expr, function(x) {
    if (is.symbol(x)) return(x)
    if (is.numeric(x) || is.character(x) || is.logical(x)) return(list(x))
    if (has_splice_prefix(x)) {
      return(eval(x[[2]], envir = mask$.data, enclos = mask))
    }
    splice_expr(x)
  })
  expr <- unlist(expr, recursive = FALSE)
  expr <- as.call(expr)
  expr
}

has_splice_prefix <- function(x) {
  is.call(x) && length(x) == 2 && identical(x[[1]], quote(`+`))
}

is_specified <- function(arg) {
  (!is.null(names(arg)) && names(arg) != "") || is_labelled(arg[[1]])
}

is_qm_labelled <- function(arg) {
  is.call(arg) &&  identical(arg[[1]], quote(`?`)) &&
    is.call(arg[[2]]) && identical(arg[[c(2, 1)]], quote(`:=`))
}


# a deparse that doesnt choke on `{`
deparse2 <- function(x) {
  paste(deparse(x), collapse = "")
}

transform2 <- function(nm, expr, mask) {
  .data <- mask$.data
  mask$. <- .data[[nm]]
  res <- eval(expr, envir = .data, enclos = mask)
  if (inherits(res, "formula")) {
    ## mutating along
    along <- res[[3]]
    if (is.symbol(along)) {
      along <- as.character(along)
      if (!along %in% names(mask$.data))
        stop(sprintf(paste0(
          "The column '%s' was not found, ",
          "if you meant to evaluate the variable '%s', ",
          "use '.(%s)' or `c(%s)`instead"),
          along, along, along, along))
    } else {
      along <- expand_expr(along, pf)
      along <- eval(along, envir = mask$.data, enclos = mask)
      if (isTRUE(is.na(along))) {
        along <- setdiff(names(mask$.data), unlist(lapply(dots, all.vars)))
      }
      if (inherits(along, "tb_selection")) {
        along <- modify_by_ref_and_return_selected_names(along, mask)
      }
    }
    along_df <- subset_j(.data, along)

    expr <- res[[2]]
    sub_dfs <- split(as.data.frame(.data), along_df)

    transformation_fun <- function(sub_df, expr) {
      mask$.subset <- as_tb(sub_df)
      mask$.  <- sub_df[[nm]]
      mask$.N <- nrow(sub_df)
      eval(expr, envir = sub_df, enclos = mask)
    }

    res <- rep(NA, nrow(.data))
    split(res, along_df) <- lapply(sub_dfs, transformation_fun, expr)
    mask$.       <- NULL
    mask$.subset <- NULL
    mask$.N      <- NULL
  }
  res
}

#' @export
mapply2 <- function(...) {
  res <- Map(...)
  if (
    all(lengths(res) == 1) &&
    all(vapply(res, function(x) !is.null(x) && is.atomic(x), logical(1)))
  ) {
    res <- unlist(res)
  }
  res
}

is_labelled <- function(x) {
  # expr should be a call
  # expr[[1]] should be `:=`
  is.call(x) && identical(x[[1]], quote(`:=`))
}

# should be done with double parens ((foo)) and not reserved to
is_parenthesized_twice <- function(expr) {
  # the lhs expr[[2]] should be a call
  is.call(expr) &&
    # expr[[2]][[1]] should be `(`
    identical(expr[[1]], quote(`(`)) &&
    is.call(expr[[2]]) &&
    identical(expr[[2]][[1]], quote(`(`))
}

is_curly_expr <- function(expr) {
  is.call(expr) && expr[[1]] == quote(`{`)
}


is_glue_name <- function(x) {
  grepl("\\{.*?\\}", x)
}

# keyval <- function(..., .key = "key", .value = "value", rm = TRUE) {
#   x <- tibble::tibble(...)
#   nms <- names(x)
#   x <- split(x, seq(nrow(x)))
#   x <- lapply(x, function(x) setNames(stack(x)[2:1],c(.key,.value)))
#   x
# }
