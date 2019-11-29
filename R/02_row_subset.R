# evaluate .i in the context of the data frame, handle .by if given
row_subset_by_ref <- function(.i, mask){ #env = parent.frame()){
  ## evaluate in  mask
  .x <- mask$.x
  .i <- eval(.i, envir=.x, enclos= mask)
  if(inherits(.i, "formula")) {
    one_sided_formula_lgl <- length(.i) == 2
    if(one_sided_formula_lgl) {
      ## use regex on rows
      # maybe we should unfeature this
      .i <- grepl(eval(.i[[2]], envir=.x, enclos= mask), rownames(.x))
      mask$.data <- `[.data.frame`(.x, .i,)
    } else {
      ## extract variables from rhs
      along_vars <- get_all_vars(.i[-2], .x)
      .i <- .i[[2]]
      ## evaluate lhs by group
      g <- do.call(interaction, along_vars)
      split_x <- split(as.data.frame(.x), g)
      .i <- unlist(lapply(split_x, function(chunk) {
        #browser()
        .i <- eval(.i, envir = chunk, enclos = mask)
        if(is.logical(.i)) {
          .i[is.na(.i)] <- FALSE
        } else if (is.numeric(.i)) {
          .i <-  seq_len(dim(chunk)[[1]]) %in% .i
        } else {
          stop("The lhs of .i should evaluate to numeric or logical.")
        }
        .i
      }))
      mask$.data <- `[.data.frame`(.x, .i, )
    }
  } else {
    if(is.logical(.i)) {
      ## turn NAs to FALSE so we keep only TRUE indices
      .i[is.na(.i)] <- FALSE
      mask$.data <- `[.data.frame`(.x, .i,)
    } else if (is.data.frame(.i)) {
      ## get the indices by doing a semi join
      class(.i) <- "data.frame"
      nms <- intersect(names(.x), names(.i))
      .i <- .i[,nms]
      # ## add a temp column
      mask$.data <-merge(.x, .i)
    } else if (is.numeric(.i) || is.character(.i)) {
      mask$.data <- `[.data.frame`(.x, .i,)
    }
  }
  invisible()
}


#
#
# # evaluate .i in the context of the data frame, handle .by if given
# simplify_i <- function(.x, .i, env = parent.frame()){
#   ## evaluate it in the context of data frame and mask
#   .i <- eval(.i, envir=c(as.list(.x), list(.x = .x)), enclos= env)
#   if(inherits(.i, "formula")) {
#     one_sided_formula_lgl <- length(.i) == 2
#     if(one_sided_formula_lgl) {
#       ## use regex on rows
#       .i <- grepl(eval(.i[[2]], env), rownames(.x))
#     } else {
#       ## extract variables from rhs
#       along_vars <- get_all_vars(.i[-2], .x)
#       ## evaluate lhs
#       .i <- eval(.i[[2]], envir=.x, enclos= env)
#       if(!is.numeric(.i)) {
#         stop("if using `.by` and `.i` non missing, `.i` must be numeric")
#       }
#       ## build ave call to get indices by group
#       call <- as.call(c(
#         quote(ave),                         # ave(
#         list(x = seq(nrow(.x))),            #   seq(nrow(.x)),
#         along_vars,                         #   grpvar1, grpvar2, ...,
#         list(FUN = seq_along)))             #   Fun = seq_along)
#       ## evaluate it and filter these indices on given values of .i
#       .i <- eval(call, envir = .x) %in% .i
#     }
#   } else {
#     if(is.logical(.i)) {
#       ## turn NAs to FALSE so we keep only TRUE indices
#       .i[is.na(.i)] <- FALSE
#     } else if (is.data.frame(.i)) {
#       ## get the indices by doing a semi join
#       nms <- intersect(names(.x), names(.i))
#       .i <- .i[,nms]
#       .i$`*temp*` <- 1
#       .i <- !is.na(merge(.x[nms], .i, all.x = TRUE)$`*temp*`)
#     }
#   }
#   .i
# }
