# evaluate i in the context of the data frame, handle by if given
row_subset_by_ref <- function(i, mask){
  if(missing(i)) return(invisible())

  ## evaluate in  mask
  i <- eval(i, envir=mask$.data, enclos= mask)


  if(inherits(i, "formula")) {
    one_sided_formula_lgl <- length(i) == 2
    if(one_sided_formula_lgl) {
      ## use regex on rows
      # maybe we should unfeature this
      i <- grepl(eval(i[[2]], envir=mask$.data, enclos= mask), rownames(mask$.data))
      mask$.data <- subset_i(mask$.data,i)
    } else {
      ## extract variables from rhs
      along_vars <- get_all_vars(i[-2], mask$.data)
      i <- i[[2]]
      ## evaluate lhs by group
      g <- do.call(interaction, along_vars)
      split_x <- split(as.data.frame(mask$.data), g)
      i <- unlist(lapply(split_x, function(chunk) {
        #browser()
        i <- eval(i, envir = chunk, enclos = mask)
        if(is.logical(i)) {
          i[is.na(i)] <- FALSE
        } else if (is.numeric(i)) {
          i <-  seq_len(dim(chunk)[[1]]) %in% i
        } else {
          stop("The lhs of i should evaluate to numeric or logical.")
        }
        i
      }))
      mask$.data <- subset_i(mask$.data,i)
    }
  } else {
    if(is.logical(i)) {
      ## turn NAs to FALSE so we keep only TRUE indices
      i[is.na(i)] <- FALSE
      mask$.data <- subset_i(mask$.data,i)
    } else if (is.data.frame(i)) {
      ## get the indices by doing a semi join
      class(i) <- "data.frame"
      nms <- intersect(names(mask$.data), names(i))
      i <- i[,nms]
      # ## add a temp column
      mask$.data <-merge(mask$.data, i)
    } else if (is.numeric(i) || is.character(i)) {
      mask$.data <- subset_i(mask$.data,i)
    }
  }
  invisible()
}
