#' Extract tag indicated by id from given item
#'
#' @param item Item data parsed as picaxml or ppxml
#' @param id Identifier of tag to be extracted
#' @export
tag <- function(item, id) {
    index <- which(names(item) == id)
    if(!identical(index, integer(0))) {
      return(item[index])
    }
}

#' Extract content of subf of tag indicated by id from given item
#'
#' @param item Item data parsed as picaxml or ppxml
#' @param id Identifier of tag to be extracted
#' @param subf Subfield identifier of tag to be extracted
#' @param preserve Whether to preserve empty and duplicate values
#' @param collapse Whether to collapse multiple values
#' @export
tag_subf <- function(item, id, subf, preserve=FALSE, collapse=FALSE) {
  parent <- tag(item, id)
  if(!is.null(parent)) {
    index <- sapply(parent, function(x) which(x==subf), simplify=FALSE)
    if (!all(sapply(index, function(x) identical(x, integer(0))))) {
      result <- sapply(parent, function(x) x[which(x==subf)+1], simplify=FALSE)
      result[sapply(result, function(x) identical(x, character(0)))] <- ""
      if(any(sapply(result, length) != 1)) {
          if(length(result) == 1) {
              result <- collapse_values(unlist(unname(result)), collapse="||")
          }
          if(length(result) > 1) {
            result <- sapply(result, function(x) {
                collapse_values(unlist(unname(x)), collapse="||")
            })
          }
      }
      if(!preserve) {
        result <- result[result!=""]
        if(length(result) < 1) {
          return(NULL)
        }
        result <- unique(unlist(result))
      }
      if(collapse) {
        return(collapse_values(result, collapse="|||"))
      }
      return(unlist(unname(result)))
    }
  }
}

collapse_values <- function(values, collapse="|||") {
    paste(values, collapse=collapse, sep="") 
}
