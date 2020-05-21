#' Extract tag indicated by id from given item
#'
#' @export
tag <- function(item, id) {
    item[[id]]
}

#' Extract content of subf of tag indicated by id from given item
#'
#' @export
tag_subf <- function(item, id, subf) {
    index <- which(tag(item, id) == subf)
    if(!identical(index, integer(0))) {
        return(tag(item, id)[[index[1]+1]])
    }
}
