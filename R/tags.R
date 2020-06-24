#' Extract tag indicated by id from given item
#'
#' @param item Item data parsed from picaxml
#' @param id Identifier of tag to be extracted
#' @export
tag <- function(item, id) {
    item[[id]]
}

#' Extract content of subf of tag indicated by id from given item
#'
#' @param item Item data parsed from picaxml
#' @param id Identifier of tag to be extracted
#' @param subf Subfield identifier of tag to be extracted
#' @export
tag_subf <- function(item, id, subf) {
    index <- which(tag(item, id) == subf)
    if(!identical(index, integer(0))) {
        return(tag(item, id)[[index[1]+1]])
    }
}

#' Extract tag indicated by id from given item
#'
#' @param item Item data parsed from PicaPlus-XML
#' @param id Identifier of tag to be extracted
#' @export
tag__pp <- function(item, id) {
    item[[id]]$subf$content
}

#' Extract content of subf of tag indicated by id from given item
#'
#' @param item Item data parsed from PicaPlus-XML
#' @param id Identifier of tag to be extracted
#' @param subf Subfield identifier of tag to be extracted
#' @export
tag_subf__pp <- function(item, id, subf) {
    item[[id]]$subf[[subf]]$content
}
