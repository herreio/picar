#' VD-17 Nummer
#'
#' @param item Item data parsed from picaxml
#' @export
vd17_number <- function(item) {
  gsub("VD17 ","", tag_subf(item, "006W", "0", collapse=TRUE), fixed=TRUE)
}
#' VD-18 Nummer
#'
#' @param item Item data parsed from picaxml
#' @export
vd18_number <- function(item) {
  gsub("VD18 ", "", tag_subf(item, "006M", "0", collapse=TRUE), fixed=TRUE)
}
