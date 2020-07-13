#' VD-17 Nummer
#'
#' @param item Item data parsed from picaxml
#' @export
vd17_number <- function(item) {
  tag_subf(item, "006W", "0")
}
#' VD-18 Nummer
#'
#' @param item Item data parsed from picaxml
#' @export
vd18_number <- function(item) {
  gsub("VD18 ", "", tag_subf(item, "006M", "0"), fixed=TRUE)
}
