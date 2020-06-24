#' VD-18 Nummer
#'
#' @param item Item data parsed from picaxml
#' @export
vd18_number <- function(item) {
  gsub("VD18 ", "", tag_subf(item, "006M", "0"), fixed=TRUE)
}
