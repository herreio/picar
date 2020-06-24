#' ...
#'
#' @param item Item data parsed from picaxml
#' @export
vd18_nr <- function(item) {
  # VD-18 Nummer
  gsub("VD18 ", "", tag_subf(item, "006M", "0"), fixed=TRUE)
}
