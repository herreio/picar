#' Normierter Verleger / Drucker (Alte Drucke)
#'
#' @param item Item data parsed from picaxml
#' @export
adr_printer <- function(item) {
  # 
  paste(tag_subf(item, "033J", "d"),
    tag_subf(item, "033J", "a"))
}

#' Normierter Verleger / Drucker (Alte Drucke)
#'
#' @param item Item data parsed from picaxml
#' @export
printer_gnd <- function(item) {
  # Normierter Verleger / Drucker (Alte Drucke)
  tag_subf(item, "033J", "0")
}
