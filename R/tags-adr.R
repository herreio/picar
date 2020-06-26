#' Normierter Verleger / Drucker (Alte Drucke)
#'
#' @param item Item data parsed from picaxml
#' @param collapse Whether to collapse multiple values
#' @export
adr_printer <- function(item, collapse=FALSE) {
    if (!is.null(tag_subf(item, "033J", "d")) & !is.null(tag_subf(item, "033J", "a"))) {
        printer_name <- paste(tag_subf(item, "033J", "d", preserve=TRUE), tag_subf(item, "033J", "a", preserve=TRUE))
        if(collapse) {
            return(collapse_values(printer_name))
        }
        return(printer_name)
    }
}

#' Normierter Verleger / Drucker GND (Alte Drucke)
#'
#' @param item Item data parsed from picaxml
#' @param preserve Whether to preserve empty values
#' @param collapse Whether to collapse multiple values
#' @export
adr_printer_gnd <- function(item, preserve=FALSE, collapse=FALSE) {
  tag_subf(item, "033J", "0", preserve=preserve, collapse=collapse)
}
