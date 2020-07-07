#' Bibliographische Gattung
#'
#' @param item Item data parsed from picaxml
#' @export
gbv_bibtype <- function(item) {
  tag_subf(item, "002@", "0")
}

#' Identifikationsnummer des Datensatzes (IDN)
#'
#' @param item Item data parsed from picaxml
#' @export
gbv_idn <- function(item) {
  tag_subf(item, "003@", "0")
}

#' Elektronische Adresse und ergaenzende Angaben zum Zugriff (URL)
#'
#' @param item Item data parsed from picaxml
#' @export
gbv_url <- function(item, preserve=FALSE, collapse=FALSE) {
  tag_subf(item, "009P", "a", preserve=preserve, collapse=collapse)
}

#' Haupttitel, Titelzusatz, Verantwortlichkeitsangabe
#'
#' @param item Item data parsed from picaxml
#' @export
gbv_maintitle <- function(item, preserve=FALSE, collapse=FALSE) {
  gsub("@", "", tag_subf(item, "021A", "a", preserve=preserve, collapse=collapse), fixed=TRUE)
}

#' Veroeffentlichungsangabe / Erscheinungsort
#'
#' @param item Item data parsed from picaxml
#' @param preserve Whether to preserve empty values
#' @param collapse Whether to collapse multiple values
#' @export
gbv_place <- function(item, preserve=FALSE, collapse=FALSE) {
  tag_subf(item, "033A", "p", preserve=preserve, collapse=collapse)
}

#' Veroeffentlichungsangabe / Verlagsname
#'
#' @param item Item data parsed from picaxml
#' @param preserve Whether to preserve empty values
#' @param collapse Whether to collapse multiple values
#' @export
gbv_publisher <- function(item, preserve=FALSE, collapse=FALSE) {
  tag_subf(item, "033A", "n", preserve=preserve, collapse=collapse)
}

#' Erscheinungsdatum/Entstehungsdatum
#'
#' @param item Item data parsed from picaxml
#' @export
gbv_year <- function(item) {
  # Jahr (Anfang) in Sortierform
  tag_subf(item, "011@", "a")
  # Datum in Vorlageform
  # tag_subf(item, "011@", "n")
}

#' Veroeffentlichungsangabe / Erscheinungsjahr in Originalschrift
#'
#' @param item Item data parsed from picaxml
#' @export
gbv_years <- function(item) {
  tag_subf(item, "033A", "d")
}

#' Format, Maße
#'
#' @param item Item data parsed from picaxml
#' @export
gbv_description <- function(item) {
  tag_subf(item, "034I", "a")
}

#' Umfang
#'
#' @param item Item data parsed from picaxml
#' @export
gbv_extent <- function(item) {
  tag_subf(item, "034D", "a")
}

#' Einträge mit angegebener bibliographischer Gattung (bg)
#'
#' @param items Item data parsed from picaxml
#' @param bg Bibliographic type of item to be filtered
#' @export
gbv_filter_bibtype <- function(items, bg) {
  items[ sapply( items, function(x) { grepl(bg, gbv_bibtype(x)) } ) ]
}
