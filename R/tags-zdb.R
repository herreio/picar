#' PICA-basiertes Titelschema der ZDB im JSON-Format
#'
#' @export
zdb_schema <- function() {
    jsonlite::read_json("http://format.gbv.de/pica/zdb/zdbformat-title-schema.json")
}

#' Verlagsorte, Verlag, Verleger
#'
#' @param item Item data parsed from PicaPlus-XML
#' @export
zdb_place <- function(item) {
    if(!is.null(tag_subf__pp(item, "033A", "p"))) {
        tag_subf__pp(item, "033A", "p")
    } else {
        if(identical(item[["033A"]]$subf$id, "p")) {
            tag__pp(item, "033A")
        }
    }
}

#' Verlagsorte, Verlag, Verleger
#'
#' @param item Item data parsed from PicaPlus-XML
#' @export
zdb_publisher <- function(item) {
    tag_subf__pp(item, "033A", "n")
}

#' Frühere Verlagsorte und Verleger
#'
#' @param item Item data parsed from PicaPlus-XML
#' @export
zdb_publisher_prev <- function(item) {
    tag_subf__pp(item, "033B", "n")
}

#' Frühere Verlagsorte und Verleger
#'
#' @param item Item data parsed from PicaPlus-XML
#' @export
zdb_place_prev <- function(item) {
    tag_subf__pp(item, "033B", "p")
}

#' Bibliographische Gattung
#'
#' @param item Item data parsed from PicaPlus-XML
#' @export
zdb_bibtype <- function(item) {
  tag__pp(item, "002@")
}

#' ZDB-Nummer
#'
#' @param item Item data parsed from PicaPlus-XML
#' @export
zdb_number <- function(item) {
    tag__pp(item, "006Z")
}

#' Identifikationsnummer des Datensatzes (IDN)
#'
#' @param item Item data parsed from PicaPlus-XML
#' @export
zdb_idn <- function(item) {
    tag__pp(item, "003@")
}

#' Hauptsachtitel
#'
#' @param item Item data parsed from PicaPlus-XML
#' @export
zdb_maintitle <- function(item) {
    if (!is.null(tag__pp(item, "021A"))) {
      # nur Hauptsachtitel
      tag__pp(item, "021A")
    } else {
      # u.a. Hauptsachtitel (subfield)
      tag_subf__pp(item, "021A", "a")
    }
}

#' Erscheinungsverlauf
#'
#' @param item Item data parsed from PicaPlus-XML
#' @export
zdb_pubhistory <- function(item) {
    tag__pp(item, "031@")
}

#' maschinell verarbeitbarer Erscheinungsverlauf
#'
#' @param item Item data parsed from PicaPlus-XML
#' @export
zdb_pubhistory_strict <- function(item) {
    # 
    year <- function(x) {
        # lösche Zeichen vor und nach vier aufeinanderfolgende Zahlen (YYYY)
        gsub( ".*(\\d{4}).*", "\\1", x)
    }
    # zwei Eintraege in Unterfeldern: Erst- und Letzerscheinungsjahr
    if (!is.null(tag_subf__pp(item, "031N", "j")) & !is.null(tag_subf__pp(item, "031N", "k"))) {
        return(c(year(tag_subf__pp(item, "031N", "j")), year(tag_subf__pp(item, "031N", "k"))))
    }
    # ein Eintrag in einem Unterfeld: nur Ersterscheinungsjahr
    if (!is.null(tag_subf__pp(item, "031N", "j")) & is.null(tag_subf__pp(item, "031N", "k"))) {
        return(c(year(tag_subf__pp(item, "031N", "j")), year(tag_subf__pp(item, "031N", "j"))))
    }
    # ein Eintrag in einem Unterfeld: nur Letzterscheinungsjahr
    if (is.null(tag_subf__pp(item, "031N", "j")) & !is.null(tag_subf__pp(item, "031N", "k"))) {
        return(c(year(tag_subf__pp(item, "031N", "k")), year(tag_subf__pp(item, "031N", "k"))))
    }
    # ein Eintrag im Hauptfeld: ein Erscheinungsjahr
    if (!is.null(tag__pp(item, "031N"))) {
        return(c(year(tag__pp(item, "031N")), year(tag__pp(item, "031N"))))
    }
    # kein Eintrag im Feld --> stattdessen „normaler“ Erscheinungsverlauf
    if (!is.null(tag__pp(item, "031@"))) {
        return(c(year(tag__pp(item, "031@")), year(tag__pp(item, "031@"))))
    }
    cat("no publication date found!\n")
    cat(paste("item:", zdb_idn(item), "\n"))
    cat(paste("bibliographic identifiers:", zdb_bibtype(item), "\n"))
    cat(paste(item[["031N"]]))
    cat("\n")
    return(NULL)
}

#' Einträge mit angegebener bibliographischer Gattung (bg)
#'
#' @param item Item data parsed from PicaPlus-XML
#' @param bg Bibliographic type of item to be filtered
#' @export
zdb_filter_bibtype <- function(items, bg) {
    items[ sapply( items, function(x) { grepl(bg, zdb_bibtype(x)) } ) ]
}
