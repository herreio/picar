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

#' Angaben zum Inhalt
#'
#' @param item Item data parsed from PicaPlus-XML
#' @export
zdb_contenttype <- function(item) {
  tag_subf__pp(item, "013D", "a")
}

#' Angaben zum Inhalt (GND-Nummer)
#'
#' @param item Item data parsed from PicaPlus-XML
#' @export
zdb_contenttype_gnd <- function(item) {
  tag_subf__pp(item, "013D", "0")
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
    year <- function(x) {
        # lösche Zeichen vor und nach vier aufeinanderfolgenden Zahlen (YYYY)
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

#' Körperschaft, Konferenz - 1. geistiger Schöpfer (Name)
#'
#' @param item Item data parsed from PicaPlus-XML
#' @export
zdb_organ_name <- function(item) {
  tag_subf__pp(item, "029A", "a")
}

#' Körperschaft, Konferenz - 1. geistiger Schöpfer (Rolle)
#'
#' @param item Item data parsed from PicaPlus-XML
#' @export
zdb_organ_role <- function(item) {
    tag_subf__pp(item, "029A", "B")
}

#' Körperschaft, Konferenz - 1. geistiger Schöpfer (GND)
#'
#' @param item Item data parsed from PicaPlus-XML
#' @export
zdb_organ_gnd <- function(item) {
    tag_subf__pp(item, "029A", "0")
}

#' Person - Bevorzugter Name
#'
#' @param item Item data parsed from PicaPlus-XML
#' @export
zdb_person_name <- function(item) {
  if (!is.null(tag_subf__pp(item, "028A", "d")) & !is.null(tag_subf__pp(item, "028A", "a"))) {
      return(paste(tag_subf__pp(item, "028A", "d"), tag_subf__pp(item, "028A", "a")))
  }
  if (is.null(tag_subf__pp(item, "028A", "d")) & !is.null(tag_subf__pp(item, "028A", "a"))) {
      return(tag_subf__pp(item, "028A", "a"))
  }
  if (!is.null(tag_subf__pp(item, "028A", "P"))) {
      return(tag_subf__pp(item, "028A", "P"))
  }
  return(NULL)
}

#' Person - Bevorzugter Name (Rolle)
#'
#' @param item Item data parsed from PicaPlus-XML
#' @export
zdb_person_role <- function(item) {
    tag_subf__pp(item, "028A", "B")
}

#' Person - Bevorzugter Name (Rolle)
#'
#' @param item Item data parsed from PicaPlus-XML
#' @export
zdb_person_gnd <- function(item) {
    tag_subf__pp(item, "028A", "9")
}

#' Person, Familie – Sonstige und Mitwirkende (Name)
#'
#' @param item Item data parsed from PicaPlus-XML
#' @export
zdb_contributor_name <- function(item) {
    if (!is.null(tag_subf__pp(item, "028C", "d")) & !is.null(tag_subf__pp(item, "028C", "a"))) {
        return(paste(tag_subf__pp(item, "028C", "d"), tag_subf__pp(item, "028C", "a")))
    }
    if (is.null(tag_subf__pp(item, "028C", "d")) & !is.null(tag_subf__pp(item, "028C", "a"))) {
        return(tag_subf__pp(item, "028C", "a"))
    }
    if (!is.null(tag_subf__pp(item, "028C", "P"))) {
        return(tag_subf__pp(item, "028C", "P"))
    }
    return(NULL)
}

#' Person, Familie – Sonstige und Mitwirkende (Rolle)
#'
#' @param item Item data parsed from PicaPlus-XML
#' @export
zdb_contributor_role <- function(item) {
    tag_subf__pp(item, "028C", "B")
}

#' Person, Familie – Sonstige und Mitwirkende (GND)
#'
#' @param item Item data parsed from PicaPlus-XML
#' @export
zdb_contributor_gnd <- function(item) {
    tag_subf__pp(item, "028C", "9")
}

#' Einträge mit angegebener bibliographischer Gattung (bg)
#'
#' @param item Item data parsed from PicaPlus-XML
#' @param bg Bibliographic type of item to be filtered
#' @export
zdb_filter_bibtype <- function(items, bg) {
    items[ sapply( items, function(x) { grepl(bg, zdb_bibtype(x)) } ) ]
}
