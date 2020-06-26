#' PICA-basiertes Titelschema der ZDB im JSON-Format
#'
#' @export
zdb_schema <- function() {
  jsonlite::read_json("http://format.gbv.de/pica/zdb/zdbformat-title-schema.json")
}

#' Verlagsorte
#'
#' @param item Pica item retrieved from ZDB
#' @param preserve Whether to preserve empty values
#' @param collapse Whether to collapse multiple values
#' @export
zdb_place <- function(item, preserve=FALSE, collapse=FALSE) {
    tag_subf(item, "033A", "p", preserve=preserve, collapse=collapse)
}

#' Verlag, Verleger
#'
#' @param item Pica item retrieved from ZDB
#' @param preserve Whether to preserve empty values
#' @param collapse Whether to collapse multiple values
#' @export
zdb_publisher <- function(item, preserve=FALSE, collapse=FALSE) {
    tag_subf(item, "033A", "n", preserve=preserve, collapse=collapse)
}

#' Frühere Verlagsorte und Verleger
#'
#' @param item Pica item retrieved from ZDB
#' @param preserve Whether to preserve empty values
#' @param collapse Whether to collapse multiple values
#' @export
zdb_publisher_prev <- function(item, preserve=FALSE, collapse=FALSE) {
    tag_subf(item, "033B", "n", preserve=preserve, collapse=collapse)
}

#' Frühere Verlagsorte und Verleger
#'
#' @param item Pica item retrieved from ZDB
#' @param preserve Whether to preserve empty values
#' @param collapse Whether to collapse multiple values
#' @export
zdb_place_prev <- function(item, preserve=FALSE, collapse=FALSE) {
    tag_subf(item, "033B", "p", preserve=preserve, collapse=collapse)
}

#' Bibliographische Gattung
#'
#' @param item Pica item retrieved from ZDB
#' @export
zdb_bibtype <- function(item) {
  tag_subf(item, "002@", "0")
}

#' Angaben zum Inhalt
#'
#' @param item Pica item retrieved from ZDB
#' @export
zdb_contenttype <- function(item) {
  tag_subf(item, "013D", "a")
}

#' Angaben zum Inhalt (GND-Nummer)
#'
#' @param item Pica item retrieved from ZDB
#' @export
zdb_contenttype_gnd <- function(item) {
  tag_subf(item, "013D", "0")
}

#' ZDB-Nummer
#'
#' @param item Pica item retrieved from ZDB
#' @export
zdb_number <- function(item) {
    tag_subf(item, "006Z", "0")
}

#' Identifikationsnummer des Datensatzes (IDN)
#'
#' @param item Pica item retrieved from ZDB
#' @export
zdb_idn <- function(item) {
    tag_subf(item, "003@", "0")
}

#' Hauptsachtitel
#'
#' @param item Pica item retrieved from ZDB
#' @export
zdb_maintitle <- function(item) {
    tag_subf(item, "021A", "a")
}

#' Erscheinungsverlauf
#'
#' @param item Pica item retrieved from ZDB
#' @export
zdb_pubhistory <- function(item) {
    tag_subf(item, "031@", "a")
}

#' Erscheinungsverlauf (maschinell verarbeitbar)
#'
#' @param item Pica item retrieved from ZDB
#' @export
zdb_pubhistory_strict <- function(item) {
    year <- function(x) {
        # lösche Zeichen vor und nach vier aufeinanderfolgenden Zahlen (YYYY)
        gsub( ".*(\\d{4}).*", "\\1", x)
    }
    # zwei Eintraege in Unterfeldern: Erst- und Letzerscheinungsjahr
    if (!is.null(tag_subf(item, "031N", "j")) & !is.null(tag_subf(item, "031N", "k"))) {
        return(c(year(tag_subf(item, "031N", "j")), year(tag_subf(item, "031N", "k"))))
    }
    # ein Eintrag in einem Unterfeld: nur Ersterscheinungsjahr
    if (!is.null(tag_subf(item, "031N", "j")) & is.null(tag_subf(item, "031N", "k"))) {
        return(c(year(tag_subf(item, "031N", "j")), year(tag_subf(item, "031N", "j"))))
    }
    # ein Eintrag in einem Unterfeld: nur Letzterscheinungsjahr
    if (is.null(tag_subf(item, "031N", "j")) & !is.null(tag_subf(item, "031N", "k"))) {
        return(c(year(tag_subf(item, "031N", "k")), year(tag_subf(item, "031N", "k"))))
    }
    # kein Eintrag im Feld --> stattdessen „normaler“ Erscheinungsverlauf
    if (!is.null(tag_subf(item, "031@", "a"))) {
        return(c(year(tag_subf(item, "031@", "a")), year(tag_subf(item, "031@", "a"))))
    }
    cat("no publication date found!\n")
    cat(paste("item:", zdb_idn(item), "\n"))
    cat(paste("bibliographic identifiers:", zdb_bibtype(item), "\n"))
    cat(paste(tag(item, "031N")))
    cat("\n")
    return(NULL)
}

#' Körperschaft, Konferenz - 1. geistiger Schöpfer (Name)
#'
#' @param item Pica item retrieved from ZDB
#' @param preserve Whether to preserve empty values
#' @param collapse Whether to collapse multiple values
#' @export
zdb_organ_name <- function(item, preserve=FALSE, collapse=FALSE) {
  tag_subf(item, "029A", "a", preserve=preserve, collapse=collapse)
}

#' Körperschaft, Konferenz - 1. geistiger Schöpfer (Rolle)
#'
#' @param item Pica item retrieved from ZDB
#' @param preserve Whether to preserve empty values
#' @param collapse Whether to collapse multiple values
#' @export
zdb_organ_role <- function(item, preserve=FALSE, collapse=FALSE) {
    tag_subf(item, "029A", "B", preserve=preserve, collapse=collapse)
}

#' Körperschaft, Konferenz - 1. geistiger Schöpfer (GND)
#'
#' @param item Pica item retrieved from ZDB
#' @param preserve Whether to preserve empty values
#' @param collapse Whether to collapse multiple values
#' @export
zdb_organ_gnd <- function(item, preserve=FALSE, collapse=FALSE) {
    tag_subf(item, "029A", "0", preserve=preserve, collapse=collapse)
}

#' Person - Bevorzugter Name
#'
#' @param item Pica item retrieved from ZDB
#' @param collapse Whether to collapse multiple values
#' @export
zdb_person_name <- function(item, collapse=FALSE) {
  if (!is.null(tag_subf(item, "028A", "d")) & !is.null(tag_subf(item, "028A", "a"))) {
    person_name <- paste(tag_subf(item, "028A", "d", preserve=TRUE), tag_subf(item, "028A", "a", preserve=TRUE))
    if(collapse) {
      return(collapse_values(person_name, collapse="|||"))
    }
    return(person_name)
  }
  if (is.null(tag_subf(item, "028A", "d")) & !is.null(tag_subf(item, "028A", "a"))) {
      return(tag_subf(item, "028A", "a", collapse=collapse))
  }
  if (!is.null(tag_subf(item, "028A", "P"))) {
      return(tag_subf(item, "028A", "P", collapse=collapse))
  }
  return(NULL)
}

#' Person - Bevorzugter Name (Rolle)
#'
#' @param item Pica item retrieved from ZDB
#' @param preserve Whether to preserve empty values
#' @param collapse Whether to collapse multiple values
#' @export
zdb_person_role <- function(item, preserve=FALSE, collapse=FALSE) {
    tag_subf(item, "028A", "B", preserve=preserve, collapse=collapse)
}

#' Person - Bevorzugter Name (Rolle)
#'
#' @param item Pica item retrieved from ZDB
#' @param preserve Whether to preserve empty values
#' @param collapse Whether to collapse multiple values
#' @export
zdb_person_gnd <- function(item, preserve=FALSE, collapse=FALSE) {
    tag_subf(item, "028A", "9", preserve=preserve, collapse=collapse)
}

zdb_contributor <- function(item) {
    tag(item, "028C")
}

#' Person, Familie – Sonstige und Mitwirkende (Name)
#'
#' @param item Pica item retrieved from ZDB
#' @param collapse Whether to collapse multiple values
#' @export
zdb_contributor_name <- function(item, collapse=FALSE) {
    if (!is.null(tag_subf(item, "028C", "d")) & !is.null(tag_subf(item, "028C", "a"))) {
        contributor_name <- paste(tag_subf(item, "028C", "d", preserve=TRUE), tag_subf(item, "028C", "a", preserve=TRUE))
        if(collapse) {
            return(collapse_values(contributor_name))
        }
        return(contributor_name)
    }
    if (is.null(tag_subf(item, "028C", "d")) & !is.null(tag_subf(item, "028C", "a"))) {
        return(tag_subf(item, "028C", "a", collapse=collapse))
    }
    if (!is.null(tag_subf(item, "028C", "P"))) {
        return(tag_subf(item, "028C", "P", collapse=collapse))
    }
    return(NULL)
}

#' Person, Familie – Sonstige und Mitwirkende (Rolle)
#'
#' @param item Pica item retrieved from ZDB
#' @param preserve Whether to preserve empty values
#' @param collapse Whether to collapse multiple values
#' @export
zdb_contributor_role <- function(item, preserve=FALSE, collapse=FALSE) {
    tag_subf(item, "028C", "B", preserve=preserve, collapse=collapse)
}

#' Person, Familie – Sonstige und Mitwirkende (GND)
#'
#' @param item Pica item retrieved from ZDB
#' @param preserve Whether to preserve empty values
#' @param collapse Whether to collapse multiple values
#' @export
zdb_contributor_gnd <- function(item, preserve=FALSE, collapse=FALSE) {
    tag_subf(item, "028C", "9", preserve=preserve, collapse=collapse)
}

#' Einträge mit angegebener bibliographischer Gattung (bg)
#'
#' @param items Pica items retrieved from ZDB
#' @param bg Bibliographic type of item to be filtered
#' @export
zdb_filter_bibtype <- function(items, bg) {
    items[ sapply( items, function(x) { grepl(bg, zdb_bibtype(x)) } ) ]
}
