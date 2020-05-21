#' Import pica data via catmandu with given parameters
#'
#' @export
import_sru <- function(base, query, recordSchema, parser=NULL, total=NULL, target=NULL, pretty=T) {
    command <- paste("catmandu", "convert", "SRU")
    params <- paste("--base", base,
                    "--query", query,
                    "--recordSchema", recordSchema)
    rs <- paste("--recordSchema", recordSchema)
    if(!identical(parser, NULL)) {
        params <- paste(params, "--parser", parser)
    }
    if(!identical(total, NULL)) {
        params <- paste(params, "--total", total)
    }
    if(pretty) {
        params <- paste(params, "to JSON --pretty 1")
    }
    if(!identical(target, NULL)) {
        target <- file.path(target, gsub("[[:punct:]]", "-", query))
    } else {
        target <- file.path(".", gsub("[[:punct:]]","-", query))
    }
    dir.create(target, showWarnings=FALSE, recursive=TRUE)
    fp <- file.path(target, "records.json")
    system(paste(command, params, ">", fp))
    return(fp)
}
