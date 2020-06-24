#' Get pica items imported via catmandu and parsed as picaxml
#'
#' @param fp Path to json file with parsed pica data.
#' @importFrom jsonlite read_json
#' @importFrom stats setNames
#' @export
get_data <- function(fp) {
    # # read json file from given path
    result <- jsonlite::read_json(fp)
    # extract set of pica tags for each item
    records <- sapply(result, function(x) {
        setNames(lapply(x$record, function(y) unlist(y)[3:length(unlist(y))]),
                 sapply(x$record, function(z) {z[[1]]}))
    })
    # set PPNs as names
    stats::setNames(records, sapply(result, function(x) x[["_id"]]))
}

#' Get pica items imported via catmandu and parsed from picaplus-xml
#'
#' @param fp Path to json file with parsed pica data.
#' @importFrom jsonlite read_json
#' @importFrom stats setNames
#' @export
get_data__pp <- function(fp) {
    # read json file from given path
    result <- jsonlite::read_json(fp)
    # extract set of global pica tags for each item
    records <- lapply(result, function(x) {
        x$recordData$record$global$tag
    })
    # set IDNs as names
    names(records) <- sapply(records, function(x) x[["003@"]]$subf$content)
    records
}
