#' Get pica items imported via catmandu and parsed as picaxml
#'
#' @export
get_pica <- function(fp) {
    # # read json file from given path
    result <- jsonlite::read_json(fp)
    # extract set of pica tags for each item
    records <- sapply(result, function(x) {
        setNames(lapply(x$record, function(y) unlist(y)[3:length(unlist(y))]),
                 sapply(x$record, function(z) {z[[1]]}))
    })
    # set PPNs as names
    setNames(records, sapply(result, function(x) x[["_id"]]))
}
