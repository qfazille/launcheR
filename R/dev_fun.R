# All are dev functions
#' @importFrom utils str
strWaitBatch <- function() {
    df <- getWaitBatch()
    str(df)
}
#' @importFrom utils str
strWaitQueue <- function() {
    df <- getWaitQueue()
    str(df)
}
#' @importFrom utils str
strHistorizedBatch <- function() {
    df <- getHistorizedBatch()
    str(df)
}
#' @importFrom utils str
strHistorizedQueue <- function() {
    df <- getHistorizedQueue()
    str(df)
}
