# All are dev functions
#' @importFrom utils str
str.wait.batch <- function() {
    df <- get.wait.batch()
    str(df)
}
#' @importFrom utils str
str.wait.queue <- function() {
    df <- get.wait.queue()
    str(df)
}
#' @importFrom utils str
str.historized.batch <- function() {
    df <- get.historized.batch()
    str(df)
}
#' @importFrom utils str
str.historized.queue <- function() {
    df <- get.historized.queue()
    str(df)
}
