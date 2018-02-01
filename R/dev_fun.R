# All are dev functions
str.wait.batch <- function() {
    df <- get.wait.batch()
    str(df)
}
str.wait.queue <- function() {
    df <- get.wait.queue()
    str(df)
}
str.historized.batch <- function() {
    df <- get.historized.batch()
    str(df)
}
str.historized.queue <- function() {
    df <- get.historized.queue()
    str(df)
}
