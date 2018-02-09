# convert factors to character
factors2char <- function(x) {
    i <- sapply(x, is.factor)
    x[i] <- lapply(x[i], as.character)
    x
}

# convert characters to factors
char2factors <- function(x) {
    i <- sapply(x, is.character)
    x[i] <- lapply(x[i], as.factor)
    x
}

#' @importFrom utils type.convert
setNumOrFact <- function(x) {
    rapply(x, utils::type.convert, classes = "character", how = "replace")
}
