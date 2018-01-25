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

# Very powerfull function that converts all numeric type to num or int and characters to factors.
setNumOrFact <- function(x) {
    x <- as.data.frame(x)
    stopifnot(is.list(x))
    x[] <- rapply(x, utils::type.convert, classes = "character", how = "replace")
    return(as.data.frame(x))
}

# Without checks
setNumOrFact <- function(x) {
    rapply(x, utils::type.convert, classes = "character", how = "replace")
}
