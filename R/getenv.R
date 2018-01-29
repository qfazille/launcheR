get.queuename <- function(mandatory = TRUE) {
    val <- Sys.getenv("LR_Q")
    if (val == "") val <- NULL
    if (mandatory & is.null(val)) stop("environment variable LR_Q cannot be null") else return(val)
}

get.queueid <- function(mandatory = TRUE) {
    val <- as.numeric(Sys.getenv("LR_QID"))
    if (is.na(val)) val <- NULL
    if (mandatory & is.null(val)) stop("environment variable LR_QID cannot be null") else return(val)
}

get.group <- function(mandatory = TRUE) {
    val <- Sys.getenv("LR_G")
    if (val == "") val <- NULL
    if (mandatory & is.null(val)) stop("environment variable LR_G cannot be null") else return(val)
}

get.batchname <- function(mandatory = TRUE) {
    val <- Sys.getenv("LR_B")
    if (val == "") val <- NULL
    if (mandatory & is.null(val)) stop("environment variable LR_B cannot be null") else return(val)
}

get.batchid <- function(mandatory = TRUE) {
    val <- as.numeric(Sys.getenv("LR_QBD"))
    if (is.na(val)) val <- NULL
    if (mandatory & is.null(val)) stop("environment variable LR_QBD cannot be null") else return(val)
}

get.batchpar <- function(mandatory = TRUE) {
    val <- as.logical(Sys.getenv("LR_BPAR"))
    if (is.na(val)) val <- NULL
    if (mandatory & is.null(val)) stop("environment variable LR_BPAR cannot be null") else return(val)
}
