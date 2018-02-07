getQueuename <- function(mandatory = TRUE) {
    val <- Sys.getenv("LR_Q")
    if (val == "") val <- NULL
    if (mandatory & is.null(val)) stop("environment variable LR_Q cannot be null") else return(val)
}

getQueueid <- function(mandatory = TRUE) {
    val <- as.numeric(Sys.getenv("LR_QID"))
    if (is.na(val)) val <- NULL
    if (mandatory & is.null(val)) stop("environment variable LR_QID cannot be null") else return(val)
}

getGroup <- function(mandatory = TRUE) {
    val <- Sys.getenv("LR_G")
    if (val == "") val <- NULL
    if (mandatory & is.null(val)) stop("environment variable LR_G cannot be null") else return(val)
}

getBatchname <- function(mandatory = TRUE) {
    val <- Sys.getenv("LR_B")
    if (val == "") val <- NULL
    if (mandatory & is.null(val)) stop("environment variable LR_B cannot be null") else return(val)
}

getBatchid <- function(mandatory = TRUE) {
    val <- as.numeric(Sys.getenv("LR_BID"))
    if (is.na(val)) val <- NULL
    if (mandatory & is.null(val)) stop("environment variable LR_BID cannot be null") else return(val)
}

getBatchpar <- function(mandatory = TRUE) {
    val <- as.logical(Sys.getenv("LR_BPAR"))
    if (is.na(val)) val <- NULL
    if (mandatory & is.null(val)) stop("environment variable LR_BPAR cannot be null") else return(val)
}

getBatchidFromRank <- function(batch_rank = NULL, mandatory = TRUE) {
    if (is.null(batch_rank)) stop("Rank must be specified")
    val <- as.numeric(Sys.getenv(paste0("LR_BR", batch_rank)))
    if (is.na(val)) val <- NULL
    if (mandatory & is.null(val)) stop(paste0("environment variable LR_BR", batch_rank, " cannot be null") else return(val)
}