# Just for dev
# Then use RSQLite : https://cran.r-project.org/web/packages/RSQLite/vignettes/RSQLite.html
# get filenames
fileWaitQueue <- function() {
    "./dev/waitqueue.RDS"
}

fileWaitBatch <- function() {
    "./dev/waitbatch.RDS"
}

fileHistorizedBatch <- function() {
    "./dev/historizedbatch.RDS"
}

fileHistorizedQueue <- function() {
    "./dev/historizedqueue.RDS"
}

# read files

#' @export
getWaitQueue <- function() {
    readRDS(file = fileWaitQueue())
}

#' @export
getWaitBatch <- function(with.done = TRUE) {
    df <- readRDS(file = fileWaitBatch())
    if (with.done) {
        return(df)
    } else {
        return(df[which(df$batchid >= 0),])
    }
}

#' @export
getHistorizedBatch <- function() {
    readRDS(file = fileHistorizedBatch())
}

#' @export
getHistorizedQueue <- function() {
    readRDS(file = fileHistorizedQueue())
}


# write files
#' @import testthat
writeWaitQueue <- function(df) {
    df <- factors2char(df)
    expect_equal(sapply(df, class), sapply(getWaitQueue(), class))
    saveRDS(object = df, file = fileWaitQueue())
}

#' @import testthat
writeWaitBatch <- function(df) {
    df <- factors2char(df)
    expect_equal(sapply(df, class), sapply(getWaitBatch(), class))
    saveRDS(object = df, file = fileWaitBatch())
}

#' @import testthat
writeHistorizedBatch <- function(df) {
    df <- factors2char(df)
    expect_equal(sapply(df, class), sapply(getHistorizedBatch(), class))
    saveRDS(object = df, file = fileHistorizedBatch())
}

#' @import testthat
writeHistorizedQueue <- function(df) {
    df <- factors2char(df)
    expect_equal(sapply(df, class), sapply(getHistorizedQueue(), class))
    saveRDS(object = df, file = fileHistorizedQueue())
}

# Launch batch or queue
launchWaitBatch <- function(id) {
    df <- getWaitBatch()
    df[which(df$batchid == id), "wait"] <- 0
    df[which(df$batchid == id), "realStartDate"] <- getDate()
    writeWaitBatch(df = df)
}

launchWaitQueue <- function(id) {
    df <- getWaitQueue()
    df[which(df$queueid == id), "wait"] <- 0
    df[which(df$queueid == id), "realStartDate"] <- getDate()
    writeWaitQueue(df = df)
}

# Add in files
addWaitBatch <- function(batchid, queueid, group = NA, name, parallelizable, wait = 0, progress = 0, startDate = getDate(), realStartDate = getDate(), endDate = NA) {
    stopifnot(!any(unlist(lapply(list(batchid, queueid, name, parallelizable, wait, progress), is.null))))
    stopifnot(class(parallelizable) == "logical")
    toInsert <- data.frame(batchid = batchid
                    , queueid = queueid
                    , group = group
                    , name = name
                    , parallelizable = parallelizable
                    , wait = wait
                    , progress = progress
                    , startDate = startDate
                    , realStartDate = realStartDate
                    , endDate = as.character(endDate)
                    , stringsAsFactors = FALSE)
    df <- getWaitBatch()
    df <- rbind(df, toInsert)
    writeWaitBatch(df = df)
}

#' @import testthat
addWaitQueue <- function(queueid, group, name, wait = 0, startDate = getDate(), realStartDate = getDate()) {
    stopifnot(!any(unlist(lapply(list(queueid, name), is.null))))
    toInsert <- data.frame(queueid = queueid
                    , group = group
                    , name = name
                    , wait = wait
                    , startDate = startDate
                    , realStartDate = realStartDate
                    , stringsAsFactors = FALSE)
    expect_equal(sapply(toInsert, class), c("numeric", "character", "character", "numeric"))
    df <- getWaitQueue()
    df <- rbind(df, toInsert)
    writeWaitQueue(df = df)
}

#' @import testthat
addHistorizedBatch <- function(queueid, batchid, group, queuename, batchname, startDate, realStartDate, endDate = getDate()) {
    stopifnot(!any(unlist(lapply(list(queueid, batchid, queuename, batchname, startDate, realStartDate, endDate)))))
    toInsert <- data.frame(queueid = queueid
                    , batchid = batchid
                    , group = group
                    , queuename = queuename
                    , batchname = batchname
                    , startDate = startDate
                    , endDate = endDate
                    , stringsAsFactors = FALSE)
    expect_equal(sapply(toInsert, class), c("numeric", "numeric", "character", "character", "character", "character", "character", "character"))
    df <- getHistorizedBatch()
    df <- rbind(df, toInsert)
    writeHistorizedBatch(df = df)
}

#' @import testthat
addHistorizedQueue <- function(queueid, group, queuename, startDate, realStartDate, endDate = getDate()) {
    stopifnot(!any(unlist(lapply(list(queueid, queuename, startDate, realStartDate, endDate)))))
    toInsert <- data.frame(queueid = queueid
                    , group = group
                    , queuename = queuename
                    , startDate = startDate
                    , realStartDate = realStartDate
                    , endDate = endDate
                    , stringsAsFactors = FALSE)
    expect_equal(sapply(toInsert, class), c("numeric", "numeric", "character", "character", "character", "character", "character", "character"))
    df <- getHistorizedQueue()
    df <- rbind(df, toInsert)
    writeHistorizedQueue(df = df)
}


