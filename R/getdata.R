# Just for dev
# Then use RSQLite : https://cran.r-project.org/web/packages/RSQLite/vignettes/RSQLite.html
# get filenames
datafolder <- function() {
    return(file.path(system.file(package = "launcheR"), "extdata"))
}

fileWaitQueue <- function() {
    file.path(datafolder(), "waitqueue.RDS")
}

fileWaitBatch <- function() {
    file.path(datafolder(), "waitbatch.RDS")
}

fileHistorizedBatch <- function() {
    file.path(datafolder(), "historizedbatch.RDS")
}

fileHistorizedQueue <- function() {
    file.path(datafolder(), "historizedqueue.RDS")
}

#' @export
getWaitQueue <- function() {
    file_ <- fileWaitQueue()
    if (file.exists(file_)) {
        df <- readRDS(file = file_)
    } else {
        # create getWaitQueue
        df <- data.frame(queueid = numeric()
                        , group = character()
                        , name = character()
                        , wait = numeric()
                        , startDate = character()
                        , realStartDate = character()
                        , stringsAsFactors = FALSE)
        saveRDS(object = df, file = file_)
    }
    return(df)
}

#' @export
getWaitBatch <- function(with.done = TRUE) {
    file_ <- fileWaitBatch()
    if (file.exists(file_)) {
        df <- readRDS(file = file_)
        if (!with.done) df <- df[which(df$batchid >= 0),]
    } else {
        df <- data.frame(batchid = numeric()
                , queueid = numeric()
                , group = character()
                , name = character()
                , parallelizable = logical()
                , wait = numeric()
                , progress = numeric()
                , startDate = character()
                , realStartDate = character()
                , endDate = character()
                , stringsAsFactors = FALSE)
        saveRDS(object = df, file = file_)
    }
    return(df)
}

#' @export
getHistorizedBatch <- function() {
    file_ <- fileHistorizedBatch()
    if (file.exists(file_)) {
        df <- readRDS(file = file_)
    } else {
        df <- data.frame(queueid = numeric()
                    , batchid = numeric()
                    , group = character()
                    , queuename = character()
                    , batchname = character()
                    , startDate = character()
                    , realStartDate = character()
                    , endDate = character()
                    , stringsAsFactors = FALSE)
        saveRDS(object = df, file = file_)
    }
    return(df)
}

#' @export
getHistorizedQueue <- function() {
    file_ <- fileHistorizedQueue()
    if (file.exists(file_)) {
        df <- readRDS(file = file_)
    } else {
        df <- data.frame(queueid = numeric()
                    , group = character()
                    , queuename = character()
                    , startDate = character()
                    , realStartDate = character()
                    , endDate = character()
                    , stringsAsFactors = FALSE)
        saveRDS(object = df, file = file_)
    }
    return(df)
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
addWaitBatch <- function(batchid, queueid, group = NULL, name, parallelizable, wait = 0, progress = 0, startDate = getDate(), realStartDate = getDate(), endDate = NA) {
    stopifnot(!any(unlist(lapply(list(batchid, queueid, name, parallelizable, wait, progress), is.null))))
    stopifnot(class(parallelizable) == "logical")
    if (is.null(group)) group <- as.character(NA)
    toInsert <- data.frame(batchid = batchid
                    , queueid = queueid
                    , group = as.character(group)
                    , name = name
                    , parallelizable = parallelizable
                    , wait = wait
                    , progress = progress
                    , startDate = startDate
                    , realStartDate = realStartDate
                    , endDate = as.character(endDate)
                    , stringsAsFactors = FALSE)
    expect_true(all(sapply(toInsert, class) == c("numeric", "numeric", "character", "character", "logical", "numeric", "numeric", "character", "character", "character")))
    df <- getWaitBatch()
    df <- rbind(df, toInsert)
    writeWaitBatch(df = df)
}

#' @import testthat
addWaitQueue <- function(queueid, group = NA, name, wait = 0, startDate = getDate(), realStartDate = getDate()) {
    stopifnot(!any(unlist(lapply(list(queueid, name), is.null))))
    toInsert <- data.frame(queueid = queueid
                    , group = as.character(group)
                    , name = name
                    , wait = wait
                    , startDate = startDate
                    , realStartDate = realStartDate
                    , stringsAsFactors = FALSE)
    expect_true(all(sapply(toInsert, class) == c("numeric", "character", "character", "numeric", "character", "character")))
    df <- getWaitQueue()
    df <- rbind(df, toInsert)
    writeWaitQueue(df = df)
}

#' @import testthat
addHistorizedBatch <- function(queueid, batchid, group, queuename, batchname, startDate, realStartDate, endDate = getDate()) {
    stopifnot(!any(unlist(lapply(list(queueid, batchid, queuename, batchname, startDate, realStartDate, endDate), is.null))))
    toInsert <- data.frame(queueid = queueid
                    , batchid = batchid
                    , group = as.character(group)
                    , queuename = queuename
                    , batchname = batchname
                    , startDate = startDate
                    , realStartDate = realStartDate
                    , endDate = endDate
                    , stringsAsFactors = FALSE)
    expect_true(all(sapply(toInsert, class) == c("numeric", "numeric", "character", "character", "character", "character", "character", "character")))
    df <- getHistorizedBatch()
    df <- rbind(df, toInsert)
    writeHistorizedBatch(df = df)
}

#' @import testthat
addHistorizedQueue <- function(queueid, group, queuename, startDate, realStartDate, endDate = getDate()) {
    stopifnot(!any(unlist(lapply(list(queueid, queuename, startDate, realStartDate, endDate), is.null))))
    toInsert <- data.frame(queueid = queueid
                    , group = as.character(group)
                    , queuename = queuename
                    , startDate = startDate
                    , realStartDate = realStartDate
                    , endDate = endDate
                    , stringsAsFactors = FALSE)
    expect_true(all(sapply(toInsert, class) == c("numeric", "character", "character", "character", "character", "character")))
    df <- getHistorizedQueue()
    df <- rbind(df, toInsert)
    writeHistorizedQueue(df = df)
}


