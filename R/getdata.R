##### !! #####
# Important note :
# If modification on strucuture of waitqueue.RDS, waitbatch.RDS, historizedbatch.RDS or historizedqueue.RDS
# Then you need to make the modification on the files stored in inst/extdata
# This is going to change when RSQLite will be implemented.
##### !! #####

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

getWaitQueue <- function(reset = FALSE) {
    file_ <- fileWaitQueue()
    if (file.exists(file_) & !reset) {
        df <- readRDS(file = file_)
    } else {
        # create getWaitQueue
        df <- data.frame(queueid = numeric()
                        , group = character()
                        , name = character()
                        , owner = character()
                        , wait = numeric()
                        , startDate = character()
                        , realStartDate = character()
                        , stringsAsFactors = FALSE)
        saveRDS(object = df, file = file_)
    }
    return(df)
}

getWaitBatch <- function(with.done = TRUE, reset = FALSE) {
    file_ <- fileWaitBatch()
    if (file.exists(file_) & !reset) {
        df <- readRDS(file = file_)
        if (!with.done) df <- df[which(df$batchid >= 0),]
    } else {
        df <- data.frame(batchid = numeric()
                , queueid = numeric()
                , group = character()
                , path = character()
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

getHistorizedBatch <- function(reset = FALSE, queueid = NULL, batchid = NULL) {
    file_ <- fileHistorizedBatch()
    if (file.exists(file_) & !reset) {
        df <- readRDS(file = file_)
        if (!is.null(queueid)) df <- df[which(df$queueid %in% queueid), ]
        if (!is.null(batchid)) df <- df[which(df$batchid %in% batchid), ]
    } else {
        df <- data.frame(queueid = numeric()
                    , batchid = numeric()
                    , group = character()
                    , path = character()
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

getHistorizedQueue <- function(reset = FALSE, queueid = NULL) {
    file_ <- fileHistorizedQueue()
    if (file.exists(file_) & !reset) {
        df <- readRDS(file = file_)
        if (!is.null(queueid)) df <- df[which(df$queueid %in% queueid), ]
    } else {
        df <- data.frame(queueid = numeric()
                    , group = character()
                    , queuename = character()
                    , owner = character()
                    , startDate = character()
                    , realStartDate = character()
                    , endDate = character()
                    , stringsAsFactors = FALSE)
        saveRDS(object = df, file = file_)
    }
    return(df)
}


writeWaitQueue <- function(df) {
    df <- factors2char(df)
    stopifnot(all(sapply(df, class) == sapply(getWaitQueue(), class)))
    saveRDS(object = df, file = fileWaitQueue())
}

writeWaitBatch <- function(df) {
    df <- factors2char(df)
    stopifnot(all(sapply(df, class) == sapply(getWaitBatch(), class)))
    saveRDS(object = df, file = fileWaitBatch())
}

writeHistorizedBatch <- function(df) {
    df <- factors2char(df)
    stopifnot(all(sapply(df, class) == sapply(getHistorizedBatch(), class)))
    saveRDS(object = df, file = fileHistorizedBatch())
}

writeHistorizedQueue <- function(df) {
    df <- factors2char(df)
    stopifnot(all(sapply(df, class) == sapply(getHistorizedQueue(), class)))
    saveRDS(object = df, file = fileHistorizedQueue())
}

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

addWaitBatch <- function(batchid, queueid, group = NULL, path, name, parallelizable, wait = 0, progress = 0, startDate = as.character(NA), realStartDate = as.character(NA), endDate = as.character(NA)) {
    stopifnot(!any(unlist(lapply(list(batchid, queueid, name, parallelizable, wait, progress), is.null))))
    stopifnot(class(parallelizable) == "logical")
    if (is.null(group)) group <- as.character(NA) # Need to keep this line because when group is explicitly called with NULL then I get arguments imply differing number of rows: 1, 0
    toInsert <- data.frame(batchid = batchid
                    , queueid = queueid
                    , group = group
                    , path = path
                    , name = name
                    , parallelizable = parallelizable
                    , wait = wait
                    , progress = progress
                    , startDate = startDate
                    , realStartDate = realStartDate
                    , endDate = endDate
                    , stringsAsFactors = FALSE)
    stopifnot(all(sapply(toInsert, class) == c("numeric", "numeric", "character", "character", "character", "logical", "numeric", "numeric", "character", "character", "character")))
    df <- getWaitBatch()
    df <- rbind(df, toInsert)
    writeWaitBatch(df = df)
}

addWaitQueue <- function(queueid, group = NULL, name, owner, wait = 0, startDate = as.character(NA), realStartDate = as.character(NA)) {
    stopifnot(!any(unlist(lapply(list(queueid, name), is.null))))
    if (is.null(group)) group <- as.character(NA) # Need to keep this line (same as in addWaitBatch function)
    toInsert <- data.frame(queueid = queueid
                    , group = group
                    , name = name
                    , owner = owner
                    , wait = wait
                    , startDate = startDate
                    , realStartDate = realStartDate
                    , stringsAsFactors = FALSE)
    stopifnot(all(sapply(toInsert, class) == c("numeric", "character", "character", "character", "numeric", "character", "character")))
    df <- getWaitQueue()
    df <- rbind(df, toInsert)
    writeWaitQueue(df = df)
}

addHistorizedBatch <- function(queueid, batchid, group, path, queuename, batchname, startDate, realStartDate, endDate = getDate()) {
    stopifnot(!any(unlist(lapply(list(queueid, batchid, path, queuename, batchname, startDate, realStartDate, endDate), is.null))))
    toInsert <- data.frame(queueid = queueid
                    , batchid = batchid
                    , group = as.character(group)
                    , path = path
                    , queuename = queuename
                    , batchname = batchname
                    , startDate = startDate
                    , realStartDate = realStartDate
                    , endDate = endDate
                    , stringsAsFactors = FALSE)
    stopifnot(all(sapply(toInsert, class) == c("numeric", "numeric", "character", "character", "character", "character", "character", "character", "character")))
    df <- getHistorizedBatch()
    df <- rbind(df, toInsert)
    writeHistorizedBatch(df = df)
}

addHistorizedQueue <- function(queueid, group, queuename, owner, startDate, realStartDate, endDate = getDate()) {
    stopifnot(!any(unlist(lapply(list(queueid, queuename, startDate, realStartDate, endDate), is.null))))
    toInsert <- data.frame(queueid = queueid
                    , group = as.character(group)
                    , queuename = queuename
                    , owner = owner
                    , startDate = startDate
                    , realStartDate = realStartDate
                    , endDate = endDate
                    , stringsAsFactors = FALSE)
    stopifnot(all(sapply(toInsert, class) == c("numeric", "character", "character", "character", "character", "character", "character")))
    df <- getHistorizedQueue()
    df <- rbind(df, toInsert)
    writeHistorizedQueue(df = df)
}


