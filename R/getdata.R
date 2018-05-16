datafilepath <- function() {
    folder <- Sys.getenv("launcheR_path")
    if (folder == "") {
        filepath <- file.path(dirname(tempdir()), "launcheR-db.sqlite")
    } else {
        filepath <- file.path(folder, "launcheR-db.sqlite")
    }
    return(filepath)
}

get_emptyTable <- function(table_name) {
    if (table_name == "WaitQueue") {
        return(data.frame(queueid = numeric()
                    , group = character()
                    , queuename = character()
                    , desc = character()
                    , owner = character()
                    , wait = numeric()
                    , startDate = character()
                    , realStartDate = character()
                    , stringsAsFactors = FALSE))
    } else if (table_name == "WaitBatch") {
        return(data.frame(batchid = numeric()
                    , queueid = numeric()
                    , group = character()
                    , path = character()
                    , batchname = character()
                    , queuename = character()
                    , desc = character()
                    , parallelizable = logical()
                    , waitBeforeNext = logical()
                    , endIfKO = logical()
                    , wait = numeric()
                    , progress = numeric()
                    , startDate = character()
                    , realStartDate = character()
                    , stringsAsFactors = FALSE))
    } else if (table_name == "HistorizedQueue") {
        return(data.frame(queueid = numeric()
                    , group = character()
                    , queuename = character()
                    , desc = character()
                    , owner = character()
                    , startDate = character()
                    , realStartDate = character()
                    , endDate = character()
                    , stringsAsFactors = FALSE))
    } else if (table_name == "HistorizedBatch") {
        return(data.frame(queueid = numeric()
                    , batchid = numeric()
                    , group = character()
                    , path = character()
                    , queuename = character()
                    , batchname = character()
                    , desc = character()
                    , status = character()
                    , startDate = character()
                    , realStartDate = character()
                    , endDate = character()
                    , stringsAsFactors = FALSE))
    }
}

checkDBExistance <- function() {
    datafilepath <- datafilepath()
    if (!file.exists(datafilepath)) createDB(datafilepath = datafilepath)
}

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
#' @importFrom RSQLite SQLite
createDB <- function(datafilepath) {
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath)
    # Manually set the permissions to all
    Sys.chmod(datafilepath, "777", use_umask = FALSE)
    dbWriteTable(mydb, "WaitQueue",         get_emptyTable("WaitQueue"), overwrite = TRUE)
    dbWriteTable(mydb, "WaitBatch",         get_emptyTable("WaitBatch"), overwrite = TRUE)
    dbWriteTable(mydb, "HistorizedQueue",   get_emptyTable("HistorizedQueue"), overwrite = TRUE)
    dbWriteTable(mydb, "HistorizedBatch",   get_emptyTable("HistorizedBatch"), overwrite = TRUE)
    dbDisconnect(mydb)
}

resetDB <- function() {
    datafilepath <- datafilepath()
    if (!file.exists(datafilepath)) {
        # If file doesn't exist, create database
        createDB(datafilepath = datafilepath)
        message("launcheR database created")
    } else {
        # if file exists, check if structure of tables are good
        db_to_create <- FALSE
        if (length(setdiff(colnames(getWaitBatch()), colnames(get_emptyTable("WaitBatch")))) > 0) db_to_create <- TRUE
        if (length(setdiff(colnames(get_emptyTable("WaitBatch")), colnames(getWaitBatch()))) > 0) db_to_create <- TRUE
        if (length(setdiff(colnames(getWaitQueue()), colnames(get_emptyTable("WaitQueue")))) > 0) db_to_create <- TRUE
        if (length(setdiff(colnames(get_emptyTable("WaitQueue")), colnames(getWaitQueue()))) > 0) db_to_create <- TRUE
        if (length(setdiff(colnames(getHistorizedBatch()), colnames(get_emptyTable("HistorizedBatch")))) > 0) db_to_create <- TRUE
        if (length(setdiff(colnames(get_emptyTable("HistorizedBatch")), colnames(getHistorizedBatch()))) > 0) db_to_create <- TRUE
        if (length(setdiff(colnames(getHistorizedQueue()), colnames(get_emptyTable("HistorizedQueue")))) > 0) db_to_create <- TRUE
        if (length(setdiff(colnames(get_emptyTable("HistorizedQueue")), colnames(getHistorizedQueue()))) > 0) db_to_create <- TRUE
        if (db_to_create) {
            createDB(datafilepath = datafilepath)
            message("launcheR database updated")
        }
    }
}

.onLoad <- function(libname, pkgname) {
    resetDB()
}

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
emptyTable <- function(table_name) {
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    dbWriteTable(mydb, table_name, get_emptyTable(table_name), overwrite = TRUE)
    dbDisconnect(mydb)
}

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
writeWaitQueue <- function(df) {
    df <- factors2char(df)
    stopifnot(all(sapply(df, class) == sapply(getWaitQueue(), class)))
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    dbWriteTable(mydb, "WaitQueue", df, overwrite = TRUE)
    dbDisconnect(mydb)
}

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
writeWaitBatch <- function(df) {
    df <- factors2char(df)
    stopifnot(all(sapply(df, class) == sapply(getWaitBatch(), class)))
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    dbWriteTable(mydb, "WaitBatch", df, overwrite = TRUE)
    dbDisconnect(mydb)
}

#' @importFrom DBI dbConnect dbSendStatement dbBind dbClearResult dbDisconnect
launchWaitBatch <- function(id) {
    # Update wait & realStartDate
    df <- getWaitBatch()
    df[which(df$batchid %in% id), "realStartDate"] <- getDate()
    df[which(df$batchid %in% id), "wait"] <- 0
    # Remove duplicates (one row per id to wait)
    df <- unique(df)
    writeWaitBatch(df)
}

#' @importFrom DBI dbConnect dbSendStatement dbBind dbClearResult dbDisconnect
launchWaitQueue <- function(id) {
    # Update wait & realStartDate
    df <- getWaitQueue()
    df[which(df$queueid %in% id), "realStartDate"] <- getDate()
    df[which(df$queueid %in% id), "wait"] <- 0
    # Remove duplicates (one row per id to wait)
    df <- unique(df)
    writeWaitQueue(df)
}

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
addWaitBatch <- function(batchid, queueid, group = as.character(NA), path, batchname, queuename, desc = as.character(NA), parallelizable, waitBeforeNext, endIfKO, wait = 0, progress = 0, startDate = as.character(NA)) {
    stopifnot(!any(unlist(lapply(list(batchid, queueid, batchname, parallelizable, wait, progress, waitBeforeNext, endIfKO), is.null))))
    toInsert <- data.frame(batchid = batchid
                    , queueid = queueid
                    , group = group
                    , path = path
                    , batchname = batchname
                    , desc = desc
                    , parallelizable = parallelizable
                    , waitBeforeNext = waitBeforeNext
                    , endIfKO = endIfKO
                    , wait = wait
                    , progress = progress
                    , startDate = startDate
                    , realStartDate = as.character(NA)
                    , stringsAsFactors = FALSE)
    stopifnot(all(sapply(toInsert, class) == c("numeric", "numeric", "character", "character", "character", "character", "logical", "logical", "logical", "numeric", "numeric", "character", "character")))
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    dbWriteTable(mydb, "WaitBatch", toInsert, append = TRUE)
    dbDisconnect(mydb)
}

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
addWaitQueue <- function(queueid, group = as.character(NA), queuename, desc = as.character(NA), owner, wait = 0, startDate = as.character(NA), realStartDate = as.character(NA)) {
    stopifnot(!any(unlist(lapply(list(queueid, queuename), is.null))))
    toInsert <- data.frame(queueid = queueid
                    , group = group
                    , queuename = queuename
                    , desc = desc
                    , owner = owner
                    , wait = wait
                    , startDate = startDate
                    , realStartDate = realStartDate
                    , stringsAsFactors = FALSE)
    stopifnot(all(sapply(toInsert, class) == c("numeric", "character", "character", "character", "character", "numeric", "character", "character")))
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    dbWriteTable(mydb, "WaitQueue", toInsert, append = TRUE)
    dbDisconnect(mydb)
}

removeWaitQueue <- function(queueid) {
    df <- getWaitQueue()
    if (any(queueid %in% df$queueid)) {
        df <- df[which(!df$queueid %in% queueid), ]
        writeWaitQueue(df)
    }
}

removeWaitBatch <- function(batchid) {
    df <- getWaitBatch()
    if (any(batchid %in% df$batchid)) {
        df <- df[which(!df$batchid %in% batchid), ]
        writeWaitBatch(df)
    }
}

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
historizedQueue <- function(queueid, status = "OK", endDate = getDate()) {
    df <- getWaitQueue()
    # keep order in queueid (param)
    df <- df[match(queueid, df$queueid), c("queueid", "group", "queuename", "desc", "owner", "startDate", "realStartDate")]
    
    # Check on endDate
    if (length(endDate) > 1) {
        stopifnot(nrow(df) == length(endDate))
    }
    
    # Check on status
    if (length(status) > 1) {
        stopifnot(nrow(df) == length(status))
    }
    
    # Add endDate & status
    df$status  <- status
    df$endDate <- endDate
    
    # Remove from waitQueue
    removeWaitQueue(queueid = queueid)
    
    # Get columns order
    cols <- colnames(getHistorizedQueue())
    
    # Add in historizedQueue
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    dbWriteTable(mydb, "HistorizedQueue", df[,match(cols, colnames(df))], append = TRUE)
    dbDisconnect(mydb)
}

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
# No need to set status & endDate here, it will be done in releaseBatch
historizedBatch <- function(batchid, status, endDate = getDate()) {
    df <- getWaitBatch()
    # keep order in batchid (param)
    df <- df[match(batchid, df$batchid), c("batchid", "queueid", "group", "path", "queuename", "batchname", "desc", "startDate", "realStartDate")]
    
    # Check on endDate
    if (length(endDate) > 1) {
        stopifnot(nrow(df) == length(endDate))
    }
    
    # Check on status
    if (length(status) > 1) {
        stopifnot(nrow(df) == length(status))
    }
    
    # Add endDate & status
    df$status  <- status
    df$endDate <- endDate
    
    removeWaitBatch(batchid = batchid)
    
    # Get columns order
    cols <- colnames(getHistorizedBatch())
    
    # Add in historizedBatch
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    dbWriteTable(mydb, "HistorizedBatch", df[,match(cols, colnames(df))], append = TRUE)
    dbDisconnect(mydb)
}