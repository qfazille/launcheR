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
                    , name = character()
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
                    , name = character()
                    , parallelizable = logical()
                    , wait = numeric()
                    , progress = numeric()
                    , startDate = character()
                    , realStartDate = character()
                    , endDate = character()
                    , stringsAsFactors = FALSE))
    } else if (table_name == "HistorizedQueue") {
        return(data.frame(queueid = numeric()
                    , group = character()
                    , queuename = character()
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
    dbWriteTable(mydb, "WaitQueue",         get_emptyTable("WaitQueue"))
    dbWriteTable(mydb, "WaitBatch",         get_emptyTable("WaitBatch"))
    dbWriteTable(mydb, "HistorizedQueue",   get_emptyTable("HistorizedQueue"))
    dbWriteTable(mydb, "HistorizedBatch",   get_emptyTable("HistorizedBatch"))
    dbDisconnect(mydb)
}

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
emptyTable <- function(table_name) {
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    dbWriteTable(mydb, , get_emptyTable(table_name))
    dbDisconnect(mydb)
}

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect dbReadTable
getWaitQueue <- function(reset = FALSE) {
    checkDBExistance()
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    if (reset) {
        emptyTable("WaitQueue")
        df <- get_emptyTable("WaitQueue")
    } else {
        df <- dbReadTable(conn = mydb, name = "WaitQueue")
    }
    dbDisconnect(mydb)
    return(df)
}

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect dbReadTable
getWaitBatch <- function(with.done = TRUE, reset = FALSE) {
    checkDBExistance()
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    if (reset) {
        emptyTable("WaitBatch")
        df <- get_emptyTable("WaitBatch")
    } else {
        df <- dbReadTable(conn = mydb, name = "WaitBatch")
        if (!with.done) df <- df[which(df$progress >= 0),]
    }
    dbDisconnect(mydb)
    return(df)
}

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect dbReadTable
getHistorizedBatch <- function(reset = FALSE, queueid = NULL, batchid = NULL) {
    checkDBExistance()
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    if (reset) {
        emptyTable("HistorizedBatch")
        df <- get_emptyTable("HistorizedBatch")
    } else {
        df <- dbReadTable(conn = mydb, name = "HistorizedBatch")
        if (!is.null(queueid)) df <- df[which(df$queueid %in% queueid), ]
        if (!is.null(batchid)) df <- df[which(df$batchid %in% batchid), ]
    }
    dbDisconnect(mydb)
    return(df)
}

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect dbReadTable
getHistorizedQueue <- function(reset = FALSE, queueid = NULL) {
    checkDBExistance()
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    if (reset) {
        emptyTable("HistorizedQueue")
        df <- get_emptyTable("HistorizedQueue")
    } else {
        df <- dbReadTable(conn = mydb, name = "HistorizedQueue")
        if (!is.null(queueid)) df <- df[which(df$queueid %in% queueid), ]
    }
    dbDisconnect(mydb)
    return(df)
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

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
writeHistorizedBatch <- function(df) {
    df <- factors2char(df)
    stopifnot(all(sapply(df, class) == sapply(getHistorizedBatch(), class)))
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    dbWriteTable(mydb, "HistorizedBatch", df, overwrite = TRUE)
    dbDisconnect(mydb)
}

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
writeHistorizedQueue <- function(df) {
    df <- factors2char(df)
    stopifnot(all(sapply(df, class) == sapply(getHistorizedQueue(), class)))
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    dbWriteTable(mydb, "HistorizedQueue", df, overwrite = TRUE)
    dbDisconnect(mydb)
}

#' @importFrom DBI dbConnect dbSendStatement dbBind dbClearResult dbDisconnect
launchWaitBatch <- function(id) {
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    rs <- dbSendStatement(mydb, 'UPDATE WaitBatch SET wait = 0, realStartDate = :gd WHERE batchid = :bid')
    dbBind(rs, params = list(gd = getDate(), bid = id))
    dbClearResult(rs)
    dbDisconnect(mydb)
}

#' @importFrom DBI dbConnect dbSendStatement dbBind dbClearResult dbDisconnect
launchWaitQueue <- function(id) {
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    rs <- dbSendStatement(mydb, 'UPDATE WaitQueue SET wait = 0, realStartDate = :gd WHERE queueid = :qid')
    dbBind(rs, params = list(gd = getDate(), qid = id))
    dbClearResult(rs)
    dbDisconnect(mydb)
}

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
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
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    dbWriteTable(mydb, "WaitBatch", toInsert, append = TRUE)
    dbDisconnect(mydb)
}

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
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
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    dbWriteTable(mydb, "WaitQueue", toInsert, append = TRUE)
    dbDisconnect(mydb)
}

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
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
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    dbWriteTable(mydb, "HistorizedBatch", toInsert, append = TRUE)
    dbDisconnect(mydb)
}

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
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
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    dbWriteTable(mydb, "HistorizedQueue", toInsert, append = TRUE)
    dbDisconnect(mydb)
}

