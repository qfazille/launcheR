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

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect dbSendQuery dbClearResult
#' @importFrom RSQLite SQLite
createDB <- function(datafilepath) {
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath)
    res <- dbSendQuery(mydb, "PRAGMA busy_timeout=5000;")
    dbClearResult(res)
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

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect dbSendQuery dbClearResult
emptyTable <- function(table_name) {
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    res <- dbSendQuery(mydb, "PRAGMA busy_timeout=5000;")
    # repeat try to add
    dbClearResult(res)
    dbWriteTable(mydb, table_name, get_emptyTable(table_name), overwrite = TRUE)
    dbDisconnect(mydb)
}

# # #' @importFrom DBI dbConnect dbWriteTable dbDisconnect dbSendQuery dbClearResult
# # writeWaitQueue <- function(df) {
    # # df <- factors2char(df)
    # # stopifnot(all(sapply(df, class) == sapply(getWaitQueue(), class)))
    # # mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    # # res <- dbSendQuery(mydb, "PRAGMA busy_timeout=5000;")
    # # dbClearResult(res)
    # # repeat {
        # # rv <- try(dbWriteTable(mydb, "WaitQueue", df, overwrite = TRUE))
        # # if(!is(rv, "try-error")) break
    # # }
    # # dbDisconnect(mydb)
# # }

# # #' @importFrom DBI dbConnect dbWriteTable dbDisconnect dbSendQuery dbClearResult
# # writeWaitBatch <- function(df) {
    # # df <- factors2char(df)
    # # stopifnot(all(sapply(df, class) == sapply(getWaitBatch(), class)))
    # # mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    # # res <- dbSendQuery(mydb, "PRAGMA busy_timeout=5000;")
    # # dbClearResult(res)
    # # repeat {
        # # rv <- try(dbWriteTable(mydb, "WaitBatch", df, overwrite = TRUE))
        # # if(!is(rv, "try-error")) break
    # # }
    # # dbDisconnect(mydb)
# # }

#' @importFrom DBI dbConnect dbSendQuery dbClearResult dbSendStatement dbBind dbDisconnect
launchWaitBatch <- function(id) {
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    res <- dbSendQuery(mydb, "PRAGMA busy_timeout=5000;")
    dbClearResult(res)
    cpt <- 0
    repeat {
        tryCatch({
            rs <- dbSendStatement(mydb, "update WaitBatch set realStartDate = :x, wait = 0 where batchid = :y")
            dbBind(rs, params = list(x = getDate(), y = id))
            dbClearResult(rs)
            break
        }, error = function(err) {
            Sys.sleep(0.2)
            cpt <- cpt + 1
            if (cpt == 50) {
                print(paste("Error in launchWaitBatch :", err))
                break
            }
        })
    }
    dbDisconnect(mydb)
}

#' @importFrom DBI dbConnect dbSendQuery dbClearResult dbSendStatement dbBind dbDisconnect
launchWaitQueue <- function(id) {
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    res <- dbSendQuery(mydb, "PRAGMA busy_timeout=5000;")
    dbClearResult(res)
    cpt <- 0
    repeat {
        tryCatch({
            rs <- dbSendStatement(mydb, "update WaitQueue set realStartDate = :x, wait = 0 where queueid = :y")
            dbBind(rs, params = list(x = getDate(), y = id))
            dbClearResult(rs)
            break
        }, error = function(err) {
            Sys.sleep(0.2)
            cpt <- cpt + 1
            if (cpt == 50) {
                print(paste("Error in launchWaitQueue :", err))
                break
            }
        })
    }
    dbDisconnect(mydb)
}

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect dbSendQuery dbClearResult
addWaitBatch <- function(batchid, queueid, group = as.character(NA), path, batchname, queuename, desc = as.character(NA), parallelizable, waitBeforeNext, endIfKO, wait = 0, progress = 0, startDate = as.character(NA)) {
    stopifnot(!any(unlist(lapply(list(batchid, queueid, batchname, queuename, parallelizable, wait, progress, waitBeforeNext, endIfKO), is.null))))
    toInsert <- data.frame(batchid = batchid
                    , queueid = queueid
                    , group = group
                    , path = path
                    , batchname = batchname
                    , queuename = queuename
                    , desc = desc
                    , parallelizable = parallelizable
                    , waitBeforeNext = waitBeforeNext
                    , endIfKO = endIfKO
                    , wait = wait
                    , progress = progress
                    , startDate = startDate
                    , realStartDate = as.character(NA)
                    , stringsAsFactors = FALSE)
    stopifnot(all(sapply(toInsert, class) == c("numeric", "numeric", "character", "character", "character", "character", "character", "logical", "logical", "logical", "numeric", "numeric", "character", "character")))
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    res <- dbSendQuery(mydb, "PRAGMA busy_timeout=5000;")
    dbClearResult(res)
    cpt <- 0
    repeat {
        tryCatch({
            dbWriteTable(mydb, "WaitBatch", toInsert, append = TRUE)
            break
        }, error = function(err) {
            Sys.sleep(0.2)
            cpt <- cpt + 1
            if (cpt == 50) {
                print(paste("Error in addWaitBatch :", err))
                break
            }
        })
    }
    dbDisconnect(mydb)
}

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect dbSendQuery dbClearResult
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
    res <- dbSendQuery(mydb, "PRAGMA busy_timeout=5000;")
    dbClearResult(res)
    cpt <- 0
    repeat {
        tryCatch({
            dbWriteTable(mydb, "WaitQueue", toInsert, append = TRUE)
            break
        }, error = function(err) {
            Sys.sleep(0.2)
            cpt <- cpt + 1
            if (cpt == 50) {
                print(paste("Error in WaitQueue :", err))
                break
            }
        })
    }
    dbDisconnect(mydb)
}

#' @importFrom DBI dbConnect dbSendQuery dbClearResult dbSendStatement dbBind dbDisconnect
removeWaitQueue <- function(queueid) {
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    res <- dbSendQuery(mydb, "PRAGMA busy_timeout=5000;")
    dbClearResult(res)
    cpt <- 0
    repeat {
        tryCatch({
            rs <- dbSendStatement(mydb, "delete from WaitQueue where queueid = :x")
            dbBind(rs, params = list(x = queueid))
            dbClearResult(rs)
            break
        }, error = function(err) {
            Sys.sleep(0.2)
            cpt <- cpt + 1
            if (cpt == 50) {
                print(paste("Error in removeWaitQueue :", err))
                break
            }
        })
    }
    dbDisconnect(mydb)
}

#' @importFrom DBI dbConnect dbSendQuery dbClearResult dbSendStatement dbBind dbDisconnect
removeWaitBatch <- function(batchid) {
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    res <- dbSendQuery(mydb, "PRAGMA busy_timeout=5000;")
    dbClearResult(res)
    cpt <- 0
    repeat {
        tryCatch({
            rs <- dbSendStatement(mydb, "delete from WaitBatch where batchid = :x")
            dbBind(rs, params = list(x = batchid))
            dbClearResult(rs)
            break
        }, error = function(err) {
            Sys.sleep(0.2)
            cpt <- cpt + 1
            if (cpt == 50) {
                print(paste("Error in removeWaitBatch :", err))
                break
            }
        })
    }
    dbDisconnect(mydb)
}

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect dbSendQuery dbClearResult
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
    res <- dbSendQuery(mydb, "PRAGMA busy_timeout=5000;")
    dbClearResult(res)
    cpt <- 0
    repeat {
        tryCatch({
            dbWriteTable(mydb, "HistorizedQueue", df[,match(cols, colnames(df))], append = TRUE)
            break
        }, error = function(err) {
            Sys.sleep(0.2)
            cpt <- cpt + 1
            if (cpt == 50) {
                print(paste("Error in historizedQueue :", err))
                break
            }
        })
    }
    dbDisconnect(mydb)
}

#' @importFrom DBI dbConnect dbWriteTable dbDisconnect dbSendQuery dbClearResult
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
    res <- dbSendQuery(mydb, "PRAGMA busy_timeout=5000;")
    dbClearResult(res)
    cpt <- 0
    repeat {
        tryCatch({
            dbWriteTable(mydb, "HistorizedBatch", df[,match(cols, colnames(df))], append = TRUE)
            break
        }, error = function(err) {
            Sys.sleep(0.2)
            cpt <- cpt + 1
            if (cpt == 50) {
                print(paste("Error in historizedBatch :", err))
                break
            }
        })
    }
    dbDisconnect(mydb)
}