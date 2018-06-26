#' @rdname createQueue
#' @title createQueue
#' @description Create an empty queue.
#' @param name Character Alias name of the queue. (Default basename(tempfile(pattern = "file", tmpdir = "")))
#' @param desc Character Description
#' @param group Character Name of the group the queue belongs to. (Default NULL)
#' @param owner Character username of launcher. (Default Sys.info()["user"])
#' @param folder Character Path to a folder that will contains the working directory folder. (Default tempdir())
#' @param logdir Character Path to the folder that will contains logs file. (By default in folder)
#' @param clean Logical Whether or not the working directory folder should be removed. (Default TRUE)
#' @param tmpdir Character If use through RSConnect you can redefine a tmpdir not in /tmp/*. (Default NULL)
#' @export
#' @examples
#' \dontrun{
#' q <- createQueue()
#' }
#' @import methods
createQueue <- function(name = basename(tempfile(pattern = "queue", tmpdir = "")), desc = NULL, group = NULL, owner = Sys.info()["user"], folder = NULL, logdir = NULL, clean = TRUE, tmpdir = NULL) {
    new(Class = "queue", name = name, desc = desc, group = group, owner = owner, folder = folder, logdir = logdir, clean = clean, tmpdir = tmpdir)
}


#' @rdname launcheR.reset
#' @title launcheR.reset
#' @description launcheR.reset reset waiting queue/batchs information.\cr
#'     This function should be use in case a batch or queue is stuck in the progress batchs.\cr
#'     That can happen when a process has been killed forcefully.
#'     The function can also be used to remove the historized batchs/queues
#' @param type Character One of c("all", "wait", "historized")
#' @param queueid Number vector of queueid(s)
#' @param prefix Character prefix of the queuename(s)
#' @export
#' @examples
#' \dontrun{
#' launcheR.reset()
#' }
launcheR.reset <- function(type = "all", queueid = NULL, prefix = NULL) {
    stopifnot(tolower(type) %in% c("wait", "historized", "all"))
    
    if (type %in% c("wait", "all")) {
        removeWaitBatch(queueid = queueid, prefix = prefix)
        removeWaitQueue(queueid = queueid, prefix = prefix)
    }
    
    if (type %in% c("historized", "all")) {
        removeHistorizedBatch(queueid = queueid, prefix = prefix)
        removeHistorizedQueue(queueid = queueid, prefix = prefix)
    }
}

#' @rdname progress
#' @title progress
#' @description Increase progress of a batch.
#' @param percentage Numeric Percentage of progress.
#' @export
#' @examples
#' \dontrun{
#' progress(50)
#' }
progress <- function(percentage = NULL) {
    if (is.null(percentage)) {
        warning("Cannot update progress because param percentage is null")
    } else if (!is.numeric(percentage)) {
        warning("Cannot update progress because param percentage is not numeric")
    } else {
        BID <- getBatchid(mandatory = FALSE)
        QID <- getQueueid(mandatory = FALSE)
        if (!is.null(BID) & !is.null(QID)) {
            # update progress
            df <- getWaitBatch()
            if (percentage > 100) percentage <- 100
            mydb <- dbConnect(RSQLite::SQLite(), datafilepath(), synchronous = NULL)
            res <- dbSendQuery(mydb, "PRAGMA busy_timeout=5000;")
            dbClearResult(res)
            cpt <- 0
            repeat {
                tryCatch({
                    rs <- dbSendStatement(mydb, "update WaitBatch set progress = :x where batchid = :bid and queueid = :qid")
                    dbBind(rs, params = list(x = percentage, bid = BID, qid = QID))
                    dbClearResult(rs)
                    break
                }, error = function(err) {
                    Sys.sleep(0.2)
                    cpt <- cpt + 1
                    if (cpt == 50) {
                        warning(paste("Error in progress :", err))
                        break
                    }
                })
            }
            dbDisconnect(mydb)
        }
    }
}

#' @rdname getWaitQueue
#' @title getWaitQueue
#' @description Get the waiting queue
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect dbReadTable dbSendQuery dbClearResult
#' @importFrom RSQLite SQLite
#' @export
#' @examples
#' \dontrun{
#' getWaitQueue()
#' }
getWaitQueue <- function() {
    checkDBExistance()
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath(), synchronous = NULL)
    res <- dbSendQuery(mydb, "PRAGMA busy_timeout=5000;")
    dbClearResult(res)
    df <- dbReadTable(conn = mydb, name = "WaitQueue")
    dbDisconnect(mydb)
    return(df)
}

#' @rdname getWaitBatch
#' @title getWaitBatch
#' @description Get the waiting queue
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect dbReadTable dbSendQuery dbClearResult
#' @importFrom RSQLite SQLite
#' @export
#' @examples
#' \dontrun{
#' getWaitBatch()
#' }
getWaitBatch <- function() {
    checkDBExistance()
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath(), synchronous = NULL)
    res <- dbSendQuery(mydb, "PRAGMA busy_timeout=5000;")
    dbClearResult(res)
    df <- dbReadTable(conn = mydb, name = "WaitBatch")
    dbDisconnect(mydb)
    return(df)
}

#' @rdname getHistorizedBatch
#' @title getHistorizedBatch
#' @description Get the waiting queue
#' @param queueid Numeric vector List of queueid to show. If NULL shows all. (Default NULL)
#' @param batchid Numeric vector List of batchid to show. If NULL shows all. (Default NULL)
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect dbReadTable dbSendQuery dbClearResult
#' @importFrom RSQLite SQLite
#' @export
#' @examples
#' \dontrun{
#' getHistorizedBatch()
#' }
getHistorizedBatch <- function(queueid = NULL, batchid = NULL) {
    checkDBExistance()
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath(), synchronous = NULL)
    res <- dbSendQuery(mydb, "PRAGMA busy_timeout=5000;")
    dbClearResult(res)
    df <- dbReadTable(conn = mydb, name = "HistorizedBatch")
    if (!is.null(queueid)) df <- df[which(df$queueid %in% queueid), ]
    if (!is.null(batchid)) df <- df[which(df$batchid %in% batchid), ]
    dbDisconnect(mydb)
    return(df)
}

#' @rdname getHistorizedQueue
#' @title getHistorizedQueue
#' @description Get the waiting queue
#' @param queueid Numeric vector List of queueid to show. If NULL shows all. (Default NULL)
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect dbReadTable dbSendQuery dbClearResult
#' @importFrom RSQLite SQLite
#' @export
#' @examples
#' \dontrun{
#' getHistorizedQueue()
#' }
getHistorizedQueue <- function(queueid = NULL) {
    checkDBExistance()
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath(), synchronous = NULL)
    res <- dbSendQuery(mydb, "PRAGMA busy_timeout=5000;")
    dbClearResult(res)
    df <- dbReadTable(conn = mydb, name = "HistorizedQueue")
    if (!is.null(queueid)) df <- df[which(df$queueid %in% queueid), ]
    dbDisconnect(mydb)
    return(df)
}
