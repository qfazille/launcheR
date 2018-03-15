#' @rdname createQueue
#' @title createQueue
#' @description Create an empty queue.
#' @param name Character Alias name of the queue. (Default basename(tempfile(pattern = "file", tmpdir = "")))
#' @param group Character Name of the group the queue belongs to. (Default NULL)
#' @param owner Character username of launcher. (Default Sys.info()["user"])
#' @param folder Character Path to a folder that will contains the working directory folder. (Default tempdir())
#' @param logdir Character Path to the folder that will contains logs file. (By default in folder)
#' @param clean Logical Whether or not the working directory folder should be removed. (Default TRUE)
#' @param tmpdir Character If use through RSConnect then you must define a tmpdir not in /tmp/*. (Default TRUE)
#' @export
#' @examples
#' \dontrun{
#' q <- createQueue()
#' }
#' @importFrom methods new
createQueue <- function(name = basename(tempfile(pattern = "queue", tmpdir = "")), group = NULL, owner = Sys.info()["user"], folder = NULL, logdir = NULL, clean = TRUE, tmpdir = NULL) {
    new(Class = "queue", name = name, group = group, owner = owner, folder = folder, logdir = logdir, clean = clean, tmpdir = tmpdir)
}


#' @rdname reset
#' @title reset
#' @description Reset all waiting queue/batchs information.\cr
#'     This function should be use in case a batch or queue is stuck in the progress batchs.\cr
#'     That can happen when a process has been killed forcefully.
#' @param historized Logical If historized queue/batchs must be also reset.
#' @export
#' @examples
#' \dontrun{
#' reset()
#' }
reset <- function(historized = FALSE) {
    getWaitQueue(reset = TRUE)
    getWaitBatch(reset = TRUE)
    if (historized) {
        getHistorizedBatch(reset = TRUE)
        getHistorizedQueue(reset = TRUE)
    }
    return(TRUE)
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
            tryCatch({
                df[which(df$batchid == BID & df$queueid == QID), "progress"]   <- percentage
                writeWaitBatch(df = df)
            }, error = function(err) {
                warning(paste("Queueid", QID, "- Batchid", BID, "not found"))
            })
        }
    }
}

#' @rdname getWaitQueue
#' @title getWaitQueue
#' @description Get the waiting queue
#' @param reset Logical If true then reset table. (Default FALSE)
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect dbReadTable
#' @importFrom RSQLite SQLite
#' @export
#' @examples
#' \dontrun{
#' getWaitQueue()
#' }
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

#' @rdname getWaitBatch
#' @title getWaitBatch
#' @description Get the waiting queue
#' @param with.done Logical If true show also ended batchs. (Default TRUE)
#' @param reset Logical If true then reset table. (Default FALSE)
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect dbReadTable
#' @importFrom RSQLite SQLite
#' @export
#' @examples
#' \dontrun{
#' getWaitBatch()
#' }
getWaitBatch <- function(with.done = TRUE, reset = FALSE) {
    checkDBExistance()
    mydb <- dbConnect(RSQLite::SQLite(), datafilepath())
    if (reset) {
        emptyTable("WaitBatch")
        df <- get_emptyTable("WaitBatch")
    } else {
        df <- dbReadTable(conn = mydb, name = "WaitBatch")
        if (!with.done) df <- df[which(df$wait >= 0),] # Because when batch end wait == -1
    }
    dbDisconnect(mydb)
    return(df)
}

#' @rdname getHistorizedBatch
#' @title getHistorizedBatch
#' @description Get the waiting queue
#' @param reset Logical If true then reset table. (Default FALSE)
#' @param queueid Numeric vector List of queueid to show. If NULL shows all. (Default NULL)
#' @param batchid Numeric vector List of batchid to show. If NULL shows all. (Default NULL)
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect dbReadTable
#' @importFrom RSQLite SQLite
#' @export
#' @examples
#' \dontrun{
#' getHistorizedBatch()
#' }
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

#' @rdname getHistorizedQueue
#' @title getHistorizedQueue
#' @description Get the waiting queue
#' @param reset Logical If true then reset table. (Default FALSE)
#' @param queueid Numeric vector List of queueid to show. If NULL shows all. (Default NULL)
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect dbReadTable
#' @importFrom RSQLite SQLite
#' @export
#' @examples
#' \dontrun{
#' getHistorizedQueue()
#' }
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
