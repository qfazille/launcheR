#' @rdname createQueue
#' @title createQueue
#' @description Create an empty queue.
#' @param name Character Alias name of the queue. (Default basename(tempfile(pattern = "file", tmpdir = "")))
#' @param group Character Name of the group the queue belongs to. (Default NULL)
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
createQueue <- function(name = basename(tempfile(pattern = "queue", tmpdir = "")), group = NULL, folder = NULL, logdir = NULL, clean = TRUE, tmpdir = NULL) {
    new(Class = "queue", name = name, group = group, folder = folder, logdir = logdir, clean = clean, tmpdir = tmpdir)
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
            df[which(df$batchid == BID & df$queueid == QID), "progress"]   <- percentage
            writeWaitBatch(df = df)
        } else {
            warning("BID and/or QID not found")
        }
    }
}