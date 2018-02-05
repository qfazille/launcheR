#' @export
setRData <- function(file) {
    if (file %in% list.files()) {
        file.rename(from = file, to = ".RData")
    } else {
        stop(paste("File", file, "doesn't exist"))
    }
}

#' @export
waitQueue <- function(){
    queue_name  <- getQueuename()
    group <- getGroup()

    # Get data
    df <- getWaitQueue()

    # Write new queueid
    newid <- newidQueue()
    writeRenviron(prefix = "LR_QID", value = newid)

    # if no group, then no check
    if (is.null(group)) {
        addWaitQueue(queueid = newid, group = NA, name = queue_name, wait = 0)
        # queue can be launched
    } else {
        # Check if group not present
        if (length(df[which(df$group == group),"id"]) == 0) {
            addWaitQueue(queueid = newid, group = group, name = queue_name)
            # queue can be launched
        } else {
            # if group already present then need to wait for max(id)
            wait_for_id <- max(df[which(df$group == group),"queueid"])
            # add in waiting queue
            addWaitQueue(queueid = newid, group = group, name = queue_name, wait = wait_for_id)
            # wait before launch queue
            waitForQueueid(id = wait_for_id)
            #launchWaitQueue(id = newid) # this set wait = 0 (commented for dev)
            # launch
        }
    }
}

#' @export
waitBatch <- function() {

    queue_name  <- getQueuename()
    queue_id    <- getQueueid()
    queue_group <- getGroup()
    batch_name  <- getBatchname()
    batch_par   <- getBatchpar()

    # Get data
    df <- getWaitBatch()

    # Write new batchid
    newid <- newidBatch()
    writeRenviron(prefix = "LR_BID", value = newid)

    if (length(which(df$name == batch_name)) == 0) {
        addWaitBatch(batchid = newid
            , queueid = queue_id
            , group = queue_group
            , name = batch_name
            , parallelizable = batch_par
            , wait = 0
            , progress = 0
            , startDate = getDate()
            , realStartDate = getDate())
        # launch
    } else {
        df <- df[which(df$name == batch_name),]
        id_wait_max <- df[which(df$wait == max(df$wait)), "batchid"]
        if (batch_par) {
            id_wait_max_par <- df[which(df$batchid %in% id_wait_max), "parallelizable"]
            # Either one batchid with FALSE or one or several batchid with par = TRUE
            if (all(id_wait_max_par)) {
                to_wait <- unique(df[which(df$batchid %in% id_wait_max), "wait"]) # should be one value
            } else {
                to_wait <- id_wait_max
            }
        } else {
            to_wait <- id_wait_max
        }
        # add to waiting batch
        addWaitBatch(batchid = newid
            , queueid = queue_id
            , group = queue_group
            , name = batch_name
            , parallelizable = batch_par
            , wait = to_wait
            , progress = 0
            , startDate = getDate())
        # wait before launch
        waitForBatchid(id = to_wait)
        #launchWaitBatch(id = newid) # this set wait = 0 (commented for dev)
        # launch
    }
}

#' @export
releaseBatch <- function() {
    batch_id <- getBatchid()
    df <- getWaitBatch()
    # Set wait to -1 (means batch not running anymore but queue still running)
    df[which(df$batchid == batch_id), "wait"]       <- -1
    df[which(df$batchid == batch_id), "progress"]   <- 100
    df[which(df$batchid == batch_id), "endDate"]    <- getDate()
    writeWaitBatch(df = df)
}

#' @export
#' @import testthat
releaseQueue <- function() {
    queue_id    <- getQueueid()
    queue_name  <- getQueuename()
    # Write waitBatch (priority 1)
    df <- getWaitBatch()
    bh <- df[which(df$queueid == queue_id), ]
    expect_gt(nrow(bh), 0)
    df <- df[-which(df$queueid == queue_id), ]
    writeWaitBatch(df = df)
    # Write waitQueue (priority 2)
    df <- getWaitQueue()
    qh <- df[which(df$queueid == queue_id), ]
    expect_equal(nrow(qh), 1)
    df <- df[-which(df$queueid == queue_id), ]
    writeWaitQueue(df = df)
    # Write historized.batch (priority 3)
    addHistorizedBatch(queueid = queue_id, batchid = bh$batchid, group = bh$group, queuename = queue_name, batchname = bh$name, startDate = bh$startDate, realStartDate = bh$realStartDate)
    # Write historized queue  (priority 3)
    addHistorizedQueue(queueid = queue_id, group = qh$group, queuename = qh$name, startDate = qh$startDate, realStartDate = qh$realStartDate)
}

#' @export
writeRenviron <- function(prefix, value) {
    x <- readLines(".Renviron")
    toChange <- which(startsWith(x, paste0(prefix, "=")))
    if (length(toChange) == 0) {
        x <- c(x, paste0(prefix, "='", value, "'"))
    } else {
        x[toChange] <- paste0(prefix, "='", value, "'")
    }
    cat(x, file = ".Renviron", sep = linebreak())
}
