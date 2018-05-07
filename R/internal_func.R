waitQueue <- function(){

    ### Write queue_name, group & owner in Renviron
    ##writeRenviron(prefix = "LR_Q", value = queue_name)
    ##if (!is.null(group)) writeRenviron(prefix = "LR_G", value = group)
    ##writeRenviron(prefix = "LR_QOWN", value = owner)
    queue <- loadMeta()
    
    # Get data
    df <- getWaitQueue()

    # Write new queueid
    newid <- newidQueue()
    writeRenviron(prefix = "LR_QID", value = newid)

    # if no group, then no check
    if (is.null(queue@group)) {
        addWaitQueue(queueid = newid, queuename = queue@name, owner = queue@owner, wait = 0, startDate = getDate(), realStartDate = getDate())
        # queue can be launched
    } else {
        # Check if group not present
        if (nrow(df[which(df$group == queue@group),]) == 0) {
            addWaitQueue(queueid = newid, group = queue@group, queuename = queue@name, owner = queue@owner, startDate = getDate(), realStartDate = getDate())
            # queue can be launched
        } else {
            # if group already present then need to wait for max(id)
            wait_for_id <- max(df[which(df$group == queue@group),"queueid"])
            # add in waiting queue
            addWaitQueue(queueid = newid, group = queue@group, queuename = queue@name, owner = queue@owner, wait = wait_for_id, startDate = getDate())
            # wait before launch queue
            waitForQueueid(id = wait_for_id)
            launchWaitQueue(id = newid)
            # launch
        }
    }
}

waitBatch <- function(Rank) {
    # Get QID
    queue_id     <- getQueueid()

    # Get meta
    queue <- loadMeta()
    batch <- batchFromRank(object = queue, Rank = Rank)
    
    # Write new batchid (I don't think LR_BID will exists anymore..., to be check later)
    newid <- newidBatch()
    writeRenviron(prefix = paste0("LR_BR", Rank), value = newid)
    
    # There are 2 checks :
    #   - search for other same path with parallelizable = FALSE
    #   - search for other batch in queueid according to wbn
    wb <- getWaitBatch()
    df <- wb
    if (length(which(df$path == batch@path)) != 0) {
        df <- df[which(df$path == batch@path),]
        id_wait_max <- df[which(df$wait == max(df$wait)), "batchid"]
        if (batch@parallelizable) {
            id_wait_max_par <- df[which(df$batchid %in% id_wait_max), "parallelizable"]
            # Either one batchid with FALSE or one or several batchid with par = TRUE
            if (all(id_wait_max_par)) {
                to_wait <- unique(df[which(df$batchid %in% id_wait_max), "wait"]) # should be one value
            } else {
                to_wait <- id_wait_max
            }
        } else {
            to_wait_1 <- id_wait_max
        }
    } else {
        to_wait_1 <- c()
    }
    
    df <- wb
    df <- df[which(df$queueid == queue_id)]
    if (nrow(df) > 0) {
        max_BID <- max(df$batchid)
        df <- test[which(test$batchid != max_BID),]
        max_BID_withTRUE <- which(df$WaitBeforeNext == TRUE)[length(which(df$WaitBeforeNext == TRUE))]
        # Add all previous with WaitBeforeNext == FALSE
        if (max_BID_withTRUE != max(df$batchid)) {
            toAdd <- df[which(df$batchid > max_BID_withTRUE), "batchid"]
        } else {
            toAdd <- c()
        }
        to_wait_2 <- c(max_BID, toAdd)
    } else {
        to_wait_2 <- c()
    }
    id_to_wait <- c(to_wait_1, to_wait_2)
    if (length(id_to_wait) == 0) id_to_wait <- 0
    
    
    # add to waiting batch
    addWaitBatch(batchid = newid
        , queueid = queue_id
        , group = queue@group
        , path = batch@path
        , batchname = batch@name
        , queuename = queue@name
        , desc = batch@desc
        , parallelizable = batch@parallelizable
        , waitBeforeNext = batch@waitBeforeNext
        , endIfKO = batch@endIfKO
        , wait = id_to_wait # This can create several lines
        , progress = 0
        , startDate = getDate())
    
    # wait before launch (stop if batch abandonned)
    waitForBatchid(id = id_to_wait)
    
    # Write realStartDate & set wait = 0
    launchWaitBatch(id = newid)
    # launch
    
    # Just before the batch launch, update the LR_BID value in .Renviron (for progress function)
    writeRenviron(prefix = "LR_BID", value = newid)
}

releaseBatch <- function(id = NULL, batch_rank = NULL, status = "OK") {
    stopifnot(any(!sapply(list(id, batch_rank), is.null)))
    if (is.null(id)) {
        id <- getBatchidFromRank(batch_rank)
    }
    df <- getWaitBatch()
    bh <- df[which(df$batchid == id), ]
    queueid <- unique(bh$queueid)
    qh <- df[which(df$queueid == queueid), ]
    if (nrow(bh) != 0) {
        historizedBatch(batchid = id, status = status)
        if (unique(bh$endIfKO) == TRUE & status == "KO") {
            # If endIfKO & status = KO then historized the whole queueid (waitBatch will get the information)
            historizedQueue(queueid = queueid, status = "KO")
        }
    } # Else batchid already historized (can happend if already abandonned)
}

releaseQueue <- function(id = NULL) {
    if (is.null(id)) {
        id    <- getQueueid()
    }
    
    # Remove from waitBatch (priority 1)
    # If the queue terminate normally, there should not be any batch in the WaitBatch table anymore. (Because the releaseBatch function historized batchs)
    df <- getWaitBatch()
    bh <- df[which(df$queueid == id), ]
    if (nrow(bh) != 0) {
        historizedBatch(batchid = bh$batchid, status = "abandonned")
    }
    # Write waitQueue (priority 2)
    historizedQueue(queueid = id)
}

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

setRData <- function(file) {
    if (file.exists(file)) {
        file.rename(from = file, to = ".RData")
    } else {
        stop(paste("File", file, "doesn't exist"))
    }
}
