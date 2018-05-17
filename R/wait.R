waitFor <- function(type = c("queue", "batch"), waiter_id, waited_ids) {
    if (length(waited_ids) == 0 || waited_ids == 0) {
        wait <- FALSE
    } else {
        wait <- TRUE
    }
    
    while(wait) {
        if (type == "queue") {
            # Check for waited_ids
            df <- getWaitQueue()
            wait <- any(waited_ids %in% df$queueid)
            # Check if waiter_id abandonned
            if (!waiter_id %in% df$queueid) stop(paste("Queueid", waiter_id, "already abandonned"))
        } else {
            # Check for waited_ids
            df <- getWaitBatch()
            wait <- any(waited_ids %in% df$batchid)
            # Check if waiter_id abandonned
            if (!waiter_id %in% df$batchid) stop(paste("Batchid", waiter_id, "already abandonned"))
        }
        if (wait) Sys.sleep(2)
    }
}

waitForBatchid <- function(waiter_id, waited_ids) {
    waitFor(type = "batch", waiter_id = waiter_id, waited_ids = waited_ids)
}

waitForQueueid <- function(waiter_id, waited_ids) {
    waitFor(type = "queue", waiter_id = waiter_id, waited_ids = waited_ids)
}
