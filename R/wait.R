waitFor <- function(type = c("queue", "batch"), id) {
    if (id == 0) {
        wait <- FALSE
    } else {
        wait <- TRUE
    }
    
    while(wait) {
        if (type == "queue") {
            df <- getWaitQueue()
            wait <- any(id %in% df$queueid)
        } else {
            df <- getWaitBatch(with.done = FALSE)
            wait <- any(id %in% df$batchid)
        }
        if (wait) Sys.sleep(2)
    }
}

waitForBatchid <- function(id) {
    waitFor(type = "batch", id = id)
}

waitForQueueid <- function(id) {
    waitFor(type = "queue", id = id)
}
