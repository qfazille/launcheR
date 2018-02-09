waitFor <- function(type = c("queue", "batch"), id) {
    if (id == 0) {
        wait <- FALSE
    } else {
        wait <- TRUE
    }
    
    while(wait) {
        Sys.sleep(10)
        if (type == "queue") {
            df <- getWaitQueue()
            wait <- any(id %in% df$queueid)
        } else {
            df <- getWaitBatch(with.done = FALSE)
            wait <- any(id %in% df$batchid)
        }
    }
}

waitForBatchid <- function(id) {
    waitFor(type = "batch", id = id)
}

waitForQueueid <- function(id) {
    waitFor(type = "queue", id = id)
}
