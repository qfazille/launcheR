#wait.for <- function(type = c("queue", "batch"), id) {
#    if (id == 0) {
#        wait <- FALSE
#    } else {
#        wait <- TRUE
#    }
#    while(wait) {
#        Sys.sleep(10)
#        if (type == "queue") {
#            df <- get.wait.queue()
#            wait <- any(id %in% df$queueid)
#        } else {
#            df <- get.wait.batch(with.done = FALSE)
#            wait <- any(id %in% df$batchid)
#        }
#    }
#}

# for dev
wait.for <- function(type = c("queue", "batch"), id) {
    message(paste("Wait for", type, paste(id, collapse = ",")))
    return(TRUE)
}

wait.for.batchid <- function(id) {
    wait.for(type = "batch", id = id)
}

wait.for.queueid <- function(id) {
    wait.for(type = "queue", id = id)
}
