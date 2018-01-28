# get filenames
file.wait.queue <- function() {
    "./dev/waitgroup.RDS"
}

file.wait.batch <- function() {
    "./dev/waitbatch.RDS"
}

file.historized <- function() {
    "./dev/historized.RDS"
}

# read files
get.wait.queue <- function() {
    readRDS(file = file.wait.queue())
}

get.wait.batch <- function() {
    readRDS(file = file.wait.batch())
}

get.historized <- function() {
    readRDS(file = file.historized())
}

# write files
write.wait.queue <- function(df) {
    if (any(!c("id", "group", "queue", "wait_id") %in% colnames(df))) stop("df doesn't match the structure")
    saveRDS(object = df, file = file.wait.queue())
}

write.wait.batch <- function(df) {
    #if (any(!c("id", "") %in% colnames(df))) stop("df doesn't match the structure")
    saveRDS(object = df, file = file.wait.batch())
}

write.historized <- function(df) {
    if (any(!c("id", "") %in% colnames(df))) stop("df doesn't match the structure")
    saveRDS(object = df, file = file.historized())
}

newid <- function(type = c("queue", "batch")) {
    if (!type %in% c("queue", "batch")) stop("function newid must have type to queue or batch")
    # try on waiting
    if (type == "batch") {
        df <- get.wait.batch()
    } else {
        df <- get.wait.queue()
    }
    if (nrow(df) > 0) {
        return(max(df$id) + 1)
    } else {
        # try on running
        df <- get.historized()
        if (nrow(df) > 0) {
            if (type == "batch") {
                return(max(df$batch_id) + 1)
            } else {
                return(max(df$queue_id) + 1)
            }
        } else {
            return(1)
        }
    }
}

newid.queue <- function() {
    newid(type = "queue")
}

newid.batch <- function() {
    newid(type = "batch")
}

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
#        } else {
#            df <- get.wait.queue()
#        }
#        wait <- any(id %in% df$id)
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

