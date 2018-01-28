setRData <- function(file) {
    if (file %in% list.files()) {
        file.rename(from = file, to = ".RData")
    } else {
        stop(paste("File", file, "doesn't exist"))
    }
}

setRenviron <- function(queue, batch) {
    if (is.null(queue) | is.null(batch)) stop("Both queue & batch must be filled")
    LR_Q <- paste0("LR_Q=", queue)
    LR_B <- paste0("LR_B=", batch)
    cat(LR_Q, LR_B, file = ".Renviron", append = FALSE, sep = "\n")
}

wait.queue <- function(){
    queue.name  <- Sys.getenv("LR_Q")
    queue.group <- Sys.getenv("LR_G")
    
    if (is.null(queue.name))   stop("environment variable LR_Q cannot be null")
    if (is.null(queue.group))  stop("environment variable LR_G cannot be null")
    
    # Get data
    df <- get.wait.group()
    
    newid <- newid.queue()
    
    # if no group, then no check
    if (is.null(group)) {
        toInsert <- c(queueid = newid, group = NA, name = queue.name, wait = 0)
        df <- rbind(df, toInsert)
        write.wait.group(df = df)
        # queue can be launched
    } else {
        # Check if group not present
        if (length(df[which(df$group == group),"id"]) == 0) {
            toInsert <- c(queueid = newid, group = group, name = name, wait = 0)
            df <- rbind(df, toInsert)
            write.wait.group(df = df)
            # queue can be launched
        } else {
            # if group already present then need to wait for max(id)
            wait_for_id <- max(df[which(df$group == group),"queueid"])
            toInsert <- c(queueid = newid, group = group, name = queue.name, wait = wait_for_id)
            df <- rbind(df, toInsert)
            write.wait.group(df = df)
            wait.for.queueid(id = wait_for_id)
            # Set wait = 0
            df <- get.wait.queue()
            df[which(df$queueid == newid), "wait"] <- 0
            write.wait.queue(df = df)
            # launch
        }
    }
    return(TRUE)
}

wait.batch <- function(parallelizable = TRUE) {
    queue.name   <- Sys.getenv("LR_Q")
    queue.id     <- Sys.getenv("LR_QID")
    queue.group  <- Sys.getenv("LR_G")
    batch.name   <- Sys.getenv("LR_B")
    batch.par    <- Sys.getenv("LR_BPAR")
    
    if (queue.name == "")    stop("environment variable LR_Q cannot be null")
    if (queue.id == "")      stop("environment variable LR_QID cannot be null")
    if (queue.group == "")   stop("environment variable LR_G cannot be null")
    if (batch.name == "")    stop("environment variable LR_B cannot be null")
    if (batch.par == "")     stop("environment variable LR_BPAR cannot be null")
    
    # Get data
    df <- get.wait.batch()

    # If batch not present then insert and launch
    newid <- newid.batch()
    if (length(which(df$batch == batch.name)) == 0) {
        toInsert <- data.frame(queueid = queue.id, batchid = newid, batch = batch.name, parallelizable = batch.par, wait = 0, progress = 0)
        df <- rbind(df, toInsert)
        write.wait.batch(df = df)
        # launch
    } else {
        id_wait_max <- df[which(df$wait == max(df$wait) & df$batch == batch.name), "batchid"]
        if (batch.par) {
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
        # batchid to wait is to_wait
        toInsert <- data.frame(queueid = queue.id, batchid = newid, batch = batch.name, parallelizable = batch.par, wait = to_wait, progress = 0)
        df <- rbind(df, toInsert)
        write.wait.batch(df = df)
        wait.for.batchid(id = to_wait)
        # Set wait = 0
        df <- get.wait.batch()
        df[which(df$batchid == newid), "wait"] <- 0
        write.wait.batch(df = df)
        # launch
    }
    return(TRUE)
}

release.batch <- function(name) {
    if (is.null(name)) stop("name cannot be NULL in release.batch function")
    
}

release.queue <- function(name) {
    if (is.null(name)) stop("name cannot be NULL in release.queue function")
    
    # Get data
    df <- get.wait.group()
    
    if (!name %in% df$queue) stop(paste("queue", name, "doesn't exists"))
    
    