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


# dev ongoing
name <- "B10"
group <- "B"

wait.queue <- function(name, group = NULL){
    if (is.null(name)) stop("name cannot be NULL in wait.queue function")
    
    # Get data
    df <- get.wait.group()

    # if no group, then no check
    if (is.null(group)) {
        toInsert <- c(id = newid.queue(), group = NA, queue = name, wait_for_id = 0)
        df <- rbind(df, toInsert)
        write.wait.group(df = df)
        # queue can be launched
    } else {
        # Check if group not present
        if (length(df[which(df$group == group),"id"]) == 0) {
            toInsert <- c(id = newid.queue(), group = group, queue = name, wait_for_id = 0)
            df <- rbind(df, toInsert)
            write.wait.group(df = df)
            # queue can be launched
        } else {
            # if group already present then need to wait for max(id)
            wait_for_id <- max(df[which(df$group == group),"id"])
            toInsert <- c(id = newid.queue(), group = group, queue = name, wait_for_id = wait_for_id)
            df <- rbind(df, toInsert)
            write.wait.group(df = df)
            # Wait for id not to exist anymore
            wait <- TRUE
            while(wait) {
                Sys.sleep(10)
                df <- get.wait.group()
                wait <- wait_for_id %in% df$id
            }
            # queue can be launched
        }
    }
}
    
wait.batch <- function(name, parallelizable = TRUE) {
    if (is.null(name)) stop("name cannot be NULL in wait.batch function")

    # Get data
    df <- get.wait.batch()

    # If batch not present then insert and launch
    newid <- newid.batch()
    if (length(which(df$batch == name)) == 0) {
        toInsert <- data.frame(id = newid, batch = name, parallelizable = parallelizable, wait = 0, progress = 0)
        df <- rbind(df, toInsert)
        write.wait.batch(df = df)
        # launch
    } else {
        id_wait_max <- df[which(df$wait == max(df$wait) & df$batch == name), "id"]
        if (parallelizable) {
            id_wait_max_par <- df[which(df$id %in% id_wait_max), "parallelizable"]
            # Either one id with FALSE or one or several id with par = TRUE
            if (all(id_wait_max_par)) {
                to_wait <- unique(df[which(df$id %in% id_wait_max), "wait"]) # should be one value
            } else {
                to_wait <- id_wait_max
            }
        } else {
            to_wait <- id_wait_max
        }
        # Id to wait is to_wait
        toInsert <- data.frame(id = newid, batch = name, parallelizable = parallelizable, wait = to_wait, progress = 0)
        df <- rbind(df, toInsert)
        write.wait.batch(df = df)
        wait.for.batchid(id = to_wait)
        # launch
    }
    return(TRUE)
}


release.queue <- function(name) {
    if (is.null(name)) stop("name cannot be NULL in wait.queue function")
    
    # Get data
    df <- get.wait.group()
    
    if (!name %in% df$queue) stop(paste("queue", name, "doesn't exists"))
    
    