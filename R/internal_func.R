setRData <- function(file) {
    if (file %in% list.files()) {
        file.rename(from = file, to = ".RData")
    } else {
        stop(paste("File", file, "doesn't exist"))
    }
}

wait.queue <- function(){
    queue.name  <- get.queuename()
    queue.group <- get.group()
    
    if (is.null(queue.name))   stop("environment variable LR_Q cannot be null")
    if (is.null(queue.group))  stop("environment variable LR_G cannot be null")
    
    # Get data
    df <- get.wait.group()
    
    # Write new queueid
    newid <- newid.queue()
    write.Renviron(prefix = "LR_QID", value = newid)
    
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

wait.batch <- function() {
    
    queue.name  <- get.queuename()
    queue.id    <- get.queueid()
    queue.group <- get.group()
    batch.name  <- get.batchname()
    batch.par   <- get.batchpar()
    
    # Get data
    df <- get.wait.batch()

    # Write new batchid
    newid <- newid.batch()
    write.Renviron(prefix = "LR_BID", value = newid)
    
    if (length(which(df$name == batch.name)) == 0) {
        toInsert <- data.frame(batchid = newid, queueid = queue.id, group = queue.group, name = batch.name, parallelizable = batch.par, wait = 0, progress = 0)
        add.wait.batch(add = toInsert)
        # launch
    } else {
        id_wait_max <- df[which(df$wait == max(df$wait) & df$name == batch.name), "batchid"]
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
        toInsert <- data.frame(batchid = newid, queueid = queue.id, group = queue.group, name = batch.name, parallelizable = batch.par, wait = to_wait, progress = 0)
        add.wait.batch(add = toInsert)
        wait.for.batchid(id = to_wait)
        #set0.wait.batch(id = newid)
        # launch
    }
    return(TRUE)
}

release.batch <- function(name) {
    if (is.null(name)) stop("name cannot be NULL in release.batch function")
    return(TRUE)
}

release.queue <- function(name) {
    if (is.null(name)) stop("name cannot be NULL in release.queue function")
    
    # Get data
    df <- get.wait.group()
    
    if (!name %in% df$queue) stop(paste("queue", name, "doesn't exists"))
    
    return(TRUE)
}

write.Renviron <- function(prefix, value) {
    x <- readLines(".Renviron")
    toChange <- which(startsWith(x, paste0(prefix, "=")))
    if (length(toChange) == 0) {
        x <- c(x, paste0(prefix, "='", value, "'"))
    } else {
        x[toChange] <- paste0(prefix, "='", value, "'")
    }
    cat(x, file = ".Renviron", sep = linebreak())
}