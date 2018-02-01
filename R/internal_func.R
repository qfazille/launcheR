setRData <- function(file) {
    if (file %in% list.files()) {
        file.rename(from = file, to = ".RData")
    } else {
        stop(paste("File", file, "doesn't exist"))
    }
}

wait.queue <- function(){
    queue.name  <- get.queuename()
    group <- get.group()

    # Get data
    df <- get.wait.queue()

    # Write new queueid
    newid <- newid.queue()
    write.Renviron(prefix = "LR_QID", value = newid)

    # if no group, then no check
    if (is.null(group)) {
        add.wait.queue(queueid = newid, group = NA, name = queue.name, wait = 0)
        # queue can be launched
    } else {
        # Check if group not present
        if (length(df[which(df$group == group),"id"]) == 0) {
            add.wait.queue(queueid = newid, group = group, name = queue.name)
            # queue can be launched
        } else {
            # if group already present then need to wait for max(id)
            wait_for_id <- max(df[which(df$group == group),"queueid"])
            # add in waiting queue
            add.wait.queue(queueid = newid, group = group, name = queue.name, wait = wait_for_id)
            # wait before launch queue
            wait.for.queueid(id = wait_for_id)
            #launch.wait.queue(id = newid) # this set wait = 0 (commented for dev)
            # launch
        }
    }
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
        add.wait.batch(batchid = newid
            , queueid = queue.id
            , group = queue.group
            , namae = batch.name
            , parallelizable = batch.par
            , wait = 0
            , progress = 0
            , startDate = get.date()
            , realStartDate = get.date())
        # launch
    } else {
        df <- df[which(df$name == batch.name),]
        id_wait_max <- df[which(df$wait == max(df$wait)), "batchid"]
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
        # add to waiting batch
        add.wait.batch(batchid = newid
            , queueid = queue.id
            , group = queue.group
            , namae = batch.name
            , parallelizable = batch.par
            , wait = to_wait
            , progress = 0
            , startDate = get.date())
        # wait before launch
        wait.for.batchid(id = to_wait)
        #launch.wait.batch(id = newid) # this set wait = 0 (commented for dev)
        # launch
    }
}

release.batch <- function() {
    batch.id <- get.batchid()
    df <- get.wait.batch()
    # Set wait to -1 (means batch not running anymore but queue still running)
    df[which(df$batchid == batch.id), "wait"]       <- -1
    df[which(df$batchid == batch.id), "progress"]   <- 100
    df[which(df$batchid == batch.id), "endDate"]    <- get.date()
    write.wait.batch(df = df)
}

release.queue <- function() {
    queue.id    <- get.queueid()
    queue.name  <- get.queuename()
    # Write wait.batch (priority 1)
    df <- get.wait.batch()
    bh <- df[which(df$queueid == queue.id), ]
    expect_gt(nrow(bh), 0)
    df <- df[-which(df$queueid == queue.id), ]
    write.wait.batch(df = df)
    # Write wait.queue (priority 2)
    df <- get.wait.queue()
    qh <- df[which(df$queueid == queue.id), ]
    expect_equal(nrow(qh), 1)
    df <- df[-which(df$queueid == queue.id), ]
    write.wait.queue(df = df)
    # Write historized.batch (priority 3)
    add.historized.batch(queueid = queue.id, batchid = bh$batchid, group = bh$group, queuename = queue.name, batchname = bh$name, startDate = bh$startDate, realStartDate = bh$realStartDate)
    # Write historized queue  (priority 3)
    add.historized.queue(queueid = queue.id, group = qh$group, queuename = qh$name, startDate = qh$startDate, realStartDate = qh$realStartDate)
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
