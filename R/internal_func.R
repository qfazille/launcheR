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
    # Get data
    df <- get.wait.group()

    # if no group, then no check
    if (is.null(group)) {
        toInsert <- c(id = maxid(df), group = NA, queue = name, wait_for_id = 0)
        df <- rbind(df, toInsert)
        write.wait.group(df = df)
    } else {
        # Check if group not present
        if (length(df[which(df$group == group),"id"]) == 0) {
            toInsert <- c(id = maxid(df), group = group, queue = name, wait_for_id = 0)
            df <- rbind(df, toInsert)
            write.wait.group(df = df)
        } else {
            # if group already present then need to wait for max(id)
            wait_for_id <- max(df[which(df$group == group),"id"])
            toInsert <- c(id = maxid(df), group = group, queue = name, wait_for_id = wait_for_id)
            df <- rbind(df, toInsert)
            write.wait.group(df = df)
            
        }
    }
}
