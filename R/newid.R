newid.queue <- function() {
    df <- get.wait.queue()
    if (nrow(df) > 0) {
        return(max(df$queueid) + 1)
    } else {
        df <- get.historized()
        if (nrow(df) > 0) {
            return(max(df$queueid) + 1)
        } else {
            return(1)
        }
    }
}

newid.batch <- function() {
    df <- get.wait.batch()
    if (nrow(df) > 0) {
        return(max(df$batchid) + 1)
    } else {
        df <- get.historized()
        if (nrow(df) > 0) {
            return(max(df$batchid) + 1)
        } else {
            return(1)
        }
    }
}

