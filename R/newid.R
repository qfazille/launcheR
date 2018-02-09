newidQueue <- function() {
    df <- getWaitQueue()
    if (nrow(df) > 0) {
        return(max(df$queueid) + 1)
    } else {
        df <- getHistorizedBatch()
        if (nrow(df) > 0) {
            return(max(df$queueid) + 1)
        } else {
            return(1)
        }
    }
}

newidBatch <- function() {
    df <- getWaitBatch()
    if (nrow(df) > 0) {
        return(max(df$batchid) + 1)
    } else {
        df <- getHistorizedBatch()
        if (nrow(df) > 0) {
            return(max(df$batchid) + 1)
        } else {
            return(1)
        }
    }
}

