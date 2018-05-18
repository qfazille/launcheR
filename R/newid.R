newidQueue <- function() {
    wq <- getWaitQueue()
    hq <- getHistorizedQueue()
    all_queueid <- c(wq$queueid, hq$queueid)
    if (length(all_queueid) > 0) {
        return(max(all_queueid, na.rm = TRUE) + 1)
    } else {
        return(1)
    }
}

newidBatch <- function() {
    wb <- getWaitBatch()
    hb <- getHistorizedBatch()
    all_batchid <- c(wb$batchid, hb$batchid)
    if (length(all_batchid) > 0) {
        return(max(all_batchid, na.rm = TRUE) + 1)
    } else {
        return(1)
    }
}

