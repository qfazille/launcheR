randomName <- function(prefix="") paste0(prefix, substr(basename(tempfile()), 8, 13))

getQID <- function(name) {
    wq <- getWaitQueue()
    id <- wq[which(wq$queuename == name), "queueid"]
    if (length(id) == 0) {
        return(NULL)
    } else {
        return(id[1])
    }
}

getBID <- function(name) {
    wb <- getWaitBatch()
    id <- wb[which(wb$batchname == name), "batchid"]
    if (length(id) == 0) {
        return(NULL)
    } else {
        return(id[1])
    }
}

checkWQ <- function(id, name = NULL, group = NULL, owner = NULL, wait = NULL, desc = NULL) {
    wq <- getWaitQueue()
    wq <- wq[wq$queueid == id,]
    if (nrow(wq) == 0) return(FALSE)
    if (!is.null(name)) if (!name %in% wq$queuename) return(FALSE)
    if (!is.null(group)) if (!group %in% wq$group) return(FALSE)
    if (!is.null(owner)) if (!owner %in% wq$owner) return(FALSE)
    if (!is.null(wait)) if (!wait %in% wq$wait) return(FALSE)
    if (!is.null(desc)) if (!desc %in% wq$desc) return(FALSE)
    return(TRUE)
}

checkWB <- function(id, name = NULL, group = NULL, path = NULL, parallelizable = NULL, wait = NULL, endIfKO = NULL, desc = NULL) {
    wb <- getWaitBatch()
    wb <- wb[wb$batchid ==id,]
    if (nrow(wb) == 0) return(FALSE)
    if (!is.null(name)) if (!name %in% wb$batchname) return(FALSE)
    if (!is.null(group)) if (!group %in% wb$group) return(FALSE)
    if (!is.null(path)) if (!path %in% wb$path) return(FALSE)
    if (!is.null(parallelizable)) if (!parallelizable %in% wb$parallelizable) return(FALSE)
    if (!is.null(wait)) if (!wait %in% wb$wait) return(FALSE)
    if (!is.null(endIfKO)) if (!endIfKO %in% wb$endIfKO) return(FALSE)
    if (!is.null(desc)) if (!desc %in% wb$desc) return(FALSE)
    return(TRUE)
}

