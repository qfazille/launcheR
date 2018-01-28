# Just for dev
# Then use RSQLite : https://cran.r-project.org/web/packages/RSQLite/vignettes/RSQLite.html
{
    # get filenames
    file.wait.queue <- function() {
        "./dev/waitqueue.RDS"
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
    
    get.wait.batch <- function(with.done = TRUE) {
        df <- readRDS(file = file.wait.batch())
        if (with.done) {
            return(df)
        } else {
            return(df[which(df$batchid >= 0),])
        }
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
}

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
#            wait <- any(id %in% df$queueid)
#        } else {
#            df <- get.wait.batch(with.done = FALSE)
#            wait <- any(id %in% df$batchid)
#        }
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

get.queuename <- function() {
    Sys.getenv("LR_Q")
}

get.queueid <- function() {
    as.numeric(Sys.getenv("LR_QID"))
}

get.group <- function() {
    Sys.getenv("LR_G")
}

get.batch <- function() {
    Sys.getenv("LR_B")
}

get.batchid <- function() {
    as.numeric(Sys.getenv("LR_QBD"))
}
