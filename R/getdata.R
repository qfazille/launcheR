# Just for dev
# Then use RSQLite : https://cran.r-project.org/web/packages/RSQLite/vignettes/RSQLite.html
{
    # get filenames
    {
        file.wait.queue <- function() {
            "./dev/waitqueue.RDS"
        }
        
        file.wait.batch <- function() {
            "./dev/waitbatch.RDS"
        }
        
        file.historized.batch <- function() {
            "./dev/historizedbatch.RDS"
        }
        
        file.historized.queue <- function() {
            "./dev/historizedqueue.RDS"
        }
    }
    
    # read files
    {
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
        
        get.historized.batch <- function() {
            readRDS(file = file.historized.batch())
        }
        
        get.historized.queue <- function() {
            readRDS(file = file.historized.queue())
        }
    }
    
    # write files
    {
        write.wait.queue <- function(df) {
            df <- factors2char(df)
            expect_equal(sapply(df, class), sapply(get.wait.queue(), class))
            saveRDS(object = df, file = file.wait.queue())
        }
        
        write.wait.batch <- function(df) {
            df <- factors2char(df)
            expect_equal(sapply(df, class), sapply(get.wait.batch(), class))
            saveRDS(object = df, file = file.wait.batch())
        }
        
        write.historized.batch <- function(df) {
            df <- factors2char(df)
            expect_equal(sapply(df, class), sapply(get.historized.batch(), class))
            saveRDS(object = df, file = file.historized.batch())
        }
        
        write.historized.queue <- function(df) {
            df <- factors2char(df)
            expect_equal(sapply(df, class), sapply(get.historized.queue(), class))
            saveRDS(object = df, file = file.historized.queue())
        }
    }
}

# Launch batch or queue
{
    launch.wait.batch <- function(id) {
        df <- get.wait.batch()
        df[which(df$batchid == id), "wait"] <- 0
        df[which(df$batchid == id), "realStartDate"] <- get.date()
        write.wait.batch(df = df)
    }
    
    launch.wait.queue <- function(id) {
        df <- get.wait.queue()
        df[which(df$queueid == id), "wait"] <- 0
        df[which(df$queueid == id), "realStartDate"] <- get.date()
        write.wait.queue(df = df)
    }
}

# Add in files
{
    add.wait.batch <- function(batchid, queueid, group = NULL, name, parallelizable, wait = 0, progress = 0, startDate = get.date(), realStartDate = get.date(), endDate = NULL) {
        stopifnot(!any(unlist(lapply(list(batchid, queueid, name, parallelizable, wait, progress), is.null))))
        toInsert <- data.frame(batchid = batchid
                        , queueid = queueid
                        , group = group
                        , name = name
                        , parallelizable = parallelizable
                        , wait = wait
                        , progress = progress
                        , startDate = startDate
                        , realStartDate = realStartDate
                        , endDate = endDate
                        , stringsAsFactors = FALSE)
        df <- get.wait.batch()
        df <- rbind(df, toInsert)
        write.wait.batch(df = df)
    }
    
    add.wait.queue <- function(queueid, group, name, wait = 0, startDate = get.date(), realStartDate = get.date()) {
        stopifnot(!any(unlist(lapply(list(queueid, name), is.null))))
        toInsert <- data.frame(queueid = queueid
                        , group = group
                        , name = name
                        , wait = wait
                        , startDate = startDate
                        , realStartDate = realStartDate
                        , stringsAsFactors = FALSE)
        expect_equal(sapply(toInsert, class), c("numeric", "character", "character", "numeric"))
        df <- get.wait.queue()
        df <- rbind(df, toInsert)
        write.wait.queue(df = df)
    }
    
    add.historized.batch <- function(queueid, batchid, group, queuename, batchname, startDate, realStartDate, endDate = get.date()) {
        stopifnot(!any(unlist(lapply(list(queueid, batchid, queuename, batchname, startDate, realStartDate, endDate)))))
        toInsert <- data.frame(queueid = queueid
                        , batchid = batchid
                        , group = group
                        , queuename = queuename
                        , batchname = batchname
                        , startDate = startDate
                        , endDate = endDate
                        , stringsAsFactors = FALSE)
        expect_equal(sapply(toInsert, class), c("numeric", "numeric", "character", "character", "character", "character", "character", "character"))
        df <- get.historized.batch()
        df <- rbind(df, toInsert)
        write.historized.batch(df = df)
    }
    
    add.historized.queue <- function(queueid, group, queuename, startDate, realStartDate, endDate = get.date()) {
        stopifnot(!any(unlist(lapply(list(queueid, queuename, startDate, realStartDate, endDate)))))
        toInsert <- data.frame(queueid = queueid
                        , group = group
                        , queuename = queuename
                        , startDate = startDate
                        , realStartDate = realStartDate
                        , endDate = endDate
                        , stringsAsFactors = FALSE)
        expect_equal(sapply(toInsert, class), c("numeric", "numeric", "character", "character", "character", "character", "character", "character"))
        df <- get.historized.queue()
        df <- rbind(df, toInsert)
        write.historized.queue(df = df)
    }

}


