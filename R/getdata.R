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
        df <- factors2char(df)
        expect_equal(c("queueid", "group", "name", "wait"), colnames(df))
        expect_equal(c("numeric", "character", "character", "numeric"), sapply(df, class))
        saveRDS(object = df, file = file.wait.queue())
    }
    
    write.wait.batch <- function(df) {
        df <- factors2char(df)
        expect_equal(c("batchid", "queueid", "group", "name", "parallelizable", "wait", "progress"), colnames(df))
        expect_true(all(c("numeric", "numeric", "character", "character", "logical", "numeric", "numeric") == sapply(df, class)))
        saveRDS(object = df, file = file.wait.batch())
    }
    
    write.historized <- function(df) {
        df <- factors2char(df)
        expect_equal(c("queueid", "batchid", "group", "queuename", "batchname"), colnames(df))
        expect_true(all(c("numeric", "numeric", "character", "character", "character") == sapply(df, class)))
        saveRDS(object = df, file = file.historized())
    }
    
    set0.wait.batch <- function(id) {
        df <- get.wait.batch()
        df[which(df$batchid == id), "wait"] <- 0
        write.wait.batch(df = df)
    }
    
    set0.wait.queue <- function(id) {
        df <- get.wait.queue()
        df[which(df$queueid == id), "wait"] <- 0
        write.wait.queue(df = df)
    }
    
    add.wait.batch <- function(add) {
        df <- get.wait.batch()
        df <- rbind(df, add)
        write.wait.batch(df = df)
    }
    
    add.wait.queue <- function(add) {
        df <- get.wait.queue()
        df <- rbind(df, add)
        write.wait.queue(df = df)
    }
}


