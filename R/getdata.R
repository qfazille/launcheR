# get filenames
file.wait.group <- function() {
    "./dev/waitgroup.RDS"
}

file.wait.batchs <- function() {
    "./dev/waitbatchs.RDS"
}

file.historized <- function() {
    "./dev/historized.RDS"
}

file.running <- function() {
    "./dev/running.RDS"
}


# read files
get.wait.group <- function() {
    readRDS(file = file.wait.group())
}

get.wait.batchs <- function() {
    readRDS(file = file.wait.batchs())
}

get.historized <- function() {
    readRDS(file = file.historized())
}

get.running <- function() {
    readRDS(file = file.running())
}

# write files
write.wait.group <- function(df) {
    if (any(!c("id", "group", "queue", "wait_for_id") %in% colnames(df))) stop("df doesn't match the structure")
    saveRDS(object = df, file = file.wait.group())
}

write.wait.batchs <- function(df) {
    if (any(!c("id", "") %in% colnames(df))) stop("df doesn't match the structure")
    saveRDS(object = df, file = file.wait.batchs())
}

write.historized <- function(df) {
    if (any(!c("id", "") %in% colnames(df))) stop("df doesn't match the structure")
    saveRDS(object = df, file = file.historized())
}

write.running <- function() {
    if (any(!c("id", "") %in% colnames(df))) stop("df doesn't match the structure")
    saveRDS(object = df, file = file.running())
}

# get maximum id through waiting, running or historized data
maxid <- function() {
    # try on waiting
    df <- get.wait.file()
    if (nrow(df) > 0) {
        maxid <- max(df$id) + 1
    } else {
        # try on running
        df <- get.running()
        if (nrow(df) > 0) {
            maxid <- max(df$id) + 1
        } else {
            # try on historized
            df <- get.historized()
            if (nrow(df) > 0) {
                maxid <- max(df$id) + 1
            } else {
                maxid <- 1
            }
        }
    }
}