library(launcheR)

getWaitBatch()

# 1st batch
Sys.setenv("LR_Q" = "Q1")
Sys.setenv("LR_QID" = "1")
Sys.setenv("LR_G" = "A")
Sys.setenv("LR_B" = "B1")
Sys.setenv("LR_BPAR" = "TRUE")
waitBatch()
getWaitBatch()

# 2nd batch
Sys.setenv("LR_B" = "B1")
Sys.setenv("LR_BPAR" = "FALSE")
waitBatch()
getWaitBatch()

# etc...
Sys.setenv("LR_B" = "B1")
Sys.setenv("LR_BPAR" = "FALSE")
waitBatch()
getWaitBatch()

df <- getWaitBatch()
df2 <- df %>% group_by(batchid, queueid, group, name, parallelizable, progress) %>% summarise(wait_c = paste(wait, collapse = ",")) %>% ungroup() %>% select(batchid, name, wait = wait_c)
df2[] <- lapply(df2, as.character)

status <- sapply(df2$wait, function(x) if(x == "0") "in progress" else "waiting")
df2$status <- status
