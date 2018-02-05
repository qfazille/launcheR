source("./R/zzz.R")
source("./R/getdata.R")


# create getWaitQueue
df <- data.frame(queueid = numeric()
                 , group = character()
                 , name = character()
                 , wait = numeric()
                 , startDate = character()
                 , realStartDate = character()
                 # no need for realStartDate because when needed, it's time to historized
                 , stringsAsFactors = FALSE)
saveRDS(object = df, file = "./dev/waitqueue.RDS")

# create getWaitBatch
df <- data.frame(batchid = numeric()
                , queueid = numeric()
                , group = character()
                , name = character()
                , parallelizable = logical()
                , wait = numeric()
                , progress = numeric()
                , startDate = character()
                , realStartDate = character()
                , endDate = character()
                , stringsAsFactors = FALSE)
saveRDS(object = df, file = "./dev/waitbatch.RDS")

# create getHistorized.batch
df <- data.frame(queueid = numeric()
                 , batchid = numeric()
                 , group = character()
                 , queuename = character()
                 , batchname = character()
                 , startDate = character()
                 , realStartDate = character()
                 , endDate = character()
                 , stringsAsFactors = FALSE)
saveRDS(object = df, file = "./dev/historizedbatch.RDS")

# create getHistorized.queue
df <- data.frame(queueid = numeric()
                 , group = character()
                 , queuename = character()
                 , startDate = character()
                 , realStartDate = character()
                 , endDate = character()
                 , stringsAsFactors = FALSE)
saveRDS(object = df, file = "./dev/historizedqueue.RDS")

