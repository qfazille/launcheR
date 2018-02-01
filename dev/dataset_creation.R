source("./R/zzz.R")
source("./R/getdata.R")


# create get.wait.queue
df <- data.frame(queueid = integer()
                 , group = character()
                 , name = character()
                 , wait = integer()
                 , startDate = character()
                 , realStartDate = character()
                 # no need for realStartDate because when needed, it's time to historized
                 , stringsAsFactors = FALSE)
saveRDS(object = df, file = "./dev/waitqueue.RDS")

# create get.wait.batch
df <- data.frame(batchid = integer()
                , queueid = integer()
                , group = character()
                , name = character()
                , parallelizable = logical()
                , wait = integer()
                , progress = integer()
                , startDate = character()
                , realStartDate = character()
                , endDate = character()
                , stringsAsFactors = FALSE)
saveRDS(object = df, file = "./dev/waitbatch.RDS")

# create get.historized.batch
df <- data.frame(queueid = integer()
                 , batchid = integer()
                 , group = character()
                 , queuename = character()
                 , batchname = character()
                 , startDate = character()
                 , realStartDate = character()
                 , endDate = character()
                 , stringsAsFactors = FALSE)
saveRDS(object = df, file = "./dev/historizedbatch.RDS")

# create get.historized.queue
df <- data.frame(queueid = integer()
                 , group = character()
                 , queuename = character()
                 , startDate = character()
                 , realStartDate = character()
                 , endDate = character()
                 , stringsAsFactors = FALSE)
saveRDS(object = df, file = "./dev/historizedqueue.RDS")

