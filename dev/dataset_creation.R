source("./R/zzz.R")
source("./R/getdata.R")


# create get.wait.queue
df <- data.frame(queueid = integer()
                 , group = character()
                 , name = character()
                 , wait = integer()
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
                , stringsAsFactors = FALSE)
saveRDS(object = df, file = "./dev/waitbatch.RDS")

# create get.historized
df <- data.frame(queueid = integer()
                 , batchid = integer()
                 , group = character()
                 , queuename = character()
                 , batchname = character()
                 , stringsAsFactors = FALSE)
saveRDS(object = df, file = "./dev/historized.RDS")

