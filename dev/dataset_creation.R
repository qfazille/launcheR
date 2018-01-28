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

# test de wait.batch
get.wait.batch()

Sys.setenv("LR_Q" = "Q1")
Sys.setenv("LR_QID" = 1)
Sys.setenv("LR_Q" = "A")
queue.name   <- Sys.getenv("LR_Q")
queue.id     <- Sys.getenv("LR_QID")
queue.group  <- Sys.getenv("LR_G")
batch.name   <- Sys.getenv("LR_B")
batch.par    <- Sys.getenv("LR_BPAR")

wait.batch()

