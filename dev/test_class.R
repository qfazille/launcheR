source("./R/utils.R")
source("./R/getdata.R")
source("./R/getenv.R")
source("./R/internal_func.R")
source("./R/external_func.R")
source("./R/newid.R")
source("./R/wait.R")
source("./R/writeRun.R")
source("./R/batch.R")
source("./R/queue.R")

setwd("/home/qfazilleau/github/launcheR")

q <- createQueue(folder = ".")

q <- addBatch(object = q, path = "./dev/tests/batch1.R")
q <- addBatch(object = q, path = "./dev/tests/batch2.R")
launch(q)
launcheR:::cleanQ(q)

launcheR:::getWaitQueue()
launcheR:::getWaitBatch()


launcheR:::waitQueue(queue_name="queue1b4d66b258b70")
launcheR:::waitBatch(batch_name="batch1", batch_par="TRUE", batch_rank="1")


