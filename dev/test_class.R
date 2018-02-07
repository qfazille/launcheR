source("./R/utils.R")
source("./dev/batch.R")
source("./dev/queue.R")

q <- new(Class = "queue")
q <- new(Class = "queue", name = "firstQ")
q <- new(Class = "queue", folder = "/home/qfazilleau/test", clean = FALSE)
q <- new(Class = "queue", folder = "/home/qfazilleau/test", logdir = "/tmp/log", clean = FALSE)
q <- new(Class = "queue", group = "demo")

b <- new(Class = "batch", Rank = 1, logfile = "/tmp/logfile.log", path = "/home/qfazilleau/github/launcheR/dev/tests/batch1.R")

q <- addBatch(object = q, path = "./dev/tests/batch1.R")
q <- addBatch(object = q, path = "./dev/tests/batch2.R")
