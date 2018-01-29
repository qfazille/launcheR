# test de wait.batch
source("./R/utils.R")
source("./R/getdata.R")
source("./R/getenv.R")
source("./R/internal_func.R") #Je commente juste set0.wait.batch(id = newid)

get.wait.batch()

# 1st batch
Sys.setenv("LR_Q" = "Q1")
Sys.setenv("LR_QID" = "1")
Sys.setenv("LR_G" = "A")
Sys.setenv("LR_B" = "B1")
Sys.setenv("LR_BPAR" = "TRUE")
wait.batch()
get.wait.batch()

# 2nd batch
Sys.setenv("LR_BPAR" = "FALSE")
wait.batch()
get.wait.batch()

# etc...
Sys.setenv("LR_BPAR" = "TRUE")
wait.batch()
get.wait.batch()
