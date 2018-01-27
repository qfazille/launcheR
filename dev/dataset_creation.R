source("R/zzz.R")
# Create dataset for dev
groups <- c(rep("A",3), rep("B", 4), rep("C", 2))
queue <- paste0(groups, 1:9)
df <- data.frame(id = 1:9, group = groups, queue = queue, wait_for_id = c(0,1,2,0,4,5,6,0,8))
df <- setNumOrFact(df)
df <- factors2char(df)
saveRDS(object = df, file = "./dev/waitgroup.RDS")

df <- data.frame(id = integer(), batch = character(), parallelizable = logical(), wait = integer(), progress = integer())
df$batch <- as.character(df$batch)
str(df)
saveRDS(object = df, file = "./dev/waitbatch.RDS")

# test de wait.batch
source("./R/getdata.R")

get.wait.batch()

wait.batch(name = "Z1", parallelizable = TRUE)
wait.batch(name = "Z1", parallelizable = FALSE)
wait.batch(name = "Z1", parallelizable = TRUE)
wait.batch(name = "Z1", parallelizable = TRUE)
wait.batch(name = "Z1", parallelizable = F)
wait.batch(name = "Z1", parallelizable = F)
wait.batch(name = "Z1", parallelizable = T)
wait.batch(name = "Z1", parallelizable = T)
wait.batch(name = "Z1", parallelizable = T)
wait.batch(name = "Z1", parallelizable = F)
wait.batch(name = "Z1", parallelizable = F)

get.wait.batch()
