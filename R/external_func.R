# external functions

launch <- function(queue) {
    # get queue name
    
    # Set .Renviron file in path folder
    # need path & queuename
    file.create(file.path(path, ".Renviron"))
    write.Renviron(prefix = "LR_Q", value = queuename)

}