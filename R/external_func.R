# external functions

launch <- function(queue) {
    # get queue name
    path <- "coucou"
    queuename <- "hello"
    # Set .Renviron file in path folder
    # need path & queuename
    file.create(file.path(path, ".Renviron"))
    writeRenviron(prefix = "LR_Q", value = queuename)

}