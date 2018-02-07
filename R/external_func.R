# function to create queue
createQueue <- function(name = basename(tempfile(pattern = "queue", tmpdir = "")), group = NULL, folder = tempdir(), logdir = NULL, clean = TRUE) {
    new(Class = "queue", name = name, group = group, folder = folder, logdir = logdir, clean = clean)
}


# external functions
# to set in class queue
launch <- function(object) {
    # object here is a queue
    
    # check queue has at least one batch
    if (is.null(object@batchs)) stop("Queue doesn't have any batch")
    
    # check if already exists (with TS should not append)
    if (file.exists(object@folder)) stop(paste("Already a folder named", folder))
    
    # create subfolder
    dir.create(object@folder)
    
    # if logdir doesn't exist create it
    if (!file.exists(object@logdir)) dir.create(object@logdir)
    
    # Initialize run.[sh|bat] with first line waitQueue
    runFile <- runInit(queue = object)
    
    # Loop on batch
    #   - Create RData file
    #   - Add in run.[sh|bat] the lines
    for (i in 1:length(object@batchs)) {
        # This method does :
        #   - if params :
        #       - creates RData
        #       - add lines in run.sh of setRdata
        #   - add line waitBatch in run.sh
        runBatch(batch = object@batchs[[i]], runFile = runFile)
    }
    
    # Add releaseQueue
    runReleaseQueue(runFile = runFile)
    
    # Add clean directory
    if (object@clean) {
        cleanFolder(folder = folder, runFile = runFile)
    }
    
    # Launch file in background
    # cmd <- launchFile(runFile = runFile)
    # system(cmd)
}