# external functions
# to set in class queue
launch <- function(object) {
    # object here is a queue
    
    # check queue has at least one batch
    if (is.null(object@batchs)) stop("Queue doesn't have any batch")
    
    # check if already exists (with TS should not appear)
    folder <- file.path(object@folder, paste0(object@name, getTS()))
    if (file.exists(folder)) stop(paste("Already a folder named", folder))
    
    # create folder
    dir.create(folder)
    
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

}