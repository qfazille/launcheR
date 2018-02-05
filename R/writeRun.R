# all function that write the run file
# in class queue
runInit <- function(object) {
    # Get info from queue object
    folder      <- object@folder
    queue_name  <- object@name
    group       <- object@group
    
    # filepath to run file
    runFile <- file.path(folder, paste("run", executableExt(), sep = "."))
    
    # write header
    cat(runHeader(), file = runFile, append = TRUE)
    
    # Write in runFile
    if (is.null(object@group)) {
        cmd <- paste0("'launcheR:::waitQueue(queue_name=\"", object@name, "\")' ", nullRedirection())
    } else {
        cmd <- paste0("'launcheR:::waitQueue(queue_name=\"", object@name, "\", group=\"", object@group, "\")' ", nullRedirection())
    }
    line_ <- paste(rscriptOptions(execute=TRUE), cmd)
    
    cat(line_, file = runFile, append = TRUE)
    return(runFile)
}

runBatch <- function(batch, runFile) {
        
        # Get info from batch object
        params <- batch@params
        rank_  <- batch@rank
        
        if (!is.null(params)) {
            # Create RData file if params
            env <- new.env()
            sapply(names(params), function(x) { assign(x = x, value = params[[x]], envir = env)})
            RData_file <- file.path(folder, paste0("params", rank_, ".RData"))
            save(list = ls(envir = env), envir = env, file = RData_file)
            # Add line setRdata
            runSetRData(runFile = runFile, file = RData_file)
        }
        # add line waitBatch + Rscript batch
        runWaitBatch(batch = batch, folder = folder)
}

# set this in batch class
runWaitBatch <- function(folder, batch) {
    # Don't forget to set sleep 1 at least if waitBeforeNew = FALSE (cf launcheR/dev/tests/run.sh)
    # Maybe get a method from batch class to get lines to write
    # need :
    #name = batch@name, bpar = batch@parallelizable, path = batch@path, waitBeforeNext = batch@waitBeforeNext
}

runSetRData <- function(runFile, file) {
    stopifnot(!any(unlist(lapply(list(runFile, file)))))
    cmd <- paste0("'launcheR:::setRData(file=\"", file, "\")' ", nullRedirection())
    line_ <- paste(rscriptOptions(execute=TRUE), cmd)
    cat(line_, file = runFile, append = TRUE)
}