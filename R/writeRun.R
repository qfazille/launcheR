runInit <- function(object) {
    # Get info from queue object
    folder      <- object@folder
    queue_name  <- object@name
    group       <- object@group
    
    # filepath to run file
    runFile <- file.path(folder, paste("run", executableExt(), sep = "."))
    
    # write header
    cat(runHeader(), file = runFile, append = TRUE)
    
    # write cd
    cat(runCD(folder = object@folder), file = runFile, append = TRUE)
    
    # init .Renviron
    cat(runRenviron(), file = runFile, append = TRUE)
    
    # If tmpdir then write it in Renviron
    if (!is.null(object@tmpdir)) {
        writeRenviron(prefix = "TMP", value = object@tmpdir)
        writeRenviron(prefix = "TMPDIR", value = object@tmpdir)
        writeRenviron(prefix = "TEMP", value = object@tmpdir)
    }
    
    # Write in runFile
    if (is.null(object@group)) {
        cmd <- paste0("'launcheR:::waitQueue(queue_name=\"", object@name, "\")' ", nullRedirection())
    } else {
        cmd <- paste0("'launcheR:::waitQueue(queue_name=\"", object@name, "\", group=\"", object@group, "\")' ", nullRedirection())
    }
    line_ <- paste(rscriptOptions(execute=TRUE), cmd)
    
    cat(line_, file = runFile, append = TRUE)
    
    # Make file executable
    executableFile(runFile = runFile)
    
    return(runFile)
}

runBatch <- function(batch, runFile) {
        
        # Get info from batch object
        params <- batch@params
        Rank   <- batch@Rank
        
        # Get folder
        folder <- dirname(runFile)
        
        # Params
        if (is.null(params)) exist_params <- FALSE else exist_params <- TRUE
        
        # If params add 'launcheR:::setRData'
        if (exist_params) {
            # Create RData file if params
            env <- new.env()
            sapply(names(params), function(x) { assign(x = x, value = params[[x]], envir = env)})
            RData_file <- file.path(folder, paste0("params", Rank, ".RData"))
            save(list = ls(envir = env), envir = env, file = RData_file)
            # Add line setRdata
            runSetRData(runFile = runFile, file_ = RData_file)
        }
        # add line 'launcheR:::waitBatch' + Rscript batch
        runWaitBatch(batch = batch, runFile = runFile, params = exist_params)
}

runWaitBatch <- function(batch, runFile, params = FALSE) {
    # check inputs
    stopifnot(!any(unlist(lapply(list(batch, runFile, params), is.null))))
    
    # Add 'launcheR:::waitBatch'
    cmd <- paste0("'launcheR:::waitBatch(batch_name=\"", batch@name, "\", batch_par=\"", batch@parallelizable, "\", batch_rank=\"", batch@Rank, "\")' ", nullRedirection())
    line_ <- paste(rscriptOptions(execute = TRUE), cmd)
    cat(line_, file = runFile, append = TRUE)
    
    # Add 'Rscript /path/batch' & 'launcheR:::releaseBatch' in the same line + & if waitBeforeNext = FALSE
    cmd1 <- paste(rscriptOptions(restore = params), batch@path, redirect_log(), batch@logfile)
    cmd2 <- paste(rscriptOptions(execute = TRUE), paste0("'launcheR:::releaseBatch(batch_rank=\"", batch@Rank, "\")' "))
    line_ <- gatherCmd(cmd1, cmd2, background = !batch@waitBeforeNext)
    cat(line_, file = runFile, append = TRUE)
    
    # Add sleep
    cmd <- getSleep()
    cat(cmd, file = runFile, append = TRUE)
}

# Add launcheR:::setRData
runSetRData <- function(runFile, file_) {
    stopifnot(!any(unlist(lapply(list(runFile, file_), is.null))))
    cmd <- paste0("'launcheR:::setRData(file=\"", file_, "\")' ", nullRedirection())
    line_ <- paste(rscriptOptions(execute = TRUE), cmd)
    cat(line_, file = runFile, append = TRUE)
}

# Add launcheR:::release
runReleaseQueue <- function(runFile = NULL) {
    stopifnot(!is.null(runFile))
    wait_line <- getWait() # This line in case user set waitBeforeNext = FALSE at the last batch of queue.
    cmd <- paste0("'launcheR:::releaseQueue()' ", nullRedirection())
    cmd_line <- paste(rscriptOptions(execute = TRUE), cmd)
    cat(wait_line, cmd_line, file = runFile, append = TRUE, sep = "")
}

# Add remove temp folder
cleanFolder <- function(folder = NULL, runFile = NULL) {
    stopifnot(!any(unlist(lapply(list(folder, runFile), is.null))))
    # Security not to rm something else that a QueueTemporary folder
    if (isTmpFolder(folder)) {
        if (sysname() == "Windows") {
            return(NULL)
        } else if (sysname() == "Unix") {
            cmd <- paste("rm -R", folder)
        }
        cat(cmd, file = runFile, append = TRUE)
    }
}
