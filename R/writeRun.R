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
        cat(setTMPDIR(object@tmpdir), file = runFile, append = TRUE)
    }
    
    # Write in runFile, the || execute only if error. In case queue is abandonned for any reason (manual releaseQueue before its launch ?) 
    cmd <- paste0("'launcheR:::waitQueue()' || exit", linebreak())
    line_ <- paste(rscriptOptions(execute=TRUE), cmd)
    cat(line_, file = runFile, append = TRUE)
    
    # Make file executable
    executableFile(runFile = runFile)
    
    return(runFile)
}

runBatch <- function(batch, runFile) {
        # Get info from batch object
        Rank   <- batch@Rank
        params <- batch@params
        if (is.null(params)) exist_params <- FALSE else exist_params <- TRUE
        
        # Get folder
        folder <- dirname(runFile)

        cmd_waitBatch <- paste(rscriptOptions(execute = TRUE), paste0("'launcheR:::waitBatch(Rank=", batch@Rank, ")' ;", linebreak()))

        if (exist_params) {
            cmd_setRData <- paste(rscriptOptions(execute = TRUE), paste0("'launcheR:::setRData(Rank=", batch@Rank, ")' ;", linebreak()))
        } else {
            cmd_setRData <- NULL
        }

        # Add /path/to/batch.R >> batch.log
        run  <- paste(rscriptOptions(restore = exist_params), batch@path, redirect_log(), batch@logfile, errorRedir(), ";", linebreak())

        # release batch : status
        release_OK    <- paste(rscriptOptions(execute = TRUE), paste0("'launcheR:::releaseBatch(Rank=", batch@Rank, ", status=\"OK\")' "))
        release_KO <- paste(rscriptOptions(execute = TRUE), paste0("'launcheR:::releaseBatch(Rank=", batch@Rank, ", status=\"KO\")' "))
        cmd_releaseBatch  <- setErrorIfElse(release_OK, release_KO)

        cmd_all <- paste0(cmd_setRData, run, cmd_releaseBatch)
        cmd_all_in_if <- setErrorIfElse(cmd_all)

        cmd_all_batch <- paste("(", cmd_waitBatch, cmd_all_in_if, ") &", linebreak(), sep = "")
        cat(cmd_all_batch, file = runFile, append = TRUE)
}

createRDataFile <- function(batch, folder) {
    env <- new.env()
    sapply(names(batch@params), function(x) { assign(x = x, value = batch@params[[x]], envir = env)})
    RData_file <- file.path(folder, paste0("params", batch@Rank, ".RData"))
    save(list = ls(envir = env), envir = env, file = RData_file)
}

# Add launcheR:::release
runReleaseQueue <- function(runFile = NULL) {
    stopifnot(!is.null(runFile))
    cmd_line <- paste(rscriptOptions(execute = TRUE), paste0("'launcheR:::releaseQueue()' ", linebreak()))
    cat(cmd_line, file = runFile, append = TRUE, sep = "")
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

# This function create a meta.RData that contains the queue without all parameters
createMeta <- function(object) {
    object@batchs <- sapply(object@batchs, function(x) {x@params <- NULL;x})
    RDS_file <- file.path(object@folder, paste0("meta.RDS"))
    saveRDS(object, file = RDS_file)
}

loadMeta <- function() {
    return(readRDS("./meta.RDS"))
}