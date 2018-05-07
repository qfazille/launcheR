setClassUnion("CharacterOrNULL", c("character", "NULL"))
setClassUnion("ListOrNULL", c("list", "NULL"))

#' @name classQueue
#' @aliases queue-class
#' @title The queue class
#' @description This class is used to store follwing slot
#' @slot name Character Alias name of the queue. (Default basename(tempfile(pattern = "file", tmpdir = "")))
#' @slot desc Character Description. (Optional)
#' @slot group Character Name of the group the queue belongs to. (Default NULL)
#' @slot owner Character username of launcher. (Default Sys.info()["user"])
#' @slot folder Character Path to a folder that will contains the working directory folder. (Default tempdir())
#' @slot batchs List Batch S4 objects. (Default NULL)
#' @slot logdir Character Path to the folder that will contains logs file. (By default in folder)
#' @slot clean Logical Whether or not the working directory folder should be removed. (Default TRUE)
#' @slot tmpdir Character If use through RSConnect you can redefine a tmpdir not in /tmp/*. (Default NULL)
#' @aliases queue-class
#' @rdname classQueue
#' @exportClass queue
#' @author Quentin Fazilleau
setClass(
    Class = "queue",
    slots = c(
        name = "character",
        desc = "CharacterOrNULL",
        group = "CharacterOrNULL",
        owner = "character",
        folder = "character",
        batchs = "ListOrNULL",
        logdir = "character",
        clean = "logical",
        tmpdir = "CharacterOrNULL"
    )
)

#' @importFrom methods validObject
setMethod(f = "initialize"
        , signature = "queue"
        , definition = function(.Object, name = NULL, desc = NULL, group = NULL, owner = NULL, folder = NULL, logdir = NULL, clean = TRUE, tmpdir = NULL)
        {
            # valid name
            .Object@name <- validName(name)

            # valid owner
            if (is.null(owner))     owner <- ""
            if (nchar(owner) > 20)  owner <- substr(owner, 1, 20)

            # Don't need to validate group, clean, batchs
            .Object@desc      <- desc
            .Object@group     <- group
            .Object@clean     <- clean
            .Object@batchs    <- list()
            .Object@owner     <- owner

            # Create the subfolder under folder
            if (is.null(folder) & !is.null(tmpdir)) {
                folder <- tmpdir
            } else if (is.null(folder)) {
                folder <- tempdir()
            }
            folder <- normalizePath(folder, mustWork = TRUE)
            temp_folder <- tmpFolder(name = .Object@name)
            subfolder <- file.path(folder, temp_folder)
            .Object@folder    <- subfolder

            # Set logdir
            if (is.null(logdir)) {
                .Object@logdir <- file.path(subfolder, "logs")
            } else {
                .Object@logdir <- logdir
            }

            # If tmpdir specified then check not in /tmp folder
            if (!is.null(tmpdir)) {
                if (folderInTmp(tmpdir)) stop("If tmpdir specified, then should not be in /tmp/* folder")
                tmpdir <- normalizePath(tmpdir, mustWork = TRUE)
            }
            .Object@tmpdir <- tmpdir

            validObject(.Object)
            return(.Object)
        }
)

#' @aliases addBatch
#' @param object An object of class queue
#' @param ... Others arguments from specific methods
#' @rdname addBatch
#' @export
setGeneric(name = "addBatch", def = function(object, ...) {
    standardGeneric("addBatch")
})

#' @name addBatch
#' @aliases addBatch,queue-method
#' @title addBatch
#' @description Add batch to a queue
#' @param path Character Path to the R batch. (Mandatory)
#' @param name Character Alias of the batch. (Default basename(batch@path))
#' @param desc Character Description
#' @param params Named list that contains the variable to be transfered to batch. (Default NULL)
#' @param parallelizable Logical If batch can be launched multiple times at the same moment regardless to groups. (Default TRUE)
#' @param waitBeforeNext Logical If queue can launch next batch while this one. (Default TRUE)
#' @param logfile Character Path to file that contains batch output. (Default queue@logfolder/batch@name.log)
#' @rdname addBatch
#' @exportMethod addBatch
#' @examples
#' \dontrun{
#' q <- createQueue()
#' q <- addBatch(q, "/path/batch.R")
#' launch(q)
#' }
#' @author Quentin Fazilleau
#' @importFrom methods new
setMethod(f = "addBatch", signature = "queue", definition = function(object, path = NULL, name = NULL, desc = NULL, params = NULL, parallelizable = TRUE, waitBeforeNext = TRUE, endIfKO = TRUE, logfile = NULL) {
    # Get Rank
    Rank <- length(object@batchs) + 1

    # Path not null & exists
    if (is.null(path)) stop("Not a valid path")
    path <- normalizePath(path, mustWork = TRUE)
    if (!checkBatchPath(path = path)) stop("Not a valid path")

    # Get name, if name null, then set file name without extension
    if (is.null(name)) name <- tools::file_path_sans_ext(basename(path))
    if (!is.character(name) | nchar(name) > 40) stop("If name is specified then must be a character with length < 40")

    # Get logfile
    if (is.null(logfile)) {
        logfile <- file.path(object@logdir, paste0(name, ".log"))
    }

    # If params not null
    if (!is.null(params)) {
        # must be a list & all slots must have names
        if (!is.list(params)) stop("If params specified then must be a list")
        if (is.null(names(params)) | any(names(params) == "")) stop("All slots in params must be named")
    }

    # Create batch
    batch <- new(Class = "batch", name = name, desc = desc, path = path, params = params, parallelizable = parallelizable, waitBeforeNext = waitBeforeNext, endIfKO = endIfKO, logfile = logfile, Rank = Rank)

    # Add batch to queue
    object@batchs[[Rank]] <- batch
    return(object)
})

#' @aliases launch
#' @param object An object of class queue
#' @rdname launch
#' @export
setGeneric(name="launch",def=function(object) {
    standardGeneric("launch")
})

#' @name launch
#' @aliases launch,queue-method
#' @title launch
#' @description Function to launch a queue.
#' @rdname launch
#' @exportMethod launch
#' @examples
#' \dontrun{
#' q <- createQueue()
#' q <- addBatch(q, "/path/batch.R")
#' launch(q)
#' }
setMethod(f = "launch", signature = "queue", definition = function(object) {
    # check queue has at least one batch
    if (is.null(object@batchs)) stop("Queue doesn't have any batch")

    # check if already exists (with TS should not append)
    if (file.exists(object@folder)) stop(paste("Already a folder named", object@folder))

    # create subfolder
    dir.create(object@folder)

    # if logdir doesn't exist create it
    if (!file.exists(object@logdir)) dir.create(object@logdir)

    # Create meta.RData containing the queue
    createMeta(object = object)

    # Initialize run.sh with first line waitQueue
    runFile <- runInit(object = object)

    # Loop on batch
    #   - Create RData file
    #   - Add in run.sh the lines
    for (i in 1:length(object@batchs)) {
        # Create .RData for parameters if needed
        if (!is.null(object@batchs[[i]]@params)) createRDataFile(batch = object@batchs[[i]], folder = object@folder)
        # Add in runFile the whole part linked to the batch (waitBatch + setRData + run + releaseBatch)
        runBatch(batch = object@batchs[[i]], runFile = runFile)
        # Add sleep 2 or wait
        if (i == length(object@batchs)) {
            cat("wait", file = runFile, append = TRUE, sep = linebreak())
        } else {
            cat("sleep 2", file = runFile, append = TRUE, sep = linebreak())
        }
    }

    # Add releaseQueue
    runReleaseQueue(runFile = runFile)

    # Add clean directory
    if (object@clean) {
        cleanFolder(folder = object@folder, runFile = runFile)
    }

    # Launch file in background
    logFile <- file.path(object@folder, "run.log")
    cmd <- launchFile(runFile = runFile, logFile = logFile)
    #system(cmd)
    print(cmd)
    # Wait 1sec just the time database is updated
    #Sys.sleep(1)
})

# Get a batch from its Rank
setGeneric(name="batchFromRank",def=function(object, Rank)   {standardGeneric("batchFromRank")})
setMethod(f = "batchFromRank", signature = "queue", definition = function(object) {
    res <- sapply(object@batchs, function(x) {if (x@Rank == Rank) x})
    res[[1]]
})


# for dev purposes
setGeneric(name="cleanQ",def=function(object)   {standardGeneric("cleanQ")})
setMethod(f = "cleanQ", signature = "queue", definition = function(object) {
    if (isTmpFolder(object@folder)) {
        if (file.exists(object@folder)) {
            unlink(object@folder, recursive = TRUE)
            message(paste("Remove folder", object@folder))
        } else {
            message(paste("Folder", object@folder, "doesn't exists"))
        }
    } else {
        message(paste("Folder", object@folder, "suspicious"))
    }
})

