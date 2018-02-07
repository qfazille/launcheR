setClassUnion("CharacterOrNULL", c("character", "NULL"))
setClassUnion("ListOrNULL", c("list", "NULL"))

### Class
#' The queue class
#'
#' This class is used to store follwing slot
#'
#' @slot name Character Contains name of the queue. (Default basename(tempfile(pattern = "file", tmpdir = "")))
#' @slot group Character Contains the group name the queue belongs to. (Default NULL)
#' @slot folder Character Contains path to a folder that will contains the working directory folder. (Default tempdir())
#' @slot batchs List Contains batch S4 objects. (Default NULL)
#'
#' @name classQueue
#' @rdname classQueue
#' @aliases queue-class
#' @exportClass queue
#' @examples
#' showClass("queue")
#' 
#' ## Methods available for this class
#' showMethods(classes="queue")
#' @author Quentin Fazilleau
setClass(
    Class = "queue",
    slots = c(
        name = "character",
        group = "CharacterOrNULL",
        folder = "character",
        batchs = "ListOrNULL",
        logdir = "character",
        clean = "logical"
    )
)

setMethod(f = "initialize"
        , signature = "queue"
        , definition = function(.Object
                            , name = basename(tempfile(pattern = "queue", tmpdir = ""))
                            , group = NULL, folder = tempdir(), logdir = NULL, clean = TRUE) 
        {
            # valid name
            .Object@name <- validName(name)
            
            # Don't need to validate group, clean, batchs
            .Object@group     <- group
            .Object@clean     <- clean
            .Object@batchs    <- list()
            
            # Create the subfolder under folder
            temp_folder <- tmpFolder(name = .Object@name) 
            subfolder <- file.path(folder, temp_folder)
            .Object@folder    <- subfolder
            
            # Set logdir
            if (is.null(logdir)) {
                .Object@logdir <- file.path(subfolder, "logs")
            } else {
                .Object@logdir <- logdir
            }
            
            validObject(.Object)
            return(.Object)
        }
)

#' addBatch
#'
#' Add batch to a queue
#' @param object An object of class queue.
#' @param ... Others arguments from specific methods.
#' @exportMethod addBatch
setGeneric(name="addBatch",def=function(object,...)			{standardGeneric("addBatch")})


#' @rdname addBatch
#' @param name Character Contains alias of the batch. (Default basename(batch@path))
#' @param path Character Contains the path to the R batch. (Mandatory)
#' @param params Named list Contains the variable to be transfered to batch. (Default NULL)
#' @param parallelizable Logical If batch can be launched multiple times at the same moment regardless to groups. (Default TRUE)
#' @param waitBeforeNext Logical If queue can launch next batch while this one. (Default TRUE)
#' @param logfile Character Contains path to file that contains batch output. (Default queue@logfolder/batch@name.log)
#' @examples 
#' \dontrun{
#' to be seen later
#' }
setMethod(f = "addBatch", signature = "queue", definition = function(object, name = NULL, path = NULL, params = NULL, parallelizable = TRUE, waitBeforeNext = TRUE, logfile = NULL) {
    # Get Rank
    Rank <- length(object@batchs) + 1
    
    # Path not null & exists
    if (is.null(path)) stop("Not a valid path")
    path <- normalizePath(path)
    if (!checkBatchPath(path = path)) stop("Not a valid path")
    
    # Get name, if name null, then set file name without extension
    if (is.null(name)) name <- tools::file_path_sans_ext(basename(path))
    if (!is.character(name) | nchar(name) > 30) stop("If name is specified then must be a character with length < 30")
    
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
    batch <- new(Class = "batch", name = name, path = path, params = params, parallelizable = parallelizable, waitBeforeNext = waitBeforeNext, logfile = logfile, Rank = Rank)
    
    # Add batch to queue
    object@batchs[[Rank]] <- batch
    return(object)
})
