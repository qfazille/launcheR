setClassUnion("ListOrNULL", c("list", "NULL"))

### Class
#' The batch class
#'
#' This class is used to store follwing slot
#'
#' @slot name Character Contains alias of the batch. (Default basename(batch@path))
#' @slot path Character Contains the path to the R batch. (Mandatory)
#' @slot params Named list Contains the variable to be transfered to batch. (Default NULL)
#' @slot parallelizable Logical If batch can be launched multiple times at the same moment regardless to groups. (Default TRUE)
#' @slot waitBeforeNext Logical If queue can launch next batch while this one. (Default TRUE)
#' @slot logfile Character Contains path to file that contains batch output. (Default queue@logfolder/batch@name.log)
#' @slot Rank Numeric Contains rank number of batch within the queue. (Not set manually)
#'
#' @name classBatch 
#' @rdname classBatch
#' @aliases batch-class
#' @exportClass batch
#' @examples
#' showClass("batch")
#' 
#' ## Methods available for this class
#' showMethods(classes="batch")
#' @author Quentin Fazilleau
setClass(
    Class = "batch",
    slots = c(
        name = "character",
        path = "character",
        params = "ListOrNULL",
        parallelizable = "logical",
        waitBeforeNext = "logical",
        logfile = "character",
        Rank = "numeric"
    )
)

setMethod(f = "initialize"
    , signature = "batch"
    , definition = function(.Object
                        , name = NULL, path = NULL, params = NULL, parallelizable = TRUE
                        , waitBeforeNext = TRUE, logfile = NULL, Rank = NULL) 
    {
        # Name, params, Rank & logfile must be set in method addBatch
        if (is.null(Rank)) stop("Rank cannot be null")
        if (is.null(logfile)) stop("logfile cannot be null")
        if (is.null(name)) stop("name should be set before")
        
        .Object@name            <- name
        .Object@path            <- path
        .Object@params          <- params
        .Object@parallelizable  <- parallelizable
        .Object@waitBeforeNext  <- waitBeforeNext
        .Object@logfile         <- logfile
        .Object@Rank            <- Rank
        
        validObject(.Object)
        return(.Object)
    }
)

