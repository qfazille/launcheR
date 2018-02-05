setClassUnion("ListOrNULL", c("list", "NULL"))

setClass(
    Class = "batch",
    slots = c(
        name = "character",
        path = "character",
        params = "ListOrNULL",
        parallelizable = "logical",
        waitBeforeNext = "logical"
    ),
    prototype = prototype(
        name = NULL,
        params = NULL,
        parallelizable = TRUE,
        waitBeforeNext = TRUE
    )
)

setMethod(f = "initialize", signature = "batch", definition = function(.Object, name = NULL, path = NULL, params = NULL, parallelizable = TRUE) {
    # Path not null & exists
    if (is.null(path)) stop("Path cannot be NULL")
    if (file.access(path, mode = 0) == -1) stop("Filepath must exists")
    path <- normalizePath(path)

    # If UNIX must be executable
    if (sysname() == "Unix") {
        if (file.access(path, mode = 2) == -1) stop("File must be executable")
    }

    # if name null, then set file name without extension
    if (is.null(name)) {
        name <- tools::file_path_sans_ext(basename(path))
    } else if (!is.character(name) | nchar(name) > 20) {
        stop("If name is specified then must be a character with length < 20")
    }

    # If params not null
    if (!is.null(params)) {
        # must be a list
        if (!is.list(params)) stop("If params specified then must be a list")
        # Check all slot have name
        if (is.null(names(params)) | any(names(params) == "")) stop("All slots in params must be named")
    } else {
        params <- NULL
    }

    .Object@path            <- path
    .Object@name           <- name
    .Object@params          <- params
    .Object@parallelizable  <- parallelizable

    validObject(.Object)
    return(.Object)
})


setMethod(f = "script", signature= "batch", definition = function(object) {
    lines <- NULL
    wait        <- sprintf(paste(rscript(), " --options -e 'launcheR:::wait(parallelizable = %s)' > ", sep = ""), object@parallelizable)
    setrdata    <- paste(rscript(), "launcheR:::set.RData()")
    setrenviron <- paste(rscript(), "launcheR:::set.Renviron()")
    # Set & or not
    if (object@waitBeforeNext) ampersand <- NULL else ampersand <- "&"
    launch      <- paste(rscript(), object@path, ampersand)
    releaselock <- paste(rscript(), "launcheR:::releaseBatch()")
    lines <- paste(wait, setrdata, setrenviron, launch, releaselock, sep = linebreak())
    return(lines)
})
