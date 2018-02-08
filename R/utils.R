# All utils functions

# Get OS
sysname <- function() {
    sysname <- tolower(.Platform$OS.type)
    if (sysname == "windows") {
        return("Windows")
    } else if (sysname == "unix") {
        return("Unix")
    } else {
        stop("OS not supported for this package")
    }
}

# Get line break according to system
linebreak <- function() {
    if (sysname() == "Windows") {
        return("\r\n")
    } else if (sysname() == "Unix") {
        return("\n")
    }
}

# Get Rscript used
rscript <- function() {
    path <- file.path(Sys.getenv("R_HOME"), "bin")
    files_ <- list.files(path)
    rscript_ <- file.path(path, files_[which(grepl("Rscript", files_))])
    if (length(rscript_) == 0) {
        stop(paste("There is no Rscript present in", path))
    } else if (length(rscript_) > 1) {
        warning(paste("Several Rscript present in ", path, "\n", "Selected :", rscript_[1]))
        return(rscript_[1])
    } else {
        return(rscript_)
    }
}

rscriptOptions <- function(execute = FALSE, restore = FALSE, nosave = FALSE) {
    opt <- rscript()
    if (execute) opt <- paste(opt, "-e")
    if (restore) opt <- paste(opt, "--restore")
    if (nosave)  opt <- paste(opt, "--no-save")
    return(opt)
}

# Null redirection
nullRedirection <- function() {
    if (sysname() == "Windows") {
        return(NULL)
    } else if (sysname() == "Unix") {
        return(paste("> /dev/null", linebreak()))
    }
}

# executable file
executableExt <- function() {
    if (sysname() == "Windows") {
        return("bat")
    } else if (sysname() == "Unix") {
        return("sh")
    }
}

# header
runHeader <- function() {
    if (sysname() == "Windows") {
        return(NULL)
    } else if (sysname() == "Unix") {
        return(paste("#!/bin/bash", linebreak()))
    }
}

# change directory at startup
runCD <- function(folder) {
    stopifnot(!is.null(folder))
    if (sysname() == "Windows") {
        return(NULL)
    } else if (sysname() == "Unix") {
        return(paste("cd", folder, linebreak()))
    }
}

# init .Renviron
runRenviron <- function() {
    if (sysname() == "Windows") {
        return(NULL)
    } else if (sysname() == "Unix") {
        return(paste("touch .Renviron", linebreak()))
    }
}

# sleep
getSleep <- function() {
    if (sysname() == "Windows") {
        return(NULL)
    } else if (sysname() == "Unix") {
        return(paste("sleep 2", linebreak()))
    }
}

# Get date as character
getDate <- function() {
    format(Sys.time(), format="%Y-%m-%d %H:%M:%S")
}

# get a timestamp
getTS <- function() {
    format(Sys.time(), format="%Y%m%d_%H%M%S")
}

redirect_log <- function() {
    if (sysname() == "Windows") {
        return(NULL)
    } else if (sysname() == "Unix") {
        return(">>")
    }
}

# Gather several lines into one
gatherCmd <- function(..., background = FALSE) {
    if (sysname() == "Windows") {
        return(NULL)
    } else if (sysname() == "Unix") {
        line_ <- paste0("(", paste(..., sep = " ; \\\n"), ")") 
        if (background) line_ <- paste(line_, "&", linebreak()) else line_ <- paste(line_, linebreak())
        return(line_)
    }
}

# Set cmd to pass to system function
launchFile <- function(runFile = NULL) {
    stopifnot(!is.null(runFile))
    if (sysname() == "Windows") {
        return(NULL)
    } else if (sysname() == "Unix") {
        line_ <- paste(runFile, "&")
        return(line_)
    }
}

# Name of temp folder
tmpFolder <- function(name = NULL) {
    stopifnot(!is.null(name))
    return(paste(name, "LR", getTS(), sep = "_"))
}

# Is temp folder
isTmpFolder <- function(folder = NULL) {
    stopifnot(!is.null(folder))
    res <- grep("LR_[0-9]{8}_[0-9]{6}", folder, perl=TRUE)
    if (length(res) == 0) return(FALSE) else return(TRUE)
}

# Remove illegal character from string
validName <- function(name = NULL) {
    stopifnot(!is.null(name))
    # iconv remove accents
    # make.names remove non character or number
    x <- iconv(make.names(name), to='ASCII//TRANSLIT')
    # Remove dots
    gsub(".", "", x, fixed = TRUE)
}

# check batch path
checkBatchPath <- function(path = NULL) {
    stopifnot(!is.null(path))
    # extension must be .R
    stopifnot(tools::file_ext(path) == "R")
    if (file.access(path, mode = 0) == -1) stop("Filepath must exists")
    return(TRUE)
}

executableFile <- function(runFile) {
    stopifnot(!is.null(runFile))
    Sys.chmod(runFile, mode = "0750", use_umask = TRUE)
}
