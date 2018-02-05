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

rscriptOptions <- function(execute = FALSE, restore = FALSE) {
    opt <- rscript()
    if (execute) opt <- paste(opt, "-e")
    if (restore) opt <- paste(opt, "--restore")
    return(opt)
}

# Null redirection
nullRedirection <- function() {
    if (sysname() == "Windows") {
        return("> NUL")
    } else if (sysname() == "Unix") {
        return("> /dev/null")
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
        return("#!/bin/bash")
    }
}

# sleep
getSleep <- function() {
    if (sysname() == "Windows") {
        return(NULL)
    } else if (sysname() == "Unix") {
        return("sleep 1")
    }
}

# Get date as character
getDate <- function() {
    format(Sys.time(), format="%Y-%m-%d %H:%M:%S")
}

getTS <- function() {
    format(Sys.time(), format="%Y%m%d_%H%M%S")
}