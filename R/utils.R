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

# Null redirection
nullRedirection <- function() {
    if (sysname() == "Windows") {
        return("> NUL")
    } else if (sysname() == "Unix") {
        return("> /dev/null")
    }
}

# Get date as character
getDate <- function() {
    format(Sys.time(), format="%Y-%m-%d %H:%M:%S")
}