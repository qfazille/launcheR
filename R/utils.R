# All utils functions

# Get OS
sysname <- function() {
    sysname <- tolower(.Platform$OS.type)
    if (sysname == "windows") {
        stop("Windows not supported yet")
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
    if (restore) opt <- paste(opt, "--restore")
    if (nosave)  opt <- paste(opt, "--no-save")
    if (execute) opt <- paste(opt, "-e")
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

# Add TMPDIR in Renviron if needed
setTMPDIR <- function(folder) {
    return(paste0("echo TMPDIR='", folder, "' >> .Renviron", linebreak()))
}

# sleep
getSleep <- function() {
    if (sysname() == "Windows") {
        return(NULL)
    } else if (sysname() == "Unix") {
        return(paste("sleep 2", linebreak()))
    }
}

# wait
getWait <- function() {
    if (sysname() == "Windows") {
        return(NULL)
    } else if (sysname() == "Unix") {
        return(paste("wait", linebreak()))
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

# Set cmd to pass to system function
launchFile <- function(runFile = NULL, logFile = NULL) {
    stopifnot(!is.null(runFile))
    stopifnot(!is.null(logFile))
    if (sysname() == "Windows") {
        return(NULL)
    } else if (sysname() == "Unix") {
        # Set in background and redirect outputs to run.log
        line_ <- paste(runFile, ">>", logFile, errorRedir(), "&")
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
    stopifnot(toupper(tools::file_ext(path)) == "R")
    if (file.access(path, mode = 0) == -1) stop("Filepath must exists")
    return(TRUE)
}

# chmod run file
executableFile <- function(runFile) {
    stopifnot(!is.null(runFile))
    Sys.chmod(runFile, mode = "0750", use_umask = TRUE)
}

# check if folder in not in /tmp/
folderInTmp <- function(folder) {
    stopifnot(!is.null(folder))
    if (length(grep(pattern = "^/tmp/", x = folder, perl = TRUE)) == 0) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

# Convert date in char to num
DateChar2Num <- function(date) {
    a <- as.POSIXct(date, format="%Y-%m-%d %H:%M:%S")
    as.numeric(a)
}

# Convert num to date in char
Num2DateChar <- function(num) {
    format(as.POSIXct(num, origin = "1970-01-01"), format = "%Y-%m-%d %H:%M:%S")
}

internalLogRedir <- function(logfile = "run.log") {
    if (sysname() == "Windows") {
        return(NULL)
    } else if (sysname() == "Unix") {
        return(paste(">>", logfile, "2>&1"))
    }
}

errorRedir <- function() {
    if (sysname() == "Windows") {
        return(NULL)
    } else if (sysname() == "Unix") {
        return("2>&1")
    }
}

getIfStatus <- function() {
    if (sysname() == "Windows") {
        return(NULL)
    } else if (sysname() == "Unix") {
        return("if [ $? -eq 0 ]; then")
    }
}

getElse <- function() {
    if (sysname() == "Windows") {
        return(NULL)
    } else if (sysname() == "Unix") {
        return("else")
    }
}

getEndIf <- function() {
    if (sysname() == "Windows") {
        return(NULL)
    } else if (sysname() == "Unix") {
        return("fi")
    }
}


setErrorIfElse <- function(if_true, if_false = NULL) {
    if (is.null(if_false)) {
        paste(getIfStatus(), addTab(if_true), getEndIf(), sep = linebreak())
    } else {
        paste(getIfStatus(), addTab(if_true), getElse(), addTab(if_false), getEndIf(), sep = linebreak())
    }
}

addTab <- function(cmd) {
    paste0("    ", gsub(pattern = linebreak(), replacement = paste0(linebreak(), "    "), x = cmd))
}