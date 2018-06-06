randomName <- function(prefix="") paste0(prefix, substr(basename(tempfile()), 8, 13))

checkHQ <- function(name = NULL, minStartDate = list(), maxStartDate = list(), minEndDate = list(), maxEndDate = list(), ...) {
    # The parameters for checking dates are list containing other queue/batch names.
    stopifnot(!is.null(name))
    # Every min/max_Start/End_Date must be a list containing lists of (name = ..., check = c("start", "end"))
    stopifnot(all(sapply(list(minStartDate, maxStartDate, minEndDate, maxEndDate), class) == "list"))

    toReturn <- TRUE

    # Get data with queuename
    hq <- getHistorizedQueue()
    df <- hq[which(hq$queuename == name),]

    # Check dates
    for (i in minStartDate) {
        if (!all(c("name", "check") %in% names(i))) stop(paste("Parameters not set correctly :  must contain a list of lists with slots 'name' & 'check'"))
        if (i[["check"]] == "start") {
            checkDate <- hq[which(hq$queuename == i[["name"]]), "realStartDate"]
        } else {
            checkDate <- hq[which(hq$queuename == i[["name"]]), "endDate"]
        }
        if (!df$realStartDate >= checkDate) {
            toReturn <- FALSE
            warning(paste(name, "startDate not superior of", i[["name"]], i[["check"]]))
        }
    }

    # Check dates
    for (i in maxStartDate) {
       if (!all(c("name", "check") %in% names(i))) stop(paste("Parameters not set correctly :  must contain a list of lists with slots 'name' & 'check'"))
       if (i[["check"]] == "start") {
           checkDate <- hq[which(hq$queuename == i[["name"]]), "realStartDate"]
       } else {
           checkDate <- hq[which(hq$queuename == i[["name"]]), "endDate"]
       }
       if (!df$realStartDate <= checkDate) {
           toReturn <- FALSE
           warning(paste(name, "startDate not inferior of", i[["name"]], i[["check"]]))
       }
    }

    # Check dates
    for (i in minEndDate) {
       if (!all(c("name", "check") %in% names(i))) stop(paste("Parameters not set correctly :  must contain a list of lists with slots 'name' & 'check'"))
       if (i[["check"]] == "start") {
           checkDate <- hq[which(hq$queuename == i[["name"]]), "realStartDate"]
       } else {
           checkDate <- hq[which(hq$queuename == i[["name"]]), "endDate"]
       }
       if (!df$endDate >= checkDate) {
           toReturn <- FALSE
           warning(paste(name, "endDate not superior of", i[["name"]], i[["check"]]))
       }
    }

    # Check dates
    for (i in minEndDate) {
       if (!all(c("name", "check") %in% names(i))) stop(paste("Parameters not set correctly :  must contain a list of lists with slots 'name' & 'check'"))
       if (i[["check"]] == "start") {
           checkDate <- hq[which(hq$queuename == i[["name"]]), "realStartDate"]
       } else {
           checkDate <- hq[which(hq$queuename == i[["name"]]), "endDate"]
       }
       if (!df$endDate >= checkDate) {
           toReturn <- FALSE
           warning(paste(name, "endDate not inferior of", i[["name"]], i[["check"]]))
       }
    }

    # Check every other parameters
    others_params <- list(...)
    for (i in names(others_params)) {
        if (df[1, i] != others_params[[i]]) {
            toReturn <- FALSE
            warning(paste("Field", i, "not equal to", others_params[[i]]))
        }
    }

    return(toReturn)
}

checkHB <- function(name = NULL, minStartDate = list(), maxStartDate = list(), minEndDate = list(), maxEndDate = list(), ...) {
    # The parameters for checking dates are list containing other queue/batch names.
    stopifnot(!is.null(name))
    # Every min/max_Start/End_Date must be a list containing lists of (name = ..., check = c("start", "end"))
    stopifnot(all(sapply(list(minStartDate, maxStartDate, minEndDate, maxEndDate), class) == "list"))

    toReturn <- TRUE

    # Get data with batchname
    hb <- getHistorizedBatch()
    df <- hb[which(hb$batchname == name),]

    # Check dates
    for (i in minStartDate) {
        if (!all(c("name", "check") %in% names(i))) stop(paste("Parameters not set correctly :  must contain a list of lists with slots 'name' & 'check'"))
        if (i[["check"]] == "start") {
            checkDate <- hb[which(hb$batchname == i[["name"]]), "realStartDate"]
        } else {
            checkDate <- hb[which(hb$batchname == i[["name"]]), "endDate"]
        }
        if (!df$realStartDate >= checkDate) {
            toReturn <- FALSE
            warning(paste(name, "startDate not superior of", i[["name"]], i[["check"]]))
        }
    }

    # Check dates
    for (i in maxStartDate) {
        if (!all(c("name", "check") %in% names(i))) stop(paste("Parameters not set correctly :  must contain a list of lists with slots 'name' & 'check'"))
        if (i[["check"]] == "start") {
            checkDate <- hb[which(hb$batchname == i[["name"]]), "realStartDate"]
        } else {
            checkDate <- hb[which(hb$batchname == i[["name"]]), "endDate"]
        }
        if (!df$realStartDate <= checkDate) {
            toReturn <- FALSE
            warning(paste(name, "startDate not inferior of", i[["name"]], i[["check"]]))
        }
    }

    # Check dates
    for (i in minEndDate) {
        if (!all(c("name", "check") %in% names(i))) stop(paste("Parameters not set correctly :  must contain a list of lists with slots 'name' & 'check'"))
        if (i[["check"]] == "start") {
            checkDate <- hb[which(hb$batchname == i[["name"]]), "realStartDate"]
        } else {
            checkDate <- hb[which(hb$batchname == i[["name"]]), "endDate"]
        }
        if (!df$endDate >= checkDate) {
            toReturn <- FALSE
            warning(paste(name, "endDate not superior of", i[["name"]], i[["check"]]))
        }
    }

    # Check dates
    for (i in minEndDate) {
        if (!all(c("name", "check") %in% names(i))) stop(paste("Parameters not set correctly :  must contain a list of lists with slots 'name' & 'check'"))
        if (i[["check"]] == "start") {
            checkDate <- hb[which(hb$batchname == i[["name"]]), "realStartDate"]
        } else {
            checkDate <- hb[which(hb$batchname == i[["name"]]), "endDate"]
        }
        if (!df$endDate >= checkDate) {
            toReturn <- FALSE
            warning(paste(name, "endDate not inferior of", i[["name"]], i[["check"]]))
        }
    }

    # Check every other parameters
    others_params <- list(...)
    for (i in names(others_params)) {
        if (df[1, i] != others_params[[i]]) {
            toReturn <- FALSE
            warning(paste("Field", i, "not equal to", others_params[[i]]))
        }
    }

    return(toReturn)
}
