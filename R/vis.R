# MNQ : Maximum number of batch at a same time in a queue (not needed)
# MNB : Maximum number of batch at a same time in a queue for a batch.
# RSDB : Real Start Date for a Batch
# RSDQ : Real Start Date for a Queue
# SDB : Start Date for a Batch
# SDQ : Start Date for a Queue
# EDB : End Date for a Batch
# EDQ : End Date for a Queue
# RB : Rank number of batch amoung batchs running at a same time (for a same queue)
getEDQ <- function(queueid) {
    DateChar2Num(getHistorizedQueue(queueid = queueid)$endDate)
}

getSDQ <- function(queueid) {
    DateChar2Num(getHistorizedQueue(queueid = queueid)$startDate)
}

getRSDQ <- function(queueid) {
    DateChar2Num(getHistorizedQueue(queueid = queueid)$realStartDate)
}

getYlabs <- function(queueid, show_queue, show_group, show_owner) {
    df <- getHistorizedQueue(queueid = queueid[1])
    # get info
    if (is.na(df$group)) group <- "no group" else group <- df$group
    if (df$owner == "") owner <- "no owner" else owner <- df$owner
    if (df$queuename == "") queuename <- "no queuename" else queuename <- df$queuename
    # write label
    if (all(show_queue, show_group, show_owner)) return(paste0(queuename, " (", group, "-", owner, ")"))
    if (all(show_queue, show_group)) return(paste0(queuename, " (", group, ")"))
    if (all(show_queue, show_owner)) return(paste0(queuename, " (", owner, ")"))
    if (all(show_group, show_owner)) return(paste0("(", group, "-", owner, ")"))
    if (show_queue) return(paste(queuename))
    if (show_group) return(paste(group))
    if (show_owner) return(paste(owner))
}

#' @importFrom stats ave
addMNB <- function(queueid) {
    df <- getHistorizedBatch(queueid = queueid)
    df$batchgroup   <- NA
    df$MNB          <- NA
    df$MED          <- NA # max end date by group
    batchgroupnumber <- 1
    for (i in 1:nrow(df)) {
        if (is.na(df[i, "batchgroup"])) {
            sdb <- DateChar2Num(df[i, "startDate"])
            edb <- DateChar2Num(df[i, "endDate"])
            # Two cases :
            #   - sdb or ebd in [df$startDate - df$endDate]
            #   - df$startDate or df$endDate in [sdb - end]
            case1a <- which(sdb >= DateChar2Num(df$startDate) & sdb <= DateChar2Num(df$endDate))
            case1b <- which(edb >= DateChar2Num(df$startDate) & edb <= DateChar2Num(df$endDate))
            case2a <- which(DateChar2Num(df$startDate) >= sdb & DateChar2Num(df$endDate) <= sdb)
            case2b <- which(DateChar2Num(df$startDate) >= edb & DateChar2Num(df$endDate) <= edb)
            df[unique(c(case1a, case1b, case2a, case2b)), "batchgroup"] <- batchgroupnumber
            df[unique(c(case1a, case1b, case2a, case2b)), "MNB"] <- length(unique(c(case1a, case1b, case2a, case2b)))
            df[unique(c(case1a, case1b, case2a, case2b)), "MED"] <- max(df[unique(c(case1a, case1b, case2a, case2b)), "endDate"])
            batchgroupnumber <- batchgroupnumber + 1
        }
    }
    df$rankBatch <- as.numeric(ave(df$realStartDate, df$batchgroup, FUN = rank))
    return(df)
}


dataVis <- function(queueid, show_queue = TRUE, show_group = TRUE, show_owner = TRUE, show_batch = "alias") {
    stopifnot(length(queueid) > 0)
    stopifnot(show_batch %in% c("alias", "path"))

    toreturn <- data.frame(center_x = numeric(), center_y = numeric(), height = numeric(), width = numeric()
                       , xmin = numeric(), xmax = numeric(), ymin = numeric(), ymax = numeric()
                       , color = character(), label = character(), stringsAsFactors = FALSE)

    for (i in 1:length(queueid)) {
        QR <- i - 1 # queue rank
        CQ <- queueid[i] # current queue
        # header
        EDQ <- getEDQ(queueid = CQ)
        SDQ <- getSDQ(queueid = CQ)
        RSDQ <- getRSDQ(queueid = CQ)

        # wait process
        wp <- data.frame(center_x = (SDQ+RSDQ)/2
                        , center_y = 1/2 + (QR-1)*1.5
                        , height = 1
                        , width = EDQ-SDQ
                        , xmin = SDQ
                        , xmax= RSDQ
                        , ymin = 0 + (QR-1)*1.5
                        , ymax = 1 + (QR-1)*1.5
                        , color = "grey70"
                        , label = NA
                        , ylabels = NA, stringsAsFactors = FALSE)

        # wait due to launcheR processes
        wl <- data.frame(center_x = (EDQ+RSDQ)/2
                        , center_y = 1/2 + (QR-1)*1.5
                        , height = 1
                        , width = EDQ-RSDQ
                        , xmin = RSDQ
                        , xmax = EDQ
                        , ymin = 0 + (QR-1)*1.5
                        , ymax = 1 + (QR-1)*1.5
                        , color = "wheat1"
                        , label = NA
                        , ylabels = NA, stringsAsFactors = FALSE)

        plotdf1 <- rbind(wp, wl)

        # batchs & wait batchs
        df <- addMNB(queueid = CQ)

        # Transform date
        df$startDate <- DateChar2Num(df$startDate)
        df$realStartDate <- DateChar2Num(df$realStartDate)
        df$endDate <- DateChar2Num(df$endDate)
        df$MED <- DateChar2Num(df$MED)

        # batch
        if (show_batch == "alias") {
            plotdfb <- df[,c("batchid", "batchname", "realStartDate", "endDate", "MNB", "rankBatch")]
        } else {
            plotdfb <- df[,c("batchid", "path", "realStartDate", "endDate", "MNB", "rankBatch")]
        }
        plotdfb$color <- "lightskyblue1"
        colnames(plotdfb) <- c("batchid", "label", "SD", "ED", "MNB", "RB", "color")

        # wait
        plotdfw <- df[,c("batchid", "batchname", "startDate", "realStartDate", "MNB", "rankBatch")]
        plotdfw$color <- "grey60"
        colnames(plotdfw) <- c("batchid", "label", "SD", "ED", "MNB", "RB", "color")
        plotdfw$label <- NA

        # wait // batch
        plotdfwp <- df[which(df$endDate != df$MED), c("batchid", "batchname", "endDate", "MED", "MNB", "rankBatch")]
        if (nrow(plotdfwp) > 0) {
            plotdfwp$color <- "grey90"
            colnames(plotdfwp) <- c("batchid", "label", "SD", "ED", "MNB", "RB", "color")
            plotdfwp$label <- NA
        }

        # rbind df
        fdf <- rbind(plotdfb, plotdfw, plotdfwp)

        # centers x/y
        fdf$center_x <- (fdf$SD + fdf$ED)/2
        fdf$center_y <- 1/(2*fdf$MNB) + (fdf$RB-1)/fdf$MNB + (QR-1)*1.5

        # height & width
        fdf$height <- 1/fdf$MNB
        fdf$width  <- fdf$ED - fdf$SD

        # x/y min/max
        fdf$xmin <- fdf$SD
        fdf$xmax <- fdf$ED
        fdf$ymin <- (fdf$RB-1)*(1/fdf$MNB) + (QR-1)*1.5
        fdf$ymax <- (fdf$RB)*(1/fdf$MNB) + (QR-1)*1.5

        # y labs
        fdf$ylabels <- getYlabs(queueid = CQ, show_queue = show_queue, show_group = show_group, show_owner = show_owner)

        # keep only rectangles that exists
        fdf <- fdf[which(fdf$width > 0),]
        plotdf2 <- fdf[,c("center_x", "center_y", "height", "width", "xmin", "xmax", "ymin", "ymax", "color", "label", "ylabels")]

        plotdf <- rbind(plotdf1, plotdf2)

        toreturn <- rbind(toreturn, plotdf)
    }
    return(toreturn)
}

#' @rdname vis
#' @title vis
#' @description visualize one or several queue in the time.
#' @param queueid Numeric Vector of queueid. (Default NULL)
#' @param last Numeric Number of last queue to display. Used only if queueid not specified (Default NULL)
#' @param show_xlabs Logical Display x labs. (Default TRUE)
#' @param show_ylabs Logical Display y labs. (Default TRUE)
#' @param show_queue Logical Display queue name. (Default TRUE)
#' @param show_group Logical Display queue group. (Default TRUE)
#' @param show_owner Logical Display queue owner. (Default TRUE)
#' @param show_batch Character "alias" or "path" to choose the label to display. (Default "alias")
#' @importFrom stats aggregate
#' @export
#' @examples
#' \dontrun{
#' vis(c(1:3))
#' }
vis <- function(queueid = NULL, last = NULL, show_xlabs = TRUE, show_ylabs = TRUE, show_queue = TRUE, show_group = TRUE, show_owner = TRUE, show_batch = "alias") {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package ggplot2 needed for this function to work. Please install it.", call. = FALSE)
    }
    if (is.null(queueid) & is.null(last)) {
        queueid <- max(getHistorizedBatch()$queueid)
    } else if (is.null(queueid)) {
        queueid <- unique(sort(getHistorizedBatch()$queueid, decreasing = TRUE))[1:last]
    }
    # check queueid is in historizedBatch
    df <- getHistorizedBatch(queueid = queueid)
    if (nrow(df) == 0) stop(paste("queueid(s)", paste(queueid, collapse = ", "), "don't exists"))

    # Get data
    plotdf <- dataVis(queueid = queueid, show_queue = show_queue, show_group = show_group, show_owner = show_owner, show_batch = show_batch)

    # plot rectangles
    p <- ggplot2::ggplot(plotdf) +
        ggplot2::geom_rect(ggplot2::aes_string(xmin = "xmin", xmax = "xmax", ymin = "ymin", ymax = "ymax", fill = "color"), linetype = "solid", color = "black") +
        ggplot2::scale_fill_identity(guide = "legend", name = NULL
                            , labels = c("queue waiting", "launcheR processes", "batch running", "batch waiting", "waiting // batch")
                            , breaks = c("grey70", "wheat1", "lightskyblue1", "grey60", "grey90"))

    # plot labels
    p <- p + ggplot2::geom_label(data = plotdf[which(!is.na(plotdf$label)),], ggplot2::aes_string(x = "center_x", y = "center_y", label = "label"))

    # Remove labs & add scale for date
    p <- p + ggplot2::theme( axis.title.y = ggplot2::element_blank(),
                    axis.ticks.y = ggplot2::element_blank(),
                    axis.ticks.x = ggplot2::element_blank(),
                    axis.title.x = ggplot2::element_blank()) +
             ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
             ggplot2::theme(panel.background = ggplot2::element_blank()) +
             ggplot2::theme(panel.border = ggplot2::element_blank())

    # Add scale x
    if (show_xlabs) {
        p <- p + ggplot2::scale_x_continuous(breaks = plotdf$xmin, labels = Num2DateChar(plotdf$xmin))
    } else {
        p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank())
    }
    # Add scale y
    if (show_ylabs & any(show_queue, show_group, show_owner)) {
        labs_y <- unique(plotdf[which(!is.na(plotdf$ylabels)), c("center_y", "ylabels")])
        labs_y <- aggregate(labs_y$center_y, list(labs_y$ylabels), mean)
        colnames(labs_y) <- c("label", "break")
        p <- p + ggplot2::scale_y_continuous(breaks = labs_y[,"break"], labels = labs_y[,"label"])
    } else {
        p <- p + ggplot2::theme(axis.text.y = ggplot2::element_blank())
    }
    return(p)
}

