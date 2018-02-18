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
    DateChar2Num(launcheR:::getHistorizedQueue(queueid = queueid)$endDate)
}

getSDQ <- function(queueid) {
    DateChar2Num(launcheR:::getHistorizedQueue(queueid = queueid)$startDate)
}

getRSDQ <- function(queueid) {
    DateChar2Num(launcheR:::getHistorizedQueue(queueid = queueid)$realStartDate)
}

addMNB <- function(queueid) {
    df <- launcheR:::getHistorizedBatch(queueid = queueid)
    df$batchgroup <- NA
    df$MNB <- NA
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
            batchgroupnumber <- batchgroupnumber + 1
        }
    }
    df$rankBatch <- as.numeric(ave(df$realStartDate, df$batchgroup, FUN = rank))
    return(df)
}


dataVis <- function(queueid) {
    # header
    EDQ <- getEDQ(queueid = queueid)
    SDQ <- getSDQ(queueid = queueid)
    RSDQ <- getRSDQ(queueid = queueid)

    # wait process
    wp <- data.frame(center_x = (SDQ+RSDQ)/2
                    , center_y = 1/2
                    , height = 1
                    , width = EDQ-SDQ
                    , xmin = SDQ
                    , xmax= RSDQ
                    , ymin = 0
                    , ymax = 1
                    , color = "grey70"
                    , label = NA)

    # wait due to launcheR processes
    wl <- data.frame(center_x = (EDQ+RSDQ)/2
                    , center_y = 1/2
                    , height = 1
                    , width = EDQ-RSDQ
                    , xmin = RSDQ
                    , xmax = EDQ
                    , ymin = 0
                    , ymax = 1
                    , color = "wheat1"
                    , label = NA)

    plotdf1 <- rbind(wp, wl)

    # batchs & wait batchs
    df <- addMNB(queueid = queueid)

    # Transform date
    df$startDate <- DateChar2Num(df$startDate)
    df$realStartDate <- DateChar2Num(df$realStartDate)
    df$endDate <- DateChar2Num(df$endDate)

    # batch
    plotdfb <- df[,c("batchid", "batchname", "realStartDate", "endDate", "MNB", "rankBatch")]
    plotdfb$color <- "lightskyblue1"
    colnames(plotdfb) <- c("batchid", "label", "SD", "ED", "MNB", "RB", "color")

    # wait
    plotdfw <- df[,c("batchid", "batchname", "startDate", "realStartDate", "MNB", "rankBatch")]
    plotdfw$color <- "grey60"
    colnames(plotdfw) <- c("batchid", "label", "SD", "ED", "MNB", "RB", "color")
    plotdfw$label = NA

    # rbind df
    fdf <- rbind(plotdfb, plotdfw)

    # centers x/y
    fdf$center_x <- (fdf$SD + fdf$ED)/2
    fdf$center_y <- 1/(2*fdf$MNB) + (fdf$RB-1)/fdf$MNB

    # height & width
    fdf$height <- 1/fdf$MNB
    fdf$width  <- fdf$ED - fdf$SD

    # x/y min/max
    fdf$xmin <- fdf$SD
    fdf$xmax <- fdf$ED
    fdf$ymin <- (fdf$RB-1)*(1/fdf$MNB)
    fdf$ymax <- (fdf$RB)*(1/fdf$MNB)

    # keep only rectangles that exists
    fdf <- fdf[which(fdf$width > 0),]
    plotdf2 <- fdf[,c("center_x", "center_y", "height", "width", "xmin", "xmax", "ymin", "ymax", "color", "label")]

    plotdf <- rbind(plotdf1, plotdf2)
    return(plotdf)
}


vis <- function(queueid) {
    # check queueid is in historizedBatch
    df <- launcheR:::getHistorizedBatch(queueid = queueid)
    if (nrow(df) == 0) stop(paste("queueid", queueid, "doesn't exists"))

    plotdf <- dataVis(queueid)

    # plot rectangles
    p <- ggplot(plotdf) +
        geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = color), linetype = "solid", color = "black") +
        scale_fill_identity(guide = "legend", name = NULL
                            , labels = c("queue waiting", "launcheR processes", "batch running", "batch waiting")
                            , breaks = c("grey70", "wheat1", "lightskyblue1", "grey60"))

    # plot labels
    p <- p + geom_label(aes(x = center_x, y = center_y, label = label))

    # Remove labs & add scale for date
    p <- p + theme(axis.title.y = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   #axis.ticks.x = element_blank(),
                   axis.title.x = element_blank()) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(panel.background = element_blank()) +
        theme(panel.border = element_blank())

    # Add scale x
    p <- p + scale_x_continuous(breaks = plotdf$xmin, labels = Num2DateChar(plotdf$xmin))
    return(p)
}

