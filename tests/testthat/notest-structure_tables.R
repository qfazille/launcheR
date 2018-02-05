context("internal table strcutures")

test_that("getWaitBatch structure", {
    df <- getWaitBatch()
    expect_is(df, "data.frame")
    expect_equal(c("batchid", "queueid", "group", "name", "parallelizable", "wait", "progress"), colnames(df))
    expect_equal(c("numeric", "numeric", "character", "character", "logcial", "numeric", "numeric"), sapply(df, class))
})

test_that("getWaitQueue structure", {
    df <- getWaitQueue()
    expect_is(df, "data.frame")
    expect_equal(c("queueid", "group", "name", "wait"), colnames(df))
    expect_equal(c("numeric", "character", "character", "numeric"), sapply(df, class))
})

test_that("getHistorized structure", {
    df <- getHistorized()
    expect_is(df, "data.frame")
    expect_equal(c("queueid", "batchid", "group", "queuename", "batchname"), colnames(df))
    expect_equal(c("numeric", "numeric", "character", "character", "character"), sapply(df, class))
})

