context("internal table strcutures")

test_that("get.wait.batch structure", {
    df <- get.wait.batch()
    expect_is(df, "data.frame")
    expect_equal(c("batchid", "queueid", "group", "name", "parallelizable", "wait", "progress"), colnames(df))
    expect_equal(c("numeric", "numeric", "character", "character", "logcial", "numeric", "numeric"), sapply(df, class))
})

test_that("get.wait.queue structure", {
    df <- get.wait.queue()
    expect_is(df, "data.frame")
    expect_equal(c("queueid", "group", "name", "wait"), colnames(df))
    expect_equal(c("numeric", "character", "character", "numeric"), sapply(df, class))
})

test_that("get.historized structure", {
    df <- get.historized()
    expect_is(df, "data.frame")
    expect_equal(c("queueid", "batchid", "group", "queuename", "batchname"), colnames(df))
    expect_equal(c("numeric", "numeric", "character", "character", "character"), sapply(df, class))
})

