context("queue class - init")

run_test <- function(obj, types = c("name", "group", "folder", "tmpdir", "logdir")) {
    if ("name" %in% types) {
        expect_is(obj@name, "character")
    } else {
        expect_equal(substr(obj@name, 1, 5), "queue")
    }

    if ("group" %in% types) {
        expect_is(obj@group, "character")
    } else {
        expect_null(obj@group)
    }

    if ("tmpdir" %in% types) {
        if (!"folder" %in% types) {
            # folder must start by obj@tmpdir
            expect_equal(grep(pattern = paste0("^", obj@tmpdir, "/"), x = obj@folder, perl = TRUE), 1)
        } else {
            if (!"logdir" %in% types) {
                # logdir must start by obj@tmpdir
                expect_equal(grep(pattern = paste0("^", obj@folder, "/"), x = obj@logdir, perl = TRUE), 1)
            }
        }
    } else {
        if (!"folder" %in% types) {
            # folder must start by tempdir()
            expect_equal(grep(pattern = paste0("^", tempdir(), "/"), x = obj@folder, perl = TRUE), 1)
            if (!"logdir" %in% types) {
                # logdir must start by tempdir()
                expect_equal(grep(pattern = paste0("^", tempdir(), "/"), x = obj@logdir, perl = TRUE), 1)
            }
        }
    }
    if ("folder" %in% types) expect_is(obj@folder, "character")
    if ("tmpdir" %in% types) expect_is(obj@tmpdir, "character")
    if ("logdir" %in% types) expect_is(obj@logdir, "character")

    # folder must point to working folder
    expect_true(launcheR:::isTmpFolder(basename(obj@folder)))
    expect_is(obj@clean, "logical")
    expect_is(obj@batchs, "list")
}

# create needed folders
folders <- c("logs", "queues", "tmp")
tocreate <- folders[which(!file.exists(folders))]
if (length(tocreate) > 0) sapply(tocreate, dir.create)

test_that("basic queue creation", {
    obj <- createQueue()
    run_test(obj, types = "none")
})

test_that("queue with group", {
    obj <- createQueue(group = "test")
    run_test(obj, types = c("group"))
})

test_that("queue with group & name", {
    obj <- createQueue(name = "test_name", group = "test_group")
    run_test(obj, types = c("name", "group"))
})

test_that("queue with tmpdir", {
    obj <- createQueue(tmpdir = "./tmp")
    run_test(obj, types = c("tmpdir"))
})

test_that("queue with folder", {
    obj <- createQueue(folder = "./queues")
    run_test(obj, types = c("folder"))
})

test_that("queue with folder & tmpdir", {
    obj <- createQueue(folder = "./queues", tmpdir = "./tmp")
    run_test(obj, types = c("folder", "tmpdir"))
})

test_that("queue with logdir", {
    obj <- createQueue(logdir = "./logs")
    run_test(obj, types = c("logdir"))
})

test_that("queue with logdir & tmpdir", {
    obj <- createQueue(logdir = "./logs", tmpdir = "./tmp")
    run_test(obj, types = c("logdir", "tmpdir"))
})

test_that("queue with logdir & folder", {
    obj <- createQueue(folder = "./queues", logdir = "./logs")
    run_test(obj, types = c("logdir", "folder"))
})

test_that("queue with logdir & folder & tmpdir", {
    obj <- createQueue(folder = "./queues", logdir = "./logs", tmpdir = "./tmp")
    run_test(obj, types = c("logdir", "folder", "tmpdir"))
})

# drop needed folders
folders <- c("logs", "queues", "tmp")
todrop <- folders[which(file.exists(folders))]
if (length(todrop) > 0) sapply(todrop, function(x) unlink(x, recursive = TRUE))
