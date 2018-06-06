library(testthat)
setwd("./tests/testthat/")
source("./nohelper-checkers.R")
# Possibility in tests_to_do :
#  - single
#  - basic
#  - intermediate
#  - advanced
tests_to_do <- c("single", "basic", "intermediate", "advanced")
clean <- FALSE

##########
# batch lists
#  - batch1.R : Sleep(5) + ls   ==> OK
#  - batch2.R : Sleep(5) + stop ==> KO
#  - batch3.R : Sleep(param) ==> OK if param set, KO if param not set
#  - batch4.R : OK if following params exists : c(p1,p2,p3), KO otherwise
batch1 <- "../files/batch1.R"
batch2 <- "../files/batch2.R"
batch3 <- "../files/batch3.R"
batch4 <- "../files/batch4.R"

print(ls())
if ("single" %in% tests_to_do) {
    ########################
    ## QB1 : one batch OK ##
    ########################
    qb1 <- randomName("testQB")
    qb1_desc <- "test : QB1"
    qb1b1 <- randomName("B")
    qb1b1_desc <- "test : QB1 - B1"
    queue_qb1 <- createQueue(name = qb1, desc = qb1_desc)
    queue_qb1 <- addBatch(object = queue_qb1, path = batch1, name = qb1b1, desc = qb1b1_desc)
    launch(queue_qb1)
}

if ("basic" %in% tests_to_do) {
    context("basics launch")

    ########################
    ## QB2 : one batch KO ##
    ########################
    qb2 <- randomName("testQB")
    qb2_desc <- "test : QB2"
    qb2b1 <- randomName("B")
    qb2b1_desc <- "test : QB2 - B1"
    queue_qb2 <- createQueue(name = qb2, desc = qb2_desc)
    queue_qb2 <- addBatch(object = queue_qb2, path = batch2, name = qb2b1, desc = qb2b1_desc)
    launch(queue_qb2)

    #######################
    ## QB3 : 2 batchs OK ##
    #######################
    qb3 <- randomName("testQB")
    qb3_desc <- "test : QB3"
    qb3b1 <- randomName("B")
    qb3b2 <- randomName("B")
    qb3b1_desc <- "test : QB3 - B1"
    qb3b2_desc <- "test : QB3 - B2"
    queue_qb3 <- createQueue(name = qb3, desc = qb3_desc)
    queue_qb3 <- addBatch(object = queue_qb3, path = batch1, name = qb3b1, desc = qb3b1_desc)
    queue_qb3 <- addBatch(object = queue_qb3, path = batch1, name = qb3b2, desc = qb3b2_desc)
    launch(queue_qb3)

    ####################################
    ## QB4 : 2 batchs KO + Abandonned ##
    ####################################
    qb4 <- randomName("testQB")
    qb4_desc <- "test : QB4"
    qb4b1 <- randomName("B")
    qb4b2 <- randomName("B")
    qb4b1_desc <- "test : QB4 - B1"
    qb4b2_desc <- "test : QB4 - B2"
    queue_qb4 <- createQueue(name = qb4, desc = qb4_desc)
    queue_qb4 <- addBatch(object = queue_qb4, path = batch2, name = qb4b1, desc = qb4b1_desc)
    queue_qb4 <- addBatch(object = queue_qb4, path = batch1, name = qb4b2, desc = qb4b2_desc)
    launch(queue_qb4)

    ############################
    ## QB5 : 2 batchs KO + OK ##
    ############################
    qb5 <- randomName("testQB")
    qb5_desc <- "test : QB5"
    qb5b1 <- randomName("B")
    qb5b2 <- randomName("B")
    qb5b1_desc <- "test : QB5 - B1"
    qb5b2_desc <- "test : QB5 - B2"
    queue_qb5 <- createQueue(name = qb5, desc = qb5_desc)
    queue_qb5 <- addBatch(object = queue_qb5, path = batch2, name = qb5b1, desc = qb5b1_desc, endIfKO = FALSE)
    queue_qb5 <- addBatch(object = queue_qb5, path = batch1, name = qb5b2, desc = qb5b2_desc)
    launch(queue_qb5)

    ############################
    ## QB6 : 2 batchs KO + KO ##
    ############################
    qb6 <- randomName("testQB")
    qb6_desc <- "test : QB6"
    qb6b1 <- randomName("B")
    qb6b2 <- randomName("B")
    qb6b1_desc <- "test : QB6 - B1"
    qb6b2_desc <- "test : QB6 - B2"
    queue_qb6 <- createQueue(name = qb6, desc = qb6_desc)
    queue_qb6 <- addBatch(object = queue_qb6, path = batch2, name = qb6b1, desc = qb6b1_desc, endIfKO = FALSE)
    queue_qb6 <- addBatch(object = queue_qb6, path = batch2, name = qb6b2, desc = qb6b2_desc)
    launch(queue_qb6)

    ################################
    ## QB7 : 1 batchs with params ##
    ################################
    qb7 <- randomName("testQB")
    qb7_desc <- "test : QB7"
    qb7b1 <- randomName("B")
    qb7b1_desc <- "test : QB7 - B1"
    queue_qb7 <- createQueue(name = qb7, desc = qb7_desc)
    queue_qb7 <- addBatch(object = queue_qb7, path = batch4, name = qb7b1, desc = qb7b1_desc, params = list(p1="coucou", p2 = 1, p3 = c(1:10)))
    launch(queue_qb7)

    ############################
    ## QB8 : queue with group ##
    ############################
    qb8 <- randomName("testQB")
    qb8_desc <- "test : QB8"
    qb8b1 <- randomName("B")
    qb8b1_desc <- "test : QB8 - B1"
    qb8g <- randomName("G")
    queue_qb8 <- createQueue(name = qb8, desc = qb8_desc, group = qb8g)
    queue_qb8 <- addBatch(object = queue_qb8, path = batch1, name = qb8b1, desc = qb8b1_desc)
    launch(queue_qb8)
}

if ("intermediate" %in% tests_to_do) {
    ###################################
    ## QI1 : 3 batchs OK wbn=(1,0,1) ##
    ###################################
    qi1 <- randomName("testQI")
    qi1_desc <- "test : QI1"
    qi1b1 <- randomName("B")
    qi1b2 <- randomName("B")
    qi1b3 <- randomName("B")
    qi1b1_desc <- "test : QI1 - B1"
    qi1b2_desc <- "test : QI1 - B2"
    qi1b3_desc <- "test : QI1 - B3"
    queue_qi1 <- createQueue(name = qi1, desc = qi1_desc)
    queue_qi1 <- addBatch(object = queue_qi1, path = batch1, name = qi1b1, desc = qi1b1_desc)
    queue_qi1 <- addBatch(object = queue_qi1, path = batch1, name = qi1b2, desc = qi1b2_desc, waitBeforeNext = FALSE)
    queue_qi1 <- addBatch(object = queue_qi1, path = batch1, name = qi1b3, desc = qi1b3_desc)
    launch(queue_qi1)

    ###########################################
    ## QI2 : 3 batchs (OK,Ab,KO) wbn=(1,0,1) ##
    ###########################################
    # Here the batch 2 goes until the end, but as the batch3 failed, then the queue is abandonned and the batch2 end up with the message abandonned (event if its not really)
    qi2 <- randomName("testQI")
    qi2_desc <- "test : QI2"
    qi2b1 <- randomName("B")
    qi2b2 <- randomName("B")
    qi2b3 <- randomName("B")
    qi2b1_desc <- "test : QI2 - B1"
    qi2b2_desc <- "test : QI2 - B2"
    qi2b3_desc <- "test : QI2 - B3"
    queue_qi2 <- createQueue(name = qi2, desc = qi2_desc)
    queue_qi2 <- addBatch(object = queue_qi2, path = batch1, name = qi2b1, desc = qi2b1_desc)
    queue_qi2 <- addBatch(object = queue_qi2, path = batch3, name = qi2b2, desc = qi2b2_desc, waitBeforeNext = FALSE, params = list(param = 30))
    queue_qi2 <- addBatch(object = queue_qi2, path = batch2, name = qi2b3, desc = qi2b3_desc)
    launch(queue_qi2)

    ###########################################
    ## QI3 : 3 batchs (KO,Ab,Ab) wbn=(1,0,1) ##
    ###########################################
    qi3 <- randomName("testQI")
    qi3_desc <- "test : QI3"
    qi3b1 <- randomName("B")
    qi3b2 <- randomName("B")
    qi3b3 <- randomName("B")
    qi3b1_desc <- "test : QI3 - B1"
    qi3b2_desc <- "test : QI3 - B2"
    qi3b3_desc <- "test : QI3 - B3"
    queue_qi3 <- createQueue(name = qi3, desc = qi3_desc)
    queue_qi3 <- addBatch(object = queue_qi3, path = batch2, name = qi3b1, desc = qi3b1_desc)
    queue_qi3 <- addBatch(object = queue_qi3, path = batch1, name = qi3b2, desc = qi3b2_desc, waitBeforeNext = FALSE)
    queue_qi3 <- addBatch(object = queue_qi3, path = batch1, name = qi3b3, desc = qi3b3_desc)
    launch(queue_qi3)

    ###########################################
    ## QI4 : 3 batchs (KO,OK,OK) wbn=(1,0,1) ##
    ###########################################
    qi4 <- randomName("testQI")
    qi4_desc <- "test : QI4"
    qi4b1 <- randomName("B")
    qi4b2 <- randomName("B")
    qi4b3 <- randomName("B")
    qi4b1_desc <- "test : QI4 - B1"
    qi4b2_desc <- "test : QI4 - B2"
    qi4b3_desc <- "test : QI4 - B3"
    queue_qi4 <- createQueue(name = qi4, desc = qi4_desc)
    queue_qi4 <- addBatch(object = queue_qi4, path = batch2, name = qi4b1, desc = qi4b1_desc, endIfKO = FALSE)
    queue_qi4 <- addBatch(object = queue_qi4, path = batch1, name = qi4b2, desc = qi4b2_desc, waitBeforeNext = FALSE)
    queue_qi4 <- addBatch(object = queue_qi4, path = batch1, name = qi4b3, desc = qi4b3_desc)
    launch(queue_qi4)

}

if ("advanced" %in% tests_to_do) {
    ###############################
    ## QA1 : 3 queues same group ##
    ###############################
    qa1 <- randomName("testQA")
    qa1_desc <- "test : QA1"
    qa1g <- randomName("G")
    qa1b1 <- randomName("B")
    qa1b1_desc <- "test : QA1-B1"
    queue_qa1 <- createQueue(name = qa1, desc = qa1_desc, group = qa1g)
    queue_qa1 <- addBatch(object = queue_qa1, path = batch3, name = qa1b1, desc = qa1b1_desc, params = list(param = 12))
    launch(queue_qa1)

    qa2 <- randomName("testQA")
    qa2_desc <- "test : QA2"
    qa2b1 <- randomName("B")
    qa2b1_desc <- "test : QA2-B1"
    queue_qa2 <- createQueue(name = qa2, desc = qa2_desc, group = qa1g)
    queue_qa2 <- addBatch(object = queue_qa2, path = batch3, name = qa2b1, desc = qa2b1_desc, params = list(param = 8))
    launch(queue_qa2)

    qa3 <- randomName("testQA")
    qa3_desc <- "test : QA3"
    qa3b1 <- randomName("B")
    qa3b1_desc <- "test : QA3-B1"
    queue_qa3 <- createQueue(name = qa3, desc = qa3_desc, group = qa1g)
    queue_qa3 <- addBatch(object = queue_qa3, path = batch3, name = qa3b1, desc = qa3b1_desc, params = list(param = 5))
    launch(queue_qa3)

}

#########################################
### Wait for all queue to be finished ###
#########################################
if (length(tests_to_do) > 0) {
    if (length(tests_to_do) == 1 & tests_to_do[1] == "single") {
        Sys.sleep(10)
    } else {
        Sys.sleep(60)
    }
}

context("test something")
print(ls())


if ("single" %in% tests_to_do) {
    context("single launch")
    test_that("QB1 : one batch OK", {
        expect_true(checkHQ(name = qb1,   desc = qb1_desc))
        expect_true(checkHB(name = qb1b1, desc = qb1b1_desc, status = "OK"))
    })
}

if ("basic" %in% tests_to_do) {
    context("basic launch")

    test_that("QB2 : one batch KO", {
        expect_true(checkHQ(name = qb2,   desc = qb2_desc))
        expect_true(checkHB(name = qb2b1, desc = qb2b1_desc, status = "KO"))
    })

    test_that("QB3 : 2 batchs OK", {
        expect_true(checkHQ(name = qb3,   desc = qb3_desc))
        expect_true(checkHB(name = qb3b1, desc = qb3b1_desc, status = "OK"))
        expect_true(checkHB(name = qb3b2, desc = qb3b2_desc, status = "OK", minStartDate = list(list(name = qb3b1, check = "end"))))
    })

    test_that("QB4 : 2 batchs KO + Abandonned", {
        expect_true(checkHQ(name = qb4,   desc = qb4_desc))
        expect_true(checkHB(name = qb4b1, desc = qb4b1_desc, status = "KO"))
        expect_true(checkHB(name = qb4b2, desc = qb4b2_desc, status = "abandonned"))
    })

    test_that("QB5 : 2 batchs KO + OK", {
        expect_true(checkHQ(name = qb5,   desc = qb5_desc))
        expect_true(checkHB(name = qb5b1, desc = qb5b1_desc, status = "KO"))
        expect_true(checkHB(name = qb5b2, desc = qb5b2_desc, status = "OK", minStartDate = list(list(name = qb5b1, check = "end"))))
    })

    test_that("QB6 : 2 batchs KO + KO", {
        expect_true(checkHQ(name = qb6,   desc = qb6_desc))
        expect_true(checkHB(name = qb6b1, desc = qb6b1_desc, status = "KO"))
        expect_true(checkHB(name = qb6b2, desc = qb6b2_desc, status = "KO", minStartDate = list(list(name = qb6b1, check = "end"))))
    })

    test_that("QB7 : 1 batchs with params", {
        expect_true(checkHQ(name = qb7,   desc = qb7_desc))
        expect_true(checkHB(name = qb7b1, desc = qb7b1_desc, status = "OK"))
    })

    test_that("QB8 : queue with group", {
        expect_true(checkHQ(name = qb8,   desc = qb8_desc, group = qb8g))
        expect_true(checkHB(name = qb8b1, desc = qb8b1_desc, status = "OK"))
    })
}

if ("intermediate" %in% tests_to_do) {
    context("intermediate launch")

    test_that("QI1 : 3 batchs OK wbn=(1,0,1)", {
        expect_true(checkHQ(name = qi1,   desc = qi1_desc))
        expect_true(checkHB(name = qi1b1, desc = qi1b1_desc, status = "OK"))
        expect_true(checkHB(name = qi1b2, desc = qi1b2_desc, status = "OK", minStartDate = list(list(name = qi1b1, check = "end")), maxStartDate = list(list(name = qi1b3, check = "end"))))
        expect_true(checkHB(name = qi1b3, desc = qi1b3_desc, status = "OK", minStartDate = list(list(name = qi1b1, check = "end")), maxStartDate = list(list(name = qi1b2, check = "end"))))
    })

    test_that("QI2 : 3 batchs (OK,OK,KO) wbn=(1,0,1)", {
        expect_true(checkHQ(name = qi2,   desc = qi2_desc))
        expect_true(checkHB(name = qi2b1, desc = qi2b1_desc, status = "OK"))
        expect_true(checkHB(name = qi2b2, desc = qi2b2_desc, status = "abandonned", minStartDate = list(list(name = qi2b1, check = "end")), maxStartDate = list(list(name = qi2b3, check = "end"))))
        expect_true(checkHB(name = qi2b3, desc = qi2b3_desc, status = "KO", minStartDate = list(list(name = qi2b1, check = "end")), maxStartDate = list(list(name = qi2b2, check = "end"))))
    })

    test_that("QI3 : 3 batchs (KO,Ab,Ab) wbn=(1,0,1)", {
        expect_true(checkHQ(name = qi3,   desc = qi3_desc))
        expect_true(checkHB(name = qi3b1, desc = qi3b1_desc, status = "KO"))
        expect_true(checkHB(name = qi3b2, desc = qi3b2_desc, status = "abandonned"))
        expect_true(checkHB(name = qi3b3, desc = qi3b3_desc, status = "abandonned"))
    })

    test_that("QI4 : 3 batchs (KO,OK,OK) wbn=(1,0,1)", {
        expect_true(checkHQ(name = qi4,   desc = qi4_desc))
        expect_true(checkHB(name = qi4b1, desc = qi4b1_desc, status = "KO"))
        expect_true(checkHB(name = qi4b2, desc = qi4b2_desc, status = "OK", minStartDate = list(list(name = qi4b1, check = "end")), maxStartDate = list(list(name = qi4b3, check = "end"))))
        expect_true(checkHB(name = qi4b3, desc = qi4b3_desc, status = "OK", minStartDate = list(list(name = qi4b1, check = "end")), maxStartDate = list(list(name = qi4b2, check = "end"))))
    })

}

if ("advanced" %in% tests_to_do) {
    context("advanced launch")

    test_that("QA1 : 3 queues same group", {
        expect_true(checkHQ(name = qa1,   desc = qa1_desc, group = qa1g))
        expect_true(checkHQ(name = qa2,   desc = qa2_desc, group = qa1g, minStartDate = list(list(name = qa1, check = "end"))))
        expect_true(checkHQ(name = qa3,   desc = qa3_desc, group = qa1g, minStartDate = list(list(name = qa2, check = "end"))))
    })

}

# Remove all tests queues
if (clean) {
    launcheR.reset(type = "historized", prefix = "testQ")
}
