context("DL hydrologic indicators")

###load the data upfront
load("data/sampleData.RData")
qfiletempf <- sampleData

test_that("DL1 test", {
        expect_equal(dl1(qfiletempf),40)
})

test_that("DL2 test", {
        expect_equal(dl2(qfiletempf),40.67)
})

test_that("DL3 test", {
        expect_equal(dl3(qfiletempf),41.21)
})

test_that("DL4 test", {
        expect_equal(dl4(qfiletempf),50.28)
})

test_that("DL5 test", {
        expect_equal(dl5(qfiletempf),97.13)
})

test_that("DL6 test", {
        expect_equal(dl6(qfiletempf),10.67)
})

test_that("DL7 test", {
        expect_equal(dl7(qfiletempf),10.43)
})

test_that("DL8 test", {
        expect_equal(dl8(qfiletempf),11.03)
})

test_that("DL9 test", {
        expect_equal(dl9(qfiletempf),8.67)
})

test_that("DL10 test", {
        expect_equal(dl10(qfiletempf),40.33)
})

test_that("DL11 test", {
        expect_equal(dl11(qfiletempf),0.33)
})

test_that("DL12 test", {
        expect_equal(dl12(qfiletempf),0.34)
})

test_that("DL13 test", {
        expect_equal(dl13(qfiletempf),0.42)
})

test_that("DL14 test", {
        expect_equal(dl14(qfiletempf),c("25%"=0.65))
})

test_that("DL15 test", {
        expect_equal(dl15(qfiletempf),c("10%"=0.42))
})

test_that("DL16 test", {
        expect_equal(dl16.17(qfiletempf),list(dl16=10.92,dl17=47.92))
})

test_that("DL18 test", {
        expect_equal(dl18(qfiletempf),0)
})

test_that("DL19 test", {
        expect_equal(dl19(qfiletempf),0)
})

test_that("DL20 test", {
        expect_equal(dl20(qfiletempf),0)
})



