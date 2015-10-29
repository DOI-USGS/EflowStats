context("DH hydrologic indicators")

###load the data upfront
load("data/sampleData.RData")
qfiletempf <- sampleData

test_that("DH1 test", {
        expect_equal(dh1(qfiletempf),1445)
})

test_that("DH2 test", {
        expect_equal(dh2(qfiletempf),858.67)
})

test_that("DH3 test", {
        expect_equal(dh3(qfiletempf),592.57)
})

test_that("DH4 test", {
        expect_equal(dh4(qfiletempf),341.77)
})

test_that("DH5 test", {
        expect_equal(dh5(qfiletempf),267.36)
})

test_that("DH6 test", {
        expect_equal(dh6(qfiletempf),9.3)
})

test_that("DH7 test", {
        expect_equal(dh7(qfiletempf),11.03)
})

test_that("DH8 test", {
        expect_equal(dh8(qfiletempf),22.26)
})

test_that("DH9 test", {
        expect_equal(dh9(qfiletempf),21.88)
})

test_that("DH10 test", {
        expect_equal(dh10(qfiletempf),20.02)
})

test_that("DH11 test", {
        expect_equal(dh11(qfiletempf),12.04)
})

test_that("DH12 test", {
        expect_equal(dh12(qfiletempf),4.94)
})

test_that("DH13 test", {
        expect_equal(dh13(qfiletempf),2.85)
})

test_that("DH14 test", {
        expect_equal(dh14(qfiletempf),c("95%"=2.39))
})

test_that("DH15.16 test", {
        expect_equal(dh15.16(qfiletempf),list(dh15=7.45,dh16=50.51))
})

test_that("DH17 test", {
        expect_equal(dh17(qfiletempf),13.04)
})

test_that("DH18 test", {
        expect_equal(dh18(qfiletempf),2.01)
})

test_that("DH19 test", {
        expect_equal(dh19(qfiletempf),1.5)
})

test_that("DH20 test", {
        expect_equal(dh20(qfiletempf),7.45)
})

test_that("DH21 test", {
        expect_equal(dh21(qfiletempf),36.53)
})

test_that("DH22 test", {
        expect_equal(dh22(qfiletempf,1158),78)
})

test_that("DH23 test", {
        expect_equal(dh23(qfiletempf, 1158),2)
})

test_that("DH24 test", {
        expect_equal(dh24(qfiletempf, 1158),107)
})


