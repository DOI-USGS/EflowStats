context("DH hydrologic indicators")

test_that("DH tests", {
        ###load the data upfront#
        load("data/sampleData.RData")
        
        qfiletempf <- sampleData
        
        expect_equal(dh1(qfiletempf),1445)
        expect_equal(dh2(qfiletempf),858.67)
        expect_equal(dh3(qfiletempf),592.57)
        expect_equal(dh4(qfiletempf),341.77)
        expect_equal(dh5(qfiletempf),267.36)
        expect_equal(dh6(qfiletempf),9.3)
        expect_equal(dh7(qfiletempf),11.03)
        expect_equal(dh8(qfiletempf),22.26)
        expect_equal(dh9(qfiletempf),21.88)
        expect_equal(dh10(qfiletempf),20.02)
        expect_equal(dh11(qfiletempf),12.04)
        expect_equal(dh12(qfiletempf),4.94)
        expect_equal(dh13(qfiletempf),2.85)
        expect_equal(dh14(qfiletempf),c("95%"=2.39))
        expect_equal(dh15.16(qfiletempf),list(dh15=7.45,dh16=50.51))
        expect_equal(dh17(qfiletempf),13.04)
        expect_equal(dh18(qfiletempf),2.01)
        expect_equal(dh19(qfiletempf),1.5)
        expect_equal(dh20(qfiletempf),7.45)
        expect_equal(dh21(qfiletempf),36.53)
        expect_equal(dh22(qfiletempf,1158),78)
        expect_equal(dh23(qfiletempf, 1158),2)
        expect_equal(dh24(qfiletempf, 1158),107)
})


