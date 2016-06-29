context("timing functions")


test_that("timing function tests", {
        load("data/sampleData.rda")
        qfiletempf <- sampleData
        
        expect_equal(ta1.2(qfiletempf),list(ta1=0.64,ta2=87.03))
        expect_equal(ta3(qfiletempf, 1158),0.33)
        expect_equal(tl1.2(qfiletempf),list(tl1=264,tl2=18.92))
        expect_equal(tl3(qfiletempf, 1161.38),0.19)
        expect_equal(tl4(qfiletempf, 1161.38),0.01)
        expect_equal(th1.2(qfiletempf),list(th1=36.87,th2=65.12))
        expect_equal(th3(qfiletempf, 1158),0.62)
})

        