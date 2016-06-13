context("DL hydrologic indicators")


test_that("DL function tests", {
        load("data/sampleData.rda")
        
        qfiletempf <- sampleData
        
        expect_equal(dl1(qfiletempf),40)
        expect_equal(dl2(qfiletempf),40.67)
        expect_equal(dl3(qfiletempf),41.21)
        expect_equal(dl4(qfiletempf),50.28)
        expect_equal(dl5(qfiletempf),97.13)
        expect_equal(dl6(qfiletempf),10.61)
        expect_equal(dl7(qfiletempf),10.43)
        expect_equal(dl8(qfiletempf),11.03)
        expect_equal(dl9(qfiletempf),8.67)
        expect_equal(dl10(qfiletempf),40.33)
        expect_equal(dl11(qfiletempf),0.33)
        expect_equal(dl12(qfiletempf),0.34)
        expect_equal(dl13(qfiletempf),0.42)
        expect_equal(dl14(qfiletempf),c("25%"=0.65))
        expect_equal(dl15(qfiletempf),c("10%"=0.42))
        expect_equal(dl16.17(qfiletempf),list(dl16=10.92,dl17=47.92))
        expect_equal(dl18(qfiletempf),0)
        expect_equal(dl19(qfiletempf),0)
        expect_equal(dl20(qfiletempf),0)
})



