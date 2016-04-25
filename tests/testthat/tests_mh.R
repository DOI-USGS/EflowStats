########################
### mh function tests### 
########################

test_that("MH function tests", {
        qfiletempf <- sampleData
        
        expect_equal(mh1.12(qfiletempf),list(311,282.5,765.5,831.5,288,300.5,416,166.5,334,205,1325,737)
        )
        expect_equal(mh13(qfiletempf),84.15)
        expect_equal(mh14(qfiletempf),11.91)
        expect_equal(mh15.17(qfiletempf),list(setNames(6.96,"99%"),setNames(2.25,"90%"),setNames(1.46,"75%"))
        )
        expect_equal(mh18(qfiletempf),1.28)
        expect_equal(mh19(qfiletempf),Inf)
        drainarea<-56.5
        expect_equal(mh20(qfiletempf,drainarea),25.58)
        expect_equal(mh21(qfiletempf),10.48)
        expect_equal(mh22(qfiletempf),5.36)
        expect_equal(mh23(qfiletempf),3.32)
        expect_equal(mh24(qfiletempf),2.99)
        expect_equal(mh25(qfiletempf),6.72)
        expect_equal(mh26(qfiletempf),10.24)
        expect_equal(mh27(qfiletempf),3.32)
})
