########################
### ml function tests### 
########################

test_that("ML function tests", {
        qfiletempf <- sampleData
        
        expect_equal(ml1.12(qfiletempf),list(109,110,116.5,176,114,87.5,66,58.5,51.5,40.5,48,100)
        )
        expect_equal(ml13(qfiletempf),46.24)
        expect_equal(ml14.16(qfiletempf),list(0.33,0.26,0.33)
        )
        expect_equal(ml17(qfiletempf),0.27)
        expect_equal(ml18(qfiletempf),11.1)
        expect_equal(ml19(qfiletempf),26.33)
        expect_equal(ml20(qfiletempf),0.67)
        expect_equal(ml21(qfiletempf),10.61)
        drainarea<-56.5
        expect_equal(ml22(qfiletempf,drainarea),0.71)
})

