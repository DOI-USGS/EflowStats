context("Magnitude functions")

###load the data upfront
load("data/sampleData.RData")
qfiletempf <- sampleData

#########################
### ma function tests ###
#########################

test_that("ma1 test", {
        expect_equal(ma1(qfiletempf),151.94)
        })

test_that("ma12.23 test", {
        expect_equal(ma12.23(qfiletempf),data.frame(x=c(169.15,
                     152.58,
                     264.42,
                     273.43,
                     165.95,
                     131.87,
                     128.18,
                     83.66,
                     84.03,
                     59.02,
                     126.27,
                     185.00))
                     )
        
        expect_equal(ma12.23(qfiletempf,pref="median"),data.frame(x=c(136.0,
                                                                      144.0,
                                                                      193.0,
                                                                      247.0,
                                                                      152.0,
                                                                      118.0,
                                                                      98.5,
                                                                      74.0,
                                                                      71.0,
                                                                      48.5,
                                                                      56.5,
                                                                      141.5))
                     )
        })

test_that("ma2 test", {
        expect_equal(ma2(qfiletempf),120)
        })

test_that("ma24.35 test", {
        expect_equal(ma24.35(qfiletempf),data.frame(x=c(30,30,45,43,25,37,46,28,68,52,199,71),
                                                    digits=c(rep(2,12)))
        )
        })

test_that("ma3 test", {
        expect_equal(ma3(qfiletempf),90.33)
        })

test_that("ma36.40 test", {
        expect_equal(ma36.40(qfiletempf),list(2.19,setNames(0.61,"75%"),setNames(1.57,"90%"),52.81,0.05)
                     )
        })

test_that("ma4.11 test", {
        expect_equal(ma4.11(qfiletempf),list(56.72,1.27,5.39,3.09,2.24,1.84,1.15,0.81)
        )
        })

test_that("ma41.45 test", {
        drain_area<-56.5
        expect_equal(ma41.45(qfiletempf,drain_area),list(2.69,0,setNames(0,"75%"),setNames(0,"90%"),0)
        )
        })

########################
### mh function tests### 
########################

test_that("mh1.12 test", {
        expect_equal(mh1.12(qfiletempf),list(311,282.5,765.5,831.5,288,300.5,416,166.5,334,205,1325,737)
        )
        })

test_that("mh13 test", {
        expect_equal(mh13(qfiletempf),84.15)
        })

test_that("mh14 test", {
        expect_equal(mh14(qfiletempf),11.91)
})

test_that("mh15.17 test", {
        expect_equal(mh15.17(qfiletempf),list(setNames(6.96,"99%"),setNames(2.25,"90%"),setNames(1.46,"75%"))
        )
})

test_that("mh18 test", {
        expect_equal(mh18(qfiletempf),1.28)
})

test_that("mh19 test", {
        expect_equal(mh19(qfiletempf),Inf)
})

test_that("mh20 test", {
        drainarea<-56.5
        expect_equal(mh20(qfiletempf,drainarea),25.58)
})

test_that("mh21 test", {
        expect_equal(mh21(qfiletempf),10.48)
})

test_that("mh22 test", {
        expect_equal(mh22(qfiletempf),5.36)
})

test_that("mh23 test", {
        expect_equal(mh23(qfiletempf),3.32)
})

test_that("mh24 test", {
        expect_equal(mh24(qfiletempf),2.99)
})

test_that("mh25 test", {
        expect_equal(mh25(qfiletempf),6.72)
})

test_that("mh26 test", {
        expect_equal(mh26(qfiletempf),10.24)
})

test_that("mh27 test", {
        expect_equal(mh27(qfiletempf),3.32)
})

########################
### ml function tests### 
########################

test_that("ml1.12 test", {
        expect_equal(ml1.12(qfiletempf),list(109,110,116.5,176,114,87.5,66,58.5,51.5,40.5,48,100)
        )
})

test_that("ml13 test", {
        expect_equal(ml13(qfiletempf),46.24)
})

test_that("ml14.16 test", {
        expect_equal(ml14.16(qfiletempf),list(0.33,0.26,0.33)
        )
})

test_that("ml17 test", {
        expect_equal(ml17(qfiletempf),0.27)
})

test_that("ml18 test", {
        expect_equal(ml18(qfiletempf),11.1)
})

test_that("ml19 test", {
        expect_equal(ml19(qfiletempf),26.33)
})

test_that("ml20 test", {
        expect_equal(ml20(qfiletempf),0.67)
})

test_that("ml21 test", {
        expect_equal(ml21(qfiletempf),10.61)
})

test_that("ml22 test", {
        drainarea<-56.5
        expect_equal(ml22(qfiletempf,drainarea),0.71)
})

