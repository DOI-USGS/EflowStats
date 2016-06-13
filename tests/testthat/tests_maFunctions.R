context("MA function tests")

test_that("MA tests", {
        ###load the data upfront#
        load("data/sampleData.rda")
        
        qfiletempf <- sampleData
        
        expect_equal(ma1(qfiletempf),151.94)
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
        expect_equal(ma2(qfiletempf),120)
        expect_equal(ma24.35(qfiletempf),data.frame(x=c(30,30,45,43,25,37,46,28,68,52,199,71),
                                                    digits=c(rep(2,12)))
        )
        expect_equal(ma3(qfiletempf),90.33)
        expect_equal(ma36.40(qfiletempf),list(2.19,setNames(0.61,"75%"),setNames(1.57,"90%"),52.81,0.05)
        )
        expect_equal(ma4.11(qfiletempf),list(56.72,1.27,5.39,3.09,2.24,1.84,1.15,0.81)
        )
        
        drain_area<-56.5
        expect_equal(ma41.45(qfiletempf,drain_area),list(2.69,0,setNames(0,"75%"),setNames(0,"90%"),0)
        )
})
