x <- sampleData

x$year_val <- x$wy_val

mhStats <- function(x,...) {
        
        #Calculate max and medians by month and year for statistics
        flowSum_year <- dplyr::summarize(dplyr::group_by(x,year_val),
                                         maxFlow = max(discharge,na.rm=TRUE),
                                         medFlow = median(discharge,na.rm=TRUE))
        flowSum_yearMon <- dplyr::summarize(dplyr::group_by(x,year_val,month_val),
                                            maxFlow = max(discharge,na.rm=TRUE),
                                            medFlow = median(discharge,na.rm=TRUE))
        medFlow <- median(x$discharge,na.rm=TRUE)
        
        #mh1-12 indices
        if(pref == "mean")
        {
                mh1.12 <- dplyr::summarize(dplyr::group_by(flowSum_yearMon,month_val),
                                           statistic = mean(maxFlow,na.rm=TRUE))
        } else {
                mh1.12 <- dplyr::summarize(dplyr::group_by(flowSum_yearMon,month_val),
                                           statistic = median(maxFlow,na.rm=TRUE))
        }
        mh1.12$month_val <- as.character(paste0("mh",mh1.12$month_val))
        mh1.12 <- dplyr::rename(mh1.12,indice=month_val)
        
        #mh13 indice
        mh13 <- data.frame(indice = "mh13",
                           statistic = (sd(maxSum$maxFlow)*100)/mean(maxSum$maxFlow)
        )
        
        #mh14
        mh14 <- data.frame(indice = "mh14",
                           statistic = median(flowSum_year$maxFlow/flowSum_year$medFlow)
        )
        
        #mh15-17
        hfcrit <- quantile(x$discharge,probs=c(.9,0.99,0.75),type=6)
        mh15.17 <- c(mh15=as.numeric(hfcrit["99%"]/medFlow),
                     mh16=as.numeric(hfcrit["90%"]/medFlow),
                     mh17=as.numeric(hfcrit["75%"]/medFlow))
        mh15.17 <- data.frame(indice = names(mh15.17),
                              statistic = unname(mh15.17))
        
        #mh18
        log10maxbyyr <- log10(flowSum_year$maxFlow)
        mh18 <- data.frame(indice="mh18",
                           statistic = (sd(log10maxbyyr)*100)/mean(log10maxbyyr)
        )
        
        #mh19
        log_disch <- log10(flowSum_year$maxFlow)
        sumq3 <- sum(log_disch^3)
        sumq2 <- sum(log_disch^2)
        sumq <- sum(log_disch)
        num_years <- nrow(flowSum_year)
        qstd <- sd(log_disch)
        mh19 <- c((num_years*num_years*sumq3) - (3*num_years*sumq*sumq2) + (2*sumq*sumq*sumq))/
                (num_years*(num_years-1)*(num_years-2)*qstd*qstd*qstd)
        mh19 <- data.frame(indice = "mh19",
                           statistic = mh19)
        
        
        #mh20
        if(!is.null(drainArea))
        {
                if (pref == "median") {
                        mh20 <- data.frame(indice = "mh20",
                                           statistic = median(flowSum_year$maxFlow)/drainArea
                        )
                } 
                else {
                        mh20 <- data.frame(indice = "mh20",
                                           statistic = mean(flowSum_year$maxFlow)/drainArea
                        )
                }        
        } else(mh20 <- data.frame(indice = "mh20",
                                  statistic = NA)
        )
        
        ###########################
        #mh21-27
        x <- x$discharge
        
        #define thresholds, thresholds are further definer in next code chunk as 
        #a factor of the median (eg 3xmedian)
        thresholdMed <- median(x,na.rm=TRUE)
        thresholdQuant <- quantile(x,.75,type=6,na.rm=TRUE)
        
        #Identify events above designated thresholds
        eventsList <- list(mh21_24 = calcHighEvents(x,threshold=thresholdMed),
                           mh22_25 = calcHighEvents(x,threshold=thresholdMed*3),
                           mh23_26 = calcHighEvents(x,threshold=thresholdMed*7),
                           mh27 = calcHighEvents(x,threshold=thresholdQuant))
        
        #Calculate the 21-23 statistics
        mh21.23 <- lapply(eventsList[1:3],function(x,...){
                eventData <- na.omit(x)
                numEvents <- length(unique(eventData$event))
                totalFlow <- sum(eventData$flow-thresholdMed)
                totalFlow/numEvents/thresholdMed
                
        })
        mh21.23 <- unlist(mh21.23)
        names(mh21.23) <- c("mh21","mh22","mh23")
        
        #Calculate the 24-27 statistics
        mh24.27 <- lapply(eventsList,function(x,...){
                eventMax <- dplyr::group_by(x[c("flow","event")],event)
                eventMax <- dplyr::summarize(eventMax,maxQ = max(flow,na.rm=TRUE))
                eventMax <- na.omit(eventMax)
                mean(eventMax$maxQ,na.rm=TRUE)/thresholdMed
        })
        
        mh24.27 <- unlist(mh24.27)
        names(mh24.27) <- c("mh24","mh25","mh26","mh27")
        
        #Combine into one vector and ouput
        mh21.27 <- c(mh21.23,mh24.27)
        
        mh21.27 <- data.frame(indice = names(mh21.27),
                              statistic = unname(mh21.27))
        ###########################################
        
        
        #Combine all indices into 1 dataframe and return
        mhStats <- dplyr::bind_rows(mh1.12,
                                    mh13,
                                    mh14,
                                    mh15.17,
                                    mh18,
                                    mh19,
                                    mh20,
                                    mh21.27)
        return(mhStats)
}




