

#####################################################

#magAverage low level functions
###MA1
ma1 = function(x,...) mean(x$flow,na.rm=TRUE)


###MA2
ma2 = function(x,...) median(x$flow,na.rm=TRUE)


###MA3: There is a mean or median option in the old HITHAT, so returning both here
ma3 = function(x,...) {
        ma3_temp <- aggregate(x$flow,
                              list(x$year_val),
                              FUN = cv)
        names(ma3_temp) <- c("year_val","cv")
        
        ma3_median <- c(ma3_median = median(ma3_temp$cv))
        ma3_mean <- c(ma3_mean = mean(ma3_temp$cv))
        return(c(ma3_median,ma3_mean))
}


########################
###MA4 through MA11 
###Remove sort(x$flow) because I don't see anywhere that flow vector needs to be ordered
########################

ma4 = function(percMean,percSD,...){ percMean/percSD*100}

ma5 = function(x,...) mean(x$flow,na.rm=TRUE)/median(x$flow,na.rm=TRUE)

ma6 = function(percentiles,...) as.numeric(percentiles["90%"]/percentiles["10%"])

ma7 = function(percentiles,...) as.numeric(percentiles["80%"]/percentiles["20%"])

ma8 = function(percentiles,...) as.numeric(percentiles["75%"]/percentiles["25%"])

ma9 = function(percentiles,...) as.numeric(percentiles["10%"]-percentiles["90%"])/median(x$flow,na.rm=TRUE)

ma10 = function(percentiles,...) as.numeric(percentiles["20%"]-percentiles["80%"])/median(x$flow,na.rm=TRUE)

ma11 = function(percentiles,...) as.numeric(percentiles["25%"]-percentiles["75%"])/median(x$flow,na.rm=TRUE)



########################
###MA12 through MA23
########################
ma12.23 = function(x,pref="mean",...) {
        if(pref=="median")
        {
                ma12.23_med_temp <- aggregate(x$flow, list(x$month_val),
                                              median, na.rm=TRUE)
                names(ma12.23_med_temp) <- c("month","value")
                ma12.23_med_temp$month <- month.abb[ma12.23_med_temp$month]
                ma12.23_med_temp$variable <- paste0("MA",seq(from=12,to=23,by=1),"_",ma12.23_med_temp$month,"_median")
                
                ma12.23 <- c(ma12.23_med_temp$value)
                names(ma12.23) <- ma12.23_med_temp$variable
        } else if (pref== "mean") {
                ma12.23_mean_temp <- aggregate(x$flow, list(x$month_val),
                                               mean, na.rm=TRUE)
                names(ma12.23_mean_temp) <- c("month","value")
                ma12.23_mean_temp$month <- month.abb[ma12.23_mean_temp$month]
                ma12.23_mean_temp$variable <- paste0("MA",seq(from=12,to=23,by=1),"_",ma12.23_mean_temp$month,"_mean")
                
                ma12.23 <- c(ma12.23_mean_temp$value)
                names(ma12.23) <- ma12.23_mean_temp$variable
        }
        
        
        return(ma12.23)
}
########################
###MA24 through MA35. 
########################

ma24.35 = function(x,pref="mean",...) {
        ma24.35_temp <- aggregate(x$flow, list(x$year_val,
                                               x$month_val), FUN = cv)
        names(ma24.35_temp) <- c("year_val","month_val","cv")
        
        if(pref == "mean") {
        ma24.35_mean_temp <- aggregate(ma24.35_temp$cv, list(ma24.35_temp$month_val), FUN = mean,na.rm=TRUE)
        names(ma24.35_mean_temp) <- c("month","value")
        
        ma24.35_mean_temp$month <- month.abb[ma24.35_mean_temp$month]
        ma24.35_mean_temp$variable <- paste0("MA",seq(from=24,to=35,by=1),"_",ma24.35_mean_temp$month,"_mean")
        
        ma24.35 <- c(ma24.35_mean_temp$value)
        names(ma24.35) <- ma24.35_mean_temp$variable
        
        } else {
        ma24.35_median_temp <- aggregate(ma24.35_temp$cv, list(ma24.35_temp$month_val), FUN = median,na.rm=TRUE)
        names(ma24.35_median_temp) <- c("month","value")
        
        ma24.35_median_temp$month <- month.abb[ma24.35_median_temp$month]
        ma24.35_median_temp$variable <- paste0("MA",seq(from=24,to=35,by=1),"_",ma24.35_median_temp$month,"_median")
        
        ma24.35 <- c(ma24.35_median_temp$value)
        names(ma24.35) <- ma24.35_median_temp$variable
        }
        
        return(ma24.35)
}

########################
###MA36 through MA40. 
########################
ma36.40 = function(x,...) {
        monthlyMean <- aggregate(x$flow, list(x$month_val,x$year_val), FUN = mean, na.rm=TRUE)
        colnames(monthlyMean) <- c("month_val","year_val","meanmo")
        
        monthlyMax <- aggregate(x$flow, list(x$month_val,x$year_val), FUN = max, na.rm=TRUE)
        colnames(monthlyMax) <- c("month_val","year_val","maxmo")
        
        monthlyMin <- aggregate(x$flow, list(x$month_val,x$year_val), FUN = min, na.rm=TRUE)
        colnames(monthlyMin) <- c("month_val","year_val","minmo")
        
        percentiles <- quantile(monthlyMean$meanmo,probs=c(0.1,0.25,0.75,0.9),type=6)
        
        medMonthlyFlow <- median(monthlyMean$meanmo)
        meanMonthlyFlow <- mean(monthlyMean$meanmo)
        
        ma36 <- c(ma36 = (max(monthlyMean$meanmo)-min(monthlyMean$meanmo))/medMonthlyFlow)
        ma37 <- c(ma37 = as.numeric((percentiles["75%"]-percentiles["25%"])/medMonthlyFlow))
        ma38 <- c(ma38 = as.numeric((percentiles["90%"]-percentiles["10%"])/medMonthlyFlow))
        ma39 <- c(ma39 = (sd(monthlyMean$meanmo)*100)/meanMonthlyFlow)
        ma40 <- c(ma40 = (meanMonthlyFlow-medMonthlyFlow)/medMonthlyFlow)
        
        return(c(ma36,ma37,ma38,ma39,ma40))
}

########################
###MA41 through MA45. 
########################
ma41.45 = function(x,drainArea=NULL,...) {
        
        yearlyMean <- aggregate(x$flow, list(x$year_val), FUN = mean, na.rm=TRUE)
        colnames(yearlyMean) <- c("year_val","meanyr")
        
        percentiles <- quantile(yearlyMean$meanyr,probs=c(0.1,0.25,0.75,0.9),type=6)
        
        if(!is.null(drainArea))
        {
                ma41 <- c(ma41 = mean(yearlyMean$meanyr)/drainArea)     
        } else(ma41 <- c(ma41 = NA))
        
        medYearlyFlow <- median(yearlyMean$meanyr, na.rm=TRUE)
        meanYearlyFlow <- mean(yearlyMean$meanyr)
        
        ma42 <- c(ma42 = (max(yearlyMean$meanyr)-min(yearlyMean$meanyr))/medYearlyFlow)
        ma43 <- c(ma43 = as.numeric((percentiles["75%"]-percentiles["25%"])/medYearlyFlow))
        ma44 <- c(ma44 = as.numeric((percentiles["90%"]-percentiles["10%"])/medYearlyFlow))
        ma45 <- c(ma45 = (meanYearlyFlow-medYearlyFlow)/medYearlyFlow)
        
        return(c(ma41,ma42,ma43,ma44,ma45))
}