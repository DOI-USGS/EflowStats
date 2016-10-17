#' Function to return the MH1-MH12 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge", "year_val" and "month_val" and 
#' calculates the mean (or median-Use Preference option) maximum flows for each month across all years. Compute 
#' the maximum daily flow for each month over the entire flow record. For example, MH1 is the mean of the maximums 
#' of all January flow values over the entire record (cubic feet per second-temporal).
#' 
#' @param x data frame containing "discharge", "year_val", and "month_val" columns containing daily flow values, year, and month
#' @return mh1.12 data frame containing the mean or median maximum flows for each month
#' @examples
#' x<-sampleData
#' mh1.12(x)
mh1.12 <- function(x,pref="mean") {
        maxbymonyr <- aggregate(x$discharge, list(x$year_val, x$month_val), FUN = max, na.rm=TRUE)
        colnames(maxbymonyr) <- c("Year","Month","maxmo")
        if(pref = "mean")
        {
        meanmaxbymon <- aggregate(maxbymonyr$maxmo, list(maxbymonyr$Month), FUN = mean, na.rm=TRUE)
        mh1 <- c(mh1_Jan_meanMax = round(meanmaxbymon[1,2],digits=2))
        mh2 <- c(mh2_Feb_meanMax = round(meanmaxbymon[2,2],digits=2))
        mh3 <- c(mh3_Mar_meanMax = round(meanmaxbymon[3,2],digits=2))
        mh4 <- c(mh4_Apr_meanMax = round(meanmaxbymon[4,2],digits=2))
        mh5 <- c(mh5_May_meanMax = round(meanmaxbymon[5,2],digits=2))
        mh6 <- c(mh6_Jun_meanMax = round(meanmaxbymon[6,2],digits=2))
        mh7 <- c(mh7_Jul_meanMax = round(meanmaxbymon[7,2],digits=2))
        mh8 <- c(mh8_Aug_meanMax = round(meanmaxbymon[8,2],digits=2))
        mh9 <- c(mh9_Sep_meanMax = round(meanmaxbymon[9,2],digits=2))
        mh10 <- c(m10_Oct_meanMax = round(meanmaxbymon[10,2],digits=2))
        mh11 <- c(mh11_Nov_meanMax = round(meanmaxbymon[11,2],digits=2))
        mh12 <- c(mh12_Dec_meanMax = round(meanmaxbymon[12,2],digits=2))
        mh1.12 <- c(mh1,mh2,mh3,mh4,mh5,mh6,mh7,mh8,mh9,mh10,mh11,mh12)
        return(mh1.12)
        } else {
                medmaxbymon <- aggregate(maxbymonyr$maxmo, list(maxbymonyr$Month), FUN = median, na.rm=TRUE)
                mh1 <- c(mh1_Jan_medMax = round(medmaxbymon[1,2],digits=2))
                mh2 <- c(mh2_Feb_medMax = round(medmaxbymon[2,2],digits=2))
                mh3 <- c(mh3_Mar_medMax = round(medmaxbymon[3,2],digits=2))
                mh4 <- c(mh4_Apr_medMax = round(medmaxbymon[4,2],digits=2))
                mh5 <- c(mh5_May_medMax = round(medmaxbymon[5,2],digits=2))
                mh6 <- c(mh6_Jun_medMax = round(medmaxbymon[6,2],digits=2))
                mh7 <- c(mh7_Jul_medMax = round(medmaxbymon[7,2],digits=2))
                mh8 <- c(mh8_Aug_medMax = round(medmaxbymon[8,2],digits=2))
                mh9 <- c(mh9_Sep_medMax = round(medmaxbymon[9,2],digits=2))
                mh10 <- c(m10_Oct_medMax = round(medmaxbymon[10,2],digits=2))
                mh11 <- c(mh11_Nov_medMax = round(medmaxbymon[11,2],digits=2))
                mh12 <- c(mh12_Dec_medMax = round(medmaxbymon[12,2],digits=2))
                mh1.12 <- c(mh1,mh2,mh3,mh4,mh5,mh6,mh7,mh8,mh9,mh10,mh11,mh12)
                return(mh1.12)
        }
}

#' Function to return the MH13 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge", "year_val" and "month_val" and 
#' calculates MH13, variability (coefficient of variation) across maximum monthly flow values. Compute the mean 
#' and standard deviation for the maximum monthly flows over the entire flow record. MH13 is the standard deviation 
#' times 100 divided by the mean maximum monthly flow for all years (percent-spatial).
#' 
#' @param x data frame containing "discharge", "year_val", and "month_val" columns containing daily flow values, year, and month
#' @return mh13 numeric value of MH13 for the given data frame
#' @examples
#' qfiletempf<-sampleData
#' mh13(qfiletempf)
mh13 <- function(x) {
        maxmonbyyr <- aggregate(x$discharge, list(x$year_val, 
                                                           x$month_val), FUN = max, na.rm=TRUE)
        colnames(maxmonbyyr) <- c("Year", "Month", "maxmo")
        sdmaxmonflows <- sd(maxmonbyyr$maxmo)
        meanmaxmonflows <- mean(maxmonbyyr$maxmo)
        mh13 <- c(mh13 = round((sdmaxmonflows * 100)/meanmaxmonflows,digits=2))
        return(mh13)
}