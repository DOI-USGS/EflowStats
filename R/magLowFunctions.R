#' Function to return the ML1-ML12 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains columns named "flow", "year_val" and "month_val" and 
#' calculates the mean (or median-Use Preference option) minimum flows for each month across all years. Compute 
#' the minimum daily flow for each month over the entire flow record. For example, ml1 is the mean of the minimums 
#' of all January flow values over the entire record (cubic feet per second-temporal).
#' 
#' @param x data frame containing "flow", "year_val", and "month_val" columns containing daily flow values, year, and month
#' @return named numeric vector containing the mean or median maximum flows for each month
#' @import dplyr
#' @examples
#' x<-sampleData
#' ml1.12(x)
ml1.12 <- function(x,pref="mean",...) {
        minbymonyr <- aggregate(x$flow, list(x$year_val, x$month_val), FUN = min, na.rm=TRUE)
        colnames(minbymonyr) <- c("Year","Month","minmo")
        if(pref == "mean")
        {
                meanminbymon <- aggregate(minbymonyr$minmo, list(minbymonyr$Month), FUN = mean, na.rm=TRUE)
                ml1.12 <- c(ml1_Jan_meanmin = meanminbymon[1,2],
                            ml2_Feb_meanmin = meanminbymon[2,2],
                            ml3_Mar_meanmin = meanminbymon[3,2],
                            ml4_Apr_meanmin = meanminbymon[4,2],
                            ml5_May_meanmin = meanminbymon[5,2],
                            ml6_Jun_meanmin = meanminbymon[6,2],
                            ml7_Jul_meanmin = meanminbymon[7,2],
                            ml8_Aug_meanmin = meanminbymon[8,2],
                            ml9_Sep_meanmin = meanminbymon[9,2],
                            m10_Oct_meanmin = meanminbymon[10,2],
                            ml11_Nov_meanmin = meanminbymon[11,2],
                            ml12_Dec_meanmin = meanminbymon[12,2])
                return(ml1.12)
        } else {
                medianminbymon <- aggregate(minbymonyr$minmo, list(minbymonyr$Month), FUN = median, na.rm=TRUE)
                ml1.12 <- c(ml1_Jan_medianmin = medianminbymon[1,2],
                            ml2_Feb_medianmin = medianminbymon[2,2],
                            ml3_Mar_medianmin = medianminbymon[3,2],
                            ml4_Apr_medianmin = medianminbymon[4,2],
                            ml5_May_medianmin = medianminbymon[5,2],
                            ml6_Jun_medianmin = medianminbymon[6,2],
                            ml7_Jul_medianmin = medianminbymon[7,2],
                            ml8_Aug_medianmin = medianminbymon[8,2],
                            ml9_Sep_medianmin = medianminbymon[9,2],
                            m10_Oct_medianmin = medianminbymon[10,2],
                            ml11_Nov_medianmin = medianminbymon[11,2],
                            ml12_Dec_medianmin = medianminbymon[12,2])
                return(ml1.12)
        }
}

#' Function to return the ML13 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge","month_val" and "year_val" and 
#' calculates the Variability (coefficient of variation) across minimum monthly flow values. Compute the mean 
#' and standard deviation for the minimum monthly flows over the entire flow record. ML13 is the standard deviation 
#' times 100 divided by the mean minimum monthly flow for all years (percent-spatial).
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return ml13 numeric value of ML13 for the given data frame
#' @examples
#' x<-sampleData
#' ml13(x)
ml13 <- function(x) {
        minmonbyyr <- aggregate(x$discharge, list(x$year_val, 
                                                           x$month_val), FUN = min, na.rm=TRUE)
        colnames(minmonbyyr) <- c("Year", "Month", "minmo")
        sdminmonflows <- sd(minmonbyyr$minmo)
        meanminmonflows <- mean(minmonbyyr$minmo)
        ml13 <- c(ml13=(sdminmonflows * 100)/meanminmonflows)
        return(ml13)
}

#' Function to return the ML14-ML16 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge" and "year_val" and calculates 
#' ML14; Mean of annual minimum annual flows.  ML14 is the mean of the ratios of minimum annual flows to the median 
#' flow for each year (dimensionless-temporal). 
#' ML15; Low flow index.  ML15 is the mean (or median-Use Preference option) of the ratios of minimum annual flows to 
#' the mean flow for each year (dimensionless-temporal). 
#' ML16; Median of annual minimum flows.  ML16 is the median of the ratios of minimum annual flows to the median 
#' flow for each year (dimensionless-temporal). 
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return ml14.16 list of ml14-ml16 for the given data frame
#' @import dplyr
#' @examples
#' x<-sampleData
#' ml14.16(x)
ml14.16 <- function(x) {
        
        minbyyear <- aggregate(x$discharge, 
                               list(x$year_val), min, na.rm=TRUE)
        medflow <- aggregate(x$discharge, list(x$year_val), 
                             median, na.rm=TRUE)
        meanflow <- aggregate(x$discharge, list(x$year_val), mean, na.rm=TRUE)
        computeml14 <- merge(merge(minbyyear, medflow, by.x="Group.1", by.y="Group.1"),meanflow, by.x="Group.1", by.z="Group.1")
        colnames(computeml14) <- c("year", "minbyyr", "medbyyr", "meanbyyr")
        dfml14 <- computeml14$minbyyr/computeml14$medbyyr
        dfml15 <- computeml14$minbyyr/computeml14$meanbyyr
        ml14 <- mean(dfml14)
        ml16 <- median(dfml14)
        ml15 <- mean(dfml15)
        ml14.16 <- c(ml14=ml14,ml15=ml15,ml16=ml16)
        return(ml14.16)
}
