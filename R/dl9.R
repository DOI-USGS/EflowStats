#' Function to return the DL9 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL9; Variability of annual minimum of 30-day moving average flow. Compute the standard deviation for the 
#' minimum 30-day moving averages. DL9 is 100 times the standard deviation divided by the mean (percent-spatial). 
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl9 numeric containing DL9 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl9(qfiletempf)
dl9 <- function(qfiletempf) {
  meandl9 <- dl4(qfiletempf, pref = "mean")
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  min30daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day30mean <- rollmean(subsetyr$discharge, 30, align = "right", 
                         na.pad = TRUE)
    min30daybyyear[i] <- min(day30mean, na.rm=TRUE)
  }
  sddl9 <- sd(min30daybyyear)
  dl9 <- round((sddl9 * 100)/meandl9,digits=2)
  return(dl9)
}