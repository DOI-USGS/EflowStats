#' Function to return the DL8 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL8; Variability of annual minimum of 7-day moving average flow. Compute the standard deviation for the 
#' minimum 7-day moving averages. DL8 is 100 times the standard deviation divided by the mean (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl8 numeric containing DL8 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl8(qfiletempf)
dl8 <- function(qfiletempf) {
  meandl8 <- dl3(qfiletempf, pref = "mean")
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  min7daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day7mean <- rollmean(subsetyr$discharge, 7, align = "right", 
                         na.pad = TRUE)
    min7daybyyear[i] <- min(day7mean, na.rm=TRUE)
  }
  sddl8 <- sd(min7daybyyear)
  dl8 <- round((sddl8 * 100)/meandl8,digits=2)
  return(dl8)
}