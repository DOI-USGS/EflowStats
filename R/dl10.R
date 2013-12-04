#' Function to return the DL10 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL10; Variability of annual minimum of 90-day moving average flow. Compute the standard deviation for the 
#' minimum 90-day moving averages. DL10 is 100 times the standard deviation divided by the mean (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl10 numeric containing DL10 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl10(qfiletempf)
dl10 <- function(qfiletempf) {
  meandl10 <- dl5(qfiletempf, pref = "mean")
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  min90daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day90mean <- rollmean(subsetyr$discharge, 90, align = "right", 
                         na.pad = FALSE)
    min90daybyyear[i] <- min(day90mean, na.rm=TRUE)
  }
  sddl10 <- sd(min90daybyyear)
  dl10 <- (sddl10 * 100)/meandl10
  return(dl10)
}