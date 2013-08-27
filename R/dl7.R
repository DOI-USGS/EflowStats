#' Function to return the DL7 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL7; Variability of annual minimum of 3-day moving average flow. Compute the standard deviation for the 
#' minimum 3-day moving averages. DL7 is 100 times the standard deviation divided by the mean (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl7 numeric containing DL7 for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dl7(qfiletempf)
dl7 <- function(qfiletempf) {
  meandl7 <- dl2(qfiletempf, pref = "mean")
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  min3daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day3mean <- rollmean(subsetyr$discharge, 3, align = "right", 
                         na.pad = TRUE)
    min3daybyyear[i] <- min(day3mean, na.rm=TRUE)
  }
  sddl7 <- sd(min3daybyyear)
  dl7 <- (sddl7 * 100)/meandl7
  return(dl7)
}