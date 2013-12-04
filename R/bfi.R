#' Function to return the base flow index for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the base flow index of the daily flow values for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return bfi numeric value of the base flow index for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' bfi(qfiletempf)
bfi <- function(qfiletempf) {
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  compbfi <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day7mean <- rollmean(subsetyr$discharge, 6, align = "right", partial = TRUE)
    min7daybyyear <- min(day7mean)
    meanflow <- mean(subsetyr$discharge)
    compbfi[i] <- min7daybyyear/meanflow 
  }
  bfi <- compbfi
  return(bfi)
}