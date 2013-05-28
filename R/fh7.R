#' Function to return the FH7 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the high flood pulse count (above 7 times the median) for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fh7 numeric value of high flood pulse count for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' fh7(qfiletempf)
fh7 <- function(qfiletempf, pref = "mean") {
  hfcrit <- 7*ma2(qfiletempf)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  counter <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    counter[i] <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>hfcrit) {
        flag <- flag+1
        counter[i] <- ifelse(flag==1,counter[i]+1,counter[i])
      } else {flag <- 0}
    }}
  if (pref == "median") {
    fh7 <- median(counter)
  }
  else {
    fh7 <- mean(counter)
  }
  return(fh7)
}