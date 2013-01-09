#' Function to return the TH1 and TH2 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the mean and variability of the Julian date of the annual maximum flow for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return th1.2 list containing the mean and variability of the Julian date of the annual maximum flow  for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' th1.2(qfiletempf)
th1.2 <- function(qfiletempf, pref = "mean") {
  max1daybyyear <- aggregate(qfiletempf$discharge, 
                             list(qfiletempf$year_val), max, na.rm=TRUE)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  juldaymax <- rep(0, noyrs)
  counter <- 0
  for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])) {
    counter <- counter + 1
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$year_val) == 
                         i)
    findjulday <- subset(subsetyr, subsetyr$discharge == 
                           max1daybyyear$x[counter])
    juldaymax[counter] <- findjulday$jul_val[1]
  }
  maxjulday <- juldaymax
  meanth2<-mean(maxjulday)
  sddth2<-sd(maxjulday)
  th2<-(sddth2*100)/meanth2
  if (pref == "median") {
    th1 <- median(maxjulday)
  }
  else {
    th1 <- mean(maxjulday)
  }
  th1.2<-list(th1=th1,th2=th2)
  return(th1.2)
}