#' Function to return the TL1 and TL2 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the mean and variability of the Julian date of the annual minimum flow for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return tl1.2 list containing the mean and variability of the Julian date of the annual minimum flow  for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' tl1.2(qfiletempf)
tl1.2 <- function(qfiletempf, pref = "mean") {
  min1daybyyear <- aggregate(qfiletempf$discharge, 
                             list(qfiletempf$year_val), min, na.rm=TRUE)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momin")
  noyrs <- length(noyears$Year)
  juldaymin <- rep(0, noyrs)
  counter <- 0
  for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])) {
    counter <- counter + 1
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$year_val) == 
                         i)
    findjulday <- subset(subsetyr, subsetyr$discharge == 
                           min1daybyyear$x[counter])
    juldaymin[counter] <- findjulday$jul_val[1]
  }
  minjulday <- juldaymin
  meantl2<-mean(minjulday)
  sddtl2<-sd(minjulday)
  tl2<-(sddtl2*100)/meantl2
  if (pref == "median") {
    tl1 <- median(minjulday)
  }
  else {
    tl1 <- mean(minjulday)
  }
  tl1.2<-list(tl1=tl1,tl2=tl2)
  return(tl1.2)
}