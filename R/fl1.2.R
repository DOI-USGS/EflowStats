#' Function to return the FL1 and FL2 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' FL1; Low flood pulse count. Compute the average number of flow events with flows below a threshold equal to the 
#' 25th percentile value for the entire flow record. FL1 is the average (or median-Use Preference option) number of 
#' events (number of events/year-temporal). and 
#' FL2; Variability in low pulse count. Compute the standard deviation in the annual pulse counts for FL1. FL2 is 
#' 100 times the standard deviation divided by the mean pulse count (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fl1.2 list of FL1 and FL2 for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' fl1.2(qfiletempf)
fl1.2 <- function(qfiletempf, pref = "mean") {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  lfcrit <- quantile(sortq,.25,type=6)
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
    if (subsetyr$discharge[j]<lfcrit) {
      flag <- flag+1
      counter[i] <- ifelse(flag==1,counter[i]+1,counter[i])
    } else {flag <- 0}
  }}
  stdevfl1 <- sd(counter)
  fl2 <- (stdevfl1 * 100)/mean(counter)
  if (pref == "median") {
    fl1 <- median(counter)
  }
  else {
    fl1 <- mean(counter)
  }
  fl1.2<-list(fl1=fl1,fl2=fl2)
  return(fl1.2)
}