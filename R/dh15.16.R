#' Function to return the DH15 and DH16 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DH15; High flow pulse duration. Compute the average duration for flow events with flows above a threshold equal 
#' to the 75th percentile value for each year in the flow record. DH15 is the median of the yearly average durations 
#' (days/year-temporal). and 
#' DH16; Variability in high flow pulse duration. Compute the standard deviation for the yearly average high pulse 
#' durations. DH16 is 100 times the standard deviation divided by the mean of the yearly average high pulse durations 
#' (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dh15.16 list containing DH15 and DH16 for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data, stringsAsFactors=FALSE)
#' dh15.16(qfiletempf)
dh15.16 <- function(qfiletempf) {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  frank <- floor(findrank(length(sortq), 0.25))
  lfcrit <- sortq[frank]
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  hfdur <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    pdur <- 0
    nevents <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>lfcrit) {
        flag <- flag+1
        nevents <- ifelse(flag==1,nevents+1,nevents)
        pdur <- pdur+1
      } else {flag <- 0}
    }
    if (nevents>0) {hfdur[i]<-pdur/nevents}
  }
  dh15 <- median(hfdur)
  dh16 <- (sd(hfdur)*100)/mean(hfdur)
  dh15.16 <- list(dh15=dh15,dh16=dh16)
  return(dh15.16)
}