#' Function to return the DL16 and DL17 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the variability and median average pulse duration for each year of flow below the 25th percentile for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl16.17 list containing the median duration of low pulse flow and variability for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dl16.17(qfiletempf)
dl16.17 <- function(qfiletempf) {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  frank <- floor(findrank(length(sortq), 0.75))
  lfcrit <- sortq[frank]
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  dur <- data.frame(Year = rep(0,nrow(qfiletempf)), dur = rep(1,nrow(qfiletempf)))
  nevents <- 0
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]<lfcrit) {
        flag <- flag+1
        nevents <- ifelse(flag==1,nevents+1,nevents)
        dur$Year[nevents] <- subsetyr$wy_val[j]
        dur$dur[nevents] <- dur$dur[nevents]+1
      } else {flag <- 0}
    }
  }
  subset_dur <- dur[1:nevents ,]
  meanbyyr <- aggregate(subset_dur$dur, list(subset_dur$Year), mean)
  dl16 <- median(meanbyyr$x)
  dl17 <- sd(meanbyyr$x)*100/mean(meanbyyr$x)
  dl16.17 <- list(dl16=dl16,dl17=dl17)
  return(dl16.17)
}