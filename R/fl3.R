#' Function to return the FL3 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' FL3; Frequency of low pulse spells. Compute the average number of flow events with flows below a threshold 
#' equal to 5 percent of the mean flow value for the entire flow record. FL3 is the average (or median-Use 
#' Preference option) number of events (number of events/year-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fl3 numeric containing FL3 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' fl3(qfiletempf)
fl3 <- function(qfiletempf, pref = "mean") {
  lfcrit <- 0.05*mean(qfiletempf$discharge)
  lowflow <- subset(qfiletempf,qfiletempf$discharge<lfcrit)

  if (nrow(lowflow)>0) {
    lowCounts <- aggregate(lowflow$discharge,list(lowflow$wy_val),FUN=length)
    names(lowCounts) <- c("wy_val","count")
    lowbyyr <- data.frame(wy_val = unique(qfiletempf$wy_val))
    lowbyyr <- merge(lowbyyr,lowCounts, by = "wy_val",all=TRUE)
    lowbyyr$count[is.na(lowbyyr$count)] <- 0
    
    if (pref == "median") {
      fl3 <- round(median(lowbyyr$count),digits=2)
    }
    else {
      fl3 <- round(mean(lowbyyr$count),digits=2)
    }}
  else {
    fl3 <- 0
  }
  return(fl3)
}