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
    lowbyyr <- aggregate(lowflow$discharge,list(lowflow$wy_val),FUN=length)
    if (pref == "median") {
      fl3 <- round(median(lowbyyr$x),digits=2)
    }
    else {
      fl3 <- round(mean(lowbyyr$x),digits=2)
    }}
  else {
    fl3 <- 0
  }
  return(fl3)
}