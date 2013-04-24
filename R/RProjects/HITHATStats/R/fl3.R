#' Function to return the FL3 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the average number of events with flows below 5% of mean for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fl3 numeric containing the average number of events with flows below 5% of mean for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' fl3(qfiletempf)
fl3 <- function(qfiletempf, pref = "mean") {
  lfcrit <- 0.05*mean(qfiletempf$discharge)
  lowflow <- subset(qfiletempf,qfiletempf$discharge<lfcrit)
  if (nrow(lowflow)>0) {
    lowbyyr <- aggregate(lowflow$discharge,list(lowflow$wy_val),FUN=length)
    if (pref == "median") {
      fl3 <- median(lowbyyr$x)
    }
    else {
      fl3 <- mean(lowbyyr$x)
    }}
  else {
    fl3 <- 'NA'
  }
  return(fl3)
}