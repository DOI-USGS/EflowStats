#' Function to return the ML19 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge" and "year_val" and 
#' calculates the base flow. Compute the ratios of the minimum annual flow to mean annual flow for 
#' each year. ML19 is the mean (or median - use preference option) of these ratios times 100.
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return ml19 numeric value of the min annual flow/mean annual flow for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' ml19(qfiletempf)
ml19 <- function(qfiletempf, pref = "mean") {
  minbyyr <- aggregate(qfiletempf$discharge,list(qfiletempf$year_val),FUN=min,na.rm=TRUE)
  colnames(minbyyr) <- c("Year","yrmin")
  meanbyyr <- aggregate(qfiletempf$discharge,list(qfiletempf$year_val),FUN=mean,na.rm=TRUE)
  colnames(meanbyyr) <- c("Year","yrmean")
  ratiominmean <- (minbyyr$yrmin/meanbyyr$yrmean)*100
  if (pref == "median") {
    ml19 <- median(ratiominmean)
  }
  else {
    ml19 <- mean(ratiominmean)
  }
  return(ml19)
}