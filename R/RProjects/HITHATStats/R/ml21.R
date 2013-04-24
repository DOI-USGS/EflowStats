#' Function to return the ML21 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge" and "year_val" and 
#' calculates ML21. Variability across annual minimum flows. Compute the mean and standard deviation for 
#' the annual minimum flows. ML21 is the standard deviation times 100 divided by the mean. 
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return ml21 numeric value of the standard deviation times 100 divided by the mean for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' x<-read.csv(load_data)
#' ml21(x)
ml21 <- function(x) {
  minbyyr <- aggregate(x$discharge,list(x$wy_val),FUN=min,na.rm=TRUE)
  colnames(minbyyr) <- c("Year","yrmin")
  ml21 <- (sd(minbyyr$yrmin)*100)/mean(minbyyr$yrmin)
  return(ml21)
}