#' Function to return the ML17 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the base flow. Compute the mean annual flows. Compute the minimum of a 7-day moving average 
#' flow for each year and divide them by the mean annual flow for that year. ML17 is the mean (or median - 
#' use preference option) of ratios. 
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return ml17 numeric value of the mean annual flow for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' x<-read.csv(load_data)
#' ml17(x)
ml17 <- function(qfiletempf, pref = "mean") {
  bfibyyear <- bfi(qfiletempf)
  if (pref == "median") {
    ml17 <- median(bfibyyear)
  }
  else {
    ml17 <- mean(bfibyyear)
  }
  return(ml17)
}