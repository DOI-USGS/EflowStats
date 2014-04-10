#' Function to return the MH21 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates MH21, high flow volume index. Compute the average volume for flow events above a threshold equal to 
#' the median flow for the entire record. MH21 is the average volume divided by the median flow for the entire 
#' record (days-temporal).
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return mh21 numeric value of MH21 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' mh21(qfiletempf)
mh21 <- function(x) {
  thresh <- ma2(x)
  nevents <- 0
  flag <- 0
  total <- 0
  for (i in 1:nrow(x)) {
    temp <- x$discharge[i]-thresh
    if (temp>0) {
      total <- total + temp
      flag <- flag+1
      nevents <- ifelse(flag==1,nevents+1,nevents)
    } else {flag <- 0}
  }
  avg_ex <- total/nevents
  mh21 <- round(avg_ex/thresh,digits=2)
  return(mh21)
}