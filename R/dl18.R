#' Function to return the DL18 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL18; Number of zero-flow days. Count the number of zero-flow days for the entire flow record. DL18 is the 
#' mean (or median-Use Preference option) annual number of zero flow days (number of days/year-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dl18 numeric containing DL18 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl18(qfiletempf)
dl18 <- function(qfiletempf, pref = "mean") {
  subset_zero <- subset(qfiletempf,qfiletempf$discharge==0)
  if (nrow(subset_zero)>0) {
  subset_zero$discharge <- subset_zero$discharge+1
  zero_cnts <- aggregate(subset_zero$discharge, list(subset_zero$wy_val), sum)
  wy_cnts <- aggregate(qfiletempf$discharge,list(qfiletempf$wy_val),sum)
  colnames(wy_cnts) <- c("wy_val","zero_cnt")
  wy_cnts$zero_cnt <- rep(0,nrow(wy_cnts))
  wy_cnts <- merge(wy_cnts,zero_cnts,by.x="wy_val",by.y="Group.1",all.x=TRUE)
  wy_cnts$zero_cnt <- wy_cnts$zero_cnt+wy_cnts$x
  wy_cnts$zero_cnt[is.na(wy_cnts$zero_cnt)] <- 0
 
  } else {wy_cnts <- data.frame(zero_cnt=rep(0,1))}
  if (pref == "median") {
    dl18 <- round(median(wy_cnts$zero_cnt),digits=2)
  }
  else {
    dl18 <- round(mean(wy_cnts$zero_cnt),digits=2)
  }
  return(dl18)
}