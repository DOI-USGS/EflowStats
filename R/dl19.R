#' Function to return the DL19 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL19; Variability in the number of zero-flow days. Compute the standard deviation for the annual number of 
#' zero-flow days. DL19 is 100 times the standard deviation divided by the mean annual number of zero-flow days 
#' (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl19 numeric containing DL19 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl19(qfiletempf)
dl19 <- function(qfiletempf) {
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
  meanzero <- mean(wy_cnts$zero_cnt)
  sdzero <- sd(wy_cnts$zero_cnt)
  dl19 <- round((100*sdzero)/meanzero,digits=2)
  } else {
    dl19 <- 0
  }
  return(dl19)
}