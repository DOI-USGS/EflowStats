#' Function to return the TH3 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the predictability of flow below the 60th percentile for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return tl4 numeric containing the ratio of maximum duration across years of flow below the 60th pctl to days in the year for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' th3(qfiletempf)
th3 <- function(qfiletempf) {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  frank <- floor(findrank(length(sortq), 0.40))
  lfcrit <- sortq[frank]
  qfiletempf$diff <- (qfiletempf$discharge-lfcrit)
  jul_day_sum <- aggregate(qfiletempf$diff, list(qfiletempf$jul_val), sum)
  maxdur <- rep(0,nrow(jul_day_sum))
  flag <- 0
  for (i in 1:365) {
    if (!is.na(jul_day_sum$x[i])==TRUE) {
    if (jul_day_sum$x[i]<=0) {
      flag <- flag+1
      maxdur[i]<-flag 
    }} else {
      maxdur[i] <- 0
    }
  }
  th3 <- max(maxdur)/365
  return(th3)
}