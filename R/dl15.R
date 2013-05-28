#' Function to return the DL15 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the 90 percent exceedance value divided by the median for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl15 numeric containing the 90 percent exceedance value divided by the median for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dl15(qfiletempf)
dl15 <- function(qfiletempf) {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  frank <- floor(findrank(length(sortq), 0.90))
  hfcrit <- sortq[frank]
  medianq <- median(qfiletempf$discharge)
  dl15 <- hfcrit/medianq
  return(dl15)
}