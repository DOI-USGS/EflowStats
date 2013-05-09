#' Function to return the DL14 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the 75 percent exceedance value divided by the median for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl14 numeric containing the 75 percent exceedance value divided by the median for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dl14(qfiletempf)
dl14 <- function(qfiletempf) {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  frank <- floor(findrank(length(sortq), 0.75))
  hfcrit <- sortq[frank]
  medianq <- median(qfiletempf$discharge)
  dl14 <- hfcrit/medianq
  return(dl14)
}