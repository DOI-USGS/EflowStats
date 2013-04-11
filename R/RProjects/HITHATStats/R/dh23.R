#' Function to return the DH23 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the mean duration of flow events above the 60th percentile for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dh23 list containing the mean annual flood days for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dh23(qfiletempf)
dh23 <- function(qfiletempf) {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  frank <- floor(findrank(length(sortq), 0.40))
  lfcrit <- sortq[frank]
  subset_crit <- subset(qfiletempf, qfiletempf$discharge>lfcrit)
  num_year <- aggregate(subset_crit$discharge, list(subset_crit$year_val), function(x) sum(!is.na(x)))
  dh23 <- mean(num_year$x)
  return(dh23)
}