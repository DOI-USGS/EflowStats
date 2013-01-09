#' Function to return the ML14-ML16 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge" and "year_val" and 
#' calculates ML14; the minimum annual flow for each year. ML14 is the mean of the ratios of minimum annual 
#' flows to the median flow for each year. ML15; Low flow index. ML15 is the mean of the ratios of minimum 
#' annual flows to the mean for for each year. ML16; Median of annual minimum flows. ML16 is the median of 
#' the ratios of minimum annual flows to the median flow for each year. 
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ml14.16 list of ml14-ml16 for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' ml14.16(qfiletempf)
ml14.16 <- function(qfiletempf) {
  minbyyear <- aggregate(qfiletempf$discharge, 
                         list(qfiletempf$year_val), min, na.rm=TRUE)
  medflow <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
                       median, na.rm=TRUE)
  meanflow <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), mean, na.rm=TRUE)
  computeml14 <- merge(merge(minbyyear, medflow, by.x="Group.1", by.y="Group.1"),meanflow, by.x="Group.1", by.z="Group.1")
  colnames(computeml14) <- c("year", "minbyyr", "medbyyr", "meanbyyr")
  dfml14 <- computeml14$minbyyr/computeml14$medbyyr
  dfml15 <- computeml14$minbyyr/computeml14$meanbyyr
  ml14 <- mean(dfml14)
  ml16 <- median(dfml14)
  ml15 <- mean(dfml15)
  ml14.16 <- list(ml14,ml15,ml16)
  return(ml14.16)
}