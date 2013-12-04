#' Function to return the base flow index for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the base flow index of the daily flow values for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return bfi numeric value of the base flow index for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' bfi(qfiletempf)
bfi <- function(qfiletempf) {
  day7mean <- rollmean(qfiletempf$discharge, 7, align = "right", 
                       na.pad = TRUE)
  rollingavg <- data.frame(qfiletempf, day7mean)
  rollingavgs7day <- subset(rollingavg, rollingavg$day7mean != 
                              "NA")
  min7daybyyear <- aggregate(rollingavgs7day$day7mean, 
                             list(rollingavgs7day$year_val), min)
  meanflow <- meanflowbyyear(qfiletempf)
  compbfi <- merge(min7daybyyear, meanflow, by.x = "Group.1", by.y = "Year")
  colnames(compbfi) <- c("year", "day7min", "meandaily")
  bfi <- compbfi$day7min/compbfi$meandaily
  return(bfi)
}