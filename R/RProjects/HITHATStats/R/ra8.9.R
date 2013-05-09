#' Function to return the RA8 and RA9 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the variability and mean annual changes in flow from one day to the next for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ra8.9 list containing the variability and mean annual changes in flow from one day to the next for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' ra8.9(qfiletempf)
ra8.9 <- function(qfiletempf) {
  noyears <- data.frame(unique(qfiletempf$wy_val),stringsAsFactors=FALSE)
  colnames(noyears) <- c("wy_val")
  noyrs <- nrow(noyears)
  for (j in 1:noyrs) {
    subq <- subset(qfiletempf,qfiletempf$wy_val==noyears$wy_val[j])
    counter <- 0
    sub_length <- nrow(subq)-1
    for (i in 2:sub_length) {
      if (subq$discharge[i+1]-subq$discharge[i]<0 && subq$discharge[i]-subq$discharge[i-1]>0) {
        counter <- counter+1
      } else if (subq$discharge[i+1]-subq$discharge[i]>0 && subq$discharge[i]-subq$discharge[i-1]<0) {
        counter <- counter+1
      } else {counter<-counter}
    }
    noyears$cnt[j] <- counter
  }
  ra8 <- mean(noyears$cnt)
  sd_diff <- sd(noyears$cnt)
  ra9 <- (sd_diff*100)/ra8
  ra8.9 <- list(ra8=ra8,ra9=ra9)
  return(ra8.9)
}