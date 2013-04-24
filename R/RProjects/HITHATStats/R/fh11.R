#' Function to return the FH11 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the mean number of flow events with flows above the 60th percentile for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fh11 numeric containing the median number of flow evens above the 60th percentile for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' fh11(qfiletempf)
fh11 <- function(qfiletempf, pref = "mean") {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  frank <- floor(findrank(length(sortq), 0.4))
  lfcrit <- sortq[frank]
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  hfcountbyyrfh4 <- rep(0, noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>medmin) {
        flag <- flag+1
        hfcountbyyrfh4[i] <- ifelse(flag==1,hfcountbyyrfh4[i]+1,hfcountbyyrfh4[i])
      } else {flag<-0}
    }
  }
  if (pref == "median") {
    fh11 <- median(hfcntbyyrfh4)
  }
  else {
    fh11 <- mean(hfcntbyyrfh4)
  }
  return(fh11)
}