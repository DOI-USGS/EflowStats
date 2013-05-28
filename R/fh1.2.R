#' Function to return the FH1 and FH2 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the high flood pulse count (above 75th percentile) and variability in high flood pulse count for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fh1.2 list of high flood pulse count and variability for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' fh1.2(qfiletempf)
fh1.2 <- function(qfiletempf, pref = "mean") {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  frank <- floor(findrank(length(sortq), 0.25))
  hfcrit <- sortq[frank]
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  counter <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    counter[i] <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>hfcrit) {
        flag <- flag+1
        counter[i] <- ifelse(flag==1,counter[i]+1,counter[i])
      } else {flag <- 0}
    }}
  stdevfh1 <- sd(counter)
  fh2 <- (stdevfh1 * 100)/mean(counter)
  if (pref == "median") {
    fh1 <- median(counter)
  }
  else {
    fh1 <- mean(counter)
  }
  fh1.2<-list(fh1=fh1,fh2=fh2)
  return(fh1.2)
}