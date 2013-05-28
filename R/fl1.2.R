#' Function to return the FL1 and FL2 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the low flood pulse count and variability in low flood pulse count for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fl1.2 list of low flood pulse count and variability for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' fl1.2(qfiletempf)
fl1.2 <- function(qfiletempf, pref = "mean") {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  frank <- floor(findrank(length(sortq), 0.75))
  lfcrit <- sortq[frank]
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
    if (subsetyr$discharge[j]<lfcrit) {
      flag <- flag+1
      counter[i] <- ifelse(flag==1,counter[i]+1,counter[i])
    } else {flag <- 0}
  }}
  stdevfl1 <- sd(counter)
  fl2 <- (stdevfl1 * 100)/mean(counter)
  if (pref == "median") {
    fl1 <- median(counter)
  }
  else {
    fl1 <- mean(counter)
  }
  fl1.2<-list(fl1=fl1,fl2=fl2)
  return(fl1.2)
}