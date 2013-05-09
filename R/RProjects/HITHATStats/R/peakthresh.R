#' Function to return the 1.67-year flood threshold using data from the NWISWeb peak values service
#' 
#' This function accepts obs_data and peak_data data frames that contain observed data and peak annual flow data for the desired 
#' NWIS site. It returns a value 'thresh' that is the 1.67-year flood threshold for the site
#' 
#' @param obs_data data frame containing processed NWIS daily flow data for the site
#' @param peakDaily data frame containing NWIS annual peak flow data for the site
#' @return thresh numeric containing the 1.67-year flood threshold
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' obs_data<-read.csv(load_data,stringsAsFactors=FALSE)
#' sites<-"02178400"
#' peakDaily<-getPeakData(sites)
#' getPeakThresh(obs_data,peakDaily)
getPeakThresh <- function(obs_data,peakDaily) {
peakDaily <- aggregate(obs_data$discharge,list(obs_data$wy_val),max)
colnames(peakDaily) <- c("wy_val","discharge")
peakInst <- peakValues[as.numeric(peakValues$wy_val)>=min(as.numeric(peakDaily$wy_val)) & as.numeric(peakValues$wy_val)<=max(as.numeric(peakDaily$wy_val)),]
peakDaily$logval <- log10(peakDaily$discharge)
dailyMean <- mean(peakDaily$logval)
instMean <- mean(peakInst$logval)
num <- sum((peakDaily$logval-dailyMean)*(peakInst$logval-instMean))
denom <- sum((peakInst$logval-instMean)*(peakInst$logval-instMean))
b <- num/denom
a <- dailyMean - (b*instMean)
isolateq <- peakInst$logval
sortq <- sort(isolateq)
frank <- floor(findrank(length(sortq), 0.6))
lfcrit <- sortq[frank]
lq167 <- a + (b*lfcrit)
thresh <- 10^(lq167)
return(thresh)
}