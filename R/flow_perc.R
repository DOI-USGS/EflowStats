#' Function to return the requested flow percentiles for a given data series
#' 
#' This function accepts a data frame containing daily data and a list of desired percentiles and 
#' returns a list of the requested percentiles
#' 
#' @param data data frame containing value data for the chosen timeseries
#' @param probs vector containing requested percentile value(s)
#' @return obs_percentiles requested flow percentiles for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' flow_perc(qfiletempf,probs=c(.1,.25,.5,.75))
flow_perc <- function(data,probs=c(.1,.25,.5,.75,.9,.15)) {
obs_percentiles <- quantile(data$discharge,probs,na.rm=TRUE)
return(obs_percentiles)
}