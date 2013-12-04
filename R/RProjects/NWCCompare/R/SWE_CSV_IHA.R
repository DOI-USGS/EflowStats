#' Function to return NWC Intercomparison portal modeled data for a given site
#' 
#' This function accepts a url and returns a data frame of daily data for that SOS request
#' 
#' @param input url for SOS service for desired site data
#' @return flow data frame containing desired time series
#' @export
#' @examples
#' url <- "http://cida.usgs.gov/nwc/thredds/sos/watersmart/stats/stats-SE-DENSE1-2.03.nc?request=GetObservation&service=SOS&version=1.0.0&offering=02178400&observedProperty=Streamflow"
#' SWE_CSV_IHA(url)
SWE_CSV_IHA <- function(input) {
  cat(paste("Retrieving data from: \n", input, "\n", 
            sep = " "))
  content<-paste(readLines(input,warn=FALSE))
  if (length(sapply(content,nchar))>1) { 
    flow <- read.delim(header = F, comment.char = "", 
                       as.is = T, sep = ",", text = xpathApply(xmlParse(input), 
                                                               "//swe:values", xmlValue)[[1]])
    nms <- c("date", "discharge")
    names(flow) <- nms
    flow$date <- as.Date(strptime(flow$date, format = "%Y-%m-%dT%H:%M:%SZ"))
    flow$discharge <- as.numeric(flow$discharge)
    flow <- as.data.frame(flow)
    attr(flow, "SRC") <- input
    class(flow) <- c("flow", "data.frame")
    cat("Finished!\n")
    return(flow)
  } else {
    cat("No data available for site\n")
    flow<-""
    return(flow)}
}