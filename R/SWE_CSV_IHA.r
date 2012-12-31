# wps.des: id=IHA_mindate, title= IHA minDate, abstract = Finds the minimum date in the input dataset.
# wps.in: input, text, 
# SOS Endpoint, A fully formed SOS GetObservations request that will return a SWE common CSV block holding date and flow.,
# value="null";

SWE_CSV_IHA <-
    function(input)
{
  cat(paste("Retrieving data from: \n", input, "\n",sep=" "))
  flow <- read.delim(header = F, comment.char = "", as.is = T, sep=",",text=xpathApply(xmlParse(input),"//swe:values", xmlValue)[[1]])
  nms <- c("date","discharge")
  names(flow) <- nms
  flow$date<- as.POSIXct(strptime(flow$date, format="%Y-%m-%dT%H:%M:%SZ"))
  flow$discharge <- as.numeric(flow$discharge)
  flow <- as.data.frame(flow)
  attr(flow, 'SRC') <- input
  class(flow) <- c('flow', 'data.frame')
  cat("Finished!\n")
  return(flow)
}
setwd('/Users/dblodgett/Desktop/r_testing/')
x<-SWE_CSV_IHA('swe_xml_payload')
y<-mindate(x$date, julian=F)
# wps.out: y, double, minDate, The earliest date found in the data.;