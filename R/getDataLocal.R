#' Function to return flow data from a locally stored file
#' 
#' This function accepts a list of sites, start and end date and returns a data frame containing requested flow data
#' 
#' @param dataPath path to directory containing data files
#' @param startdate beginning water year, will be translated to 10/01
#' @param enddate ending water year, will be translated to 09/30
#' @param sepChar string containing the datafile separator, default is comma
#' @return dataOut data frame containing requested flow data for the stations
#' @export
#' @examples
#' dataPath <- "C:/Users/jlthomps/Documents/R/JData/modeled/"
#' startdate <- "2009"
#' enddate <- "2013"
#' getDataLocal(dataPath,startdate,enddate)
getDataLocal <- function(dataPath,startDt="",endDt="",sepChar=",") {
  startdate <- paste(startdate,"10","01",sep="-")
  enddate <- paste(enddate,"09","30",sep="-")
  fileList <- system2("ls",args=dataPath,stdout=TRUE)
  for (i in 1:length(fileList)) {
    fileList[i] <- ifelse(nchar(strsplit(fileList[i],".csv"))<nchar(fileList[i]) | nchar(strsplit(fileList[i],".txt"))<nchar(fileList[i]), fileList[i],NA)
  }
  fileList <- fileList[which(!is.na(fileList))]
  drainFile <- fileList[charmatch("drain",fileList)]
  peakFiles <- fileList[charmatch("peak",fileList)]
  if (!is.na(peakFiles)) {
    sites <- fileList-peakFiles
    sites <- sites[which(!fileList %in% drainFile)]
  } else {sites <- fileList[which(!fileList %in% drainFile)]}
  dataOut <- list()
  for (i in 1:length(sites)) {
    x_obs <- read.table(paste(dataPath,sites[i],sep=""),sep=sepChar,stringsAsFactors=FALSE,header=TRUE)
    site <- x_obs[1,1]
    x_obs <- x_obs[,2:3]
    obs_data <- get_obsdata(x_obs)
    obs_data$date <- strptime(obs_data$date,format="%Y-%m-%d")
    if (nchar(startDt)>1) {obs_data<-obs_data[which(strptime(obs_data$date,"%Y-%m-%d")>=strptime(startdate,"%Y-%m-%d")),]}
    if (nchar(endDt)>1) {obs_data<-obs_data[which(strptime(obs_data$date,"%Y-%m-%d")<=strptime(enddate,"%Y-%m-%d")),]}
    obs_count<-nrow(obs_data)
    cat(paste("get_obsdata run on x_obs for site",site,obs_count,"\n",sep=" "))
    drain_area<-drainAreas$darea[which(as.numeric(drainAreas$siteNo)==as.numeric(site))]
    cat(paste("data and drainage area retrieved for site",site,drain_area,"\n",sep=" "))
    countbyyr<-aggregate(obs_data$discharge, list(obs_data$wy_val), length)
    colnames(countbyyr)<-c("wy","num_samples")
    sub_countbyyr<-subset(countbyyr,num_samples >= 365)
    if (nrow(sub_countbyyr)==0) {
      obs_data<-"No complete water years for site"
    } else {
      obs_data<-merge(obs_data,sub_countbyyr,by.x="wy_val",by.y="wy")
      obs_data<-obs_data[order(obs_data$date),]
      obs_data <- obs_data[,c('wy_val','date','discharge','month_val','year_val','day_val','jul_val')]
      }
    dataOut[[i]]<-obs_data
  }
  return(dataOut)
}