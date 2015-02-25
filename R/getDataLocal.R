#' Function to return flow data from a locally stored file
#' 
#' This function accepts a list of sites, start and end date and returns a data frame containing requested flow data
#' 
#' @param dataPath path to directory containing data files
#' @param startDt beginning water year, will be translated to 10/01
#' @param endDt ending water year, will be translated to 09/30
#' @param sepChar string containing the datafile separator, default is comma
#' @return dataOut data frame containing requested flow data for the stations
#' @export
#' @examples
#' dataPath <- system.file("extdata", package="EflowStats")
#' dataPath <- paste(dataPath, "modeled", sep="/")
#' startdate <- "2009"
#' enddate <- "2013"
#' localData <- getDataLocal(dataPath,startDt=startdate,endDt=enddate)
getDataLocal <- function(dataPath,startDt="",endDt="",sepChar=",") {
  if (nchar(startDt)>1) {startdate <- paste(startDt,"10","01",sep="-")}
  if (nchar(endDt)>1) {enddate <- paste(endDt,"09","30",sep="-")}
  fileList <- list.files(path=dataPath)
  for (i in 1:length(fileList)) {
    fileList[i] <- ifelse(nchar(strsplit(fileList[i],".csv"))<nchar(fileList[i]) | nchar(strsplit(fileList[i],".txt"))<nchar(fileList[i]), fileList[i],NA)
  }
  
  dateFormatCheck <- function(date){  # checks for the format YYYY-MM-DD
    parts <- strsplit(date,"-",fixed=TRUE)
    condition <- FALSE
    if (length(parts[[1]])>1) {
      if (nchar(parts[[1]][1]) == 4 && nchar(parts[[1]][2]) == 2 && nchar(parts[[1]][3]) == 2){
        testYear <- as.numeric(parts[[1]][1])
        testMonth <- as.numeric(parts[[1]][2])
        testDay <- as.numeric(parts[[1]][3])
        if (!is.na(testYear) && !is.na(testMonth) && !is.na(testDay)){
          if (testMonth <= 12 && testDay <= 31){
            condition <- TRUE
          }        
        }      
      }
    }
    return(condition)
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
    x_obs <- read.table(file.path(dataPath,sites[i]),
                        sep=sepChar,stringsAsFactors=FALSE,
                        header=TRUE, colClasses = "character")
    site <- x_obs[1,1]
    x_obs <- x_obs[,2:3]
    x_obs[,2] <- as.numeric(x_obs[,2])
    
    if(!dateFormatCheck(x_obs[,1])){
      x_obs[,1] <- as.character(as.Date(x_obs[,1],format="%m/%d/%Y"))
    }
    
    obs_data <- get_obsdata(x_obs)
    obs_data$date <- strptime(obs_data$date,format="%Y-%m-%d")
    if (nchar(startDt)>1) {obs_data<-obs_data[which(strptime(obs_data$date,"%Y-%m-%d")>=strptime(startdate,"%Y-%m-%d")),]}
    if (nchar(endDt)>1) {obs_data<-obs_data[which(strptime(obs_data$date,"%Y-%m-%d")<=strptime(enddate,"%Y-%m-%d")),]}
    obs_count<-nrow(obs_data)
    cat(paste("get_obsdata run on x_obs for site",site,obs_count,"\n"))
    if (nrow(obs_data)==0) {
      obs_data<-"No complete water years for site"
    } else {
      countbyyr<-aggregate(obs_data$discharge, list(obs_data$wy_val), length)
      colnames(countbyyr)<-c("wy","num_samples")
      sub_countbyyr<-countbyyr[countbyyr$num_samples>=365,]
      if (nrow(sub_countbyyr)==0) {
        obs_data<-"No complete water years for site"
      } else {
        obs_data<-merge(obs_data,sub_countbyyr,by.x="wy_val",by.y="wy")
        obs_data<-obs_data[order(obs_data$date),]
        obs_data <- obs_data[,c('wy_val','date','discharge','month_val','year_val','day_val','jul_val')]
        site_no <- rep(site,nrow(obs_data))
        obs_data <- cbind(site_no,obs_data,stringsAsFactors=FALSE)
        }
      dataOut[[i]]<-obs_data
    }
  }
  return(dataOut)
}