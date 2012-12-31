# This code is a wrapper to compute all statisitics for a group of flow
# files. Last updated on 11/16/10.
# Original code from SA Archfield
#
# modified by JKiang 2/21/2012
# - changed outputs slightly - added station ID, changed to tab delimited, no quotes
# - slight changes to setting of data directory.  now don't switch working directory.
# - will think about changing compstatsall() so that it returns a data.frame, not matrix

#datadir<-"C:/Data/daily flow estimation/statscalcs/sarchfield"
datadir<-"."
gagelist<-paste(datadir,"/","TestList.txt",sep="")
rgage<-read.table(gagelist,as.is=TRUE,header=TRUE,colClasses="character")

source("compstats.jek.r")

N<-length(rgage$GAGE_ID)
rgageall<-rgage$GAGE_ID

m<-37 #number of stats to be computed

#Create dataframe to store results
allstats<-mat.or.vec(N,m) #gages are in rows, stats in columns
startdate <- mat.or.vec(N,1)
enddate <- mat.or.vec(N,1)

for(i in 1:N) {
  print(paste("Starting ",i," of ",N," Station = ",rgageall[i],sep=""))
  #Specify filename
  inputfile<-paste(rgageall[i],'_f.txt',sep="")
  #Read in datafile
  qfiletemp<-read.table(paste(datadir,"/",inputfile,sep=""),as.is=TRUE,header=TRUE,colClasses="character")
  #Assign format to data
  dateaschar<-as.character(qfiletemp$dv_dt)
  qfiletemp$dv_dt<-chron(dateaschar,format=c(dates="y-m-d"),origin=c(month=1,day=1,year=1990),out.format=c(dates="year-m-d"))
  mkqnum<-as.numeric(qfiletemp$dv_va)
  qfiletemp$dv_va<-mkqnum
  #Parse data and format for imput to streamflow stats functions
  qfiletempf<-flow.data1630(qfiletemp)
  #Put parsed and formatted data into qfile dataframe
  qfile<-NULL
  qfile<-qfiletempf
  #Compute flow stats and write results to a dataframe
  resallstats<-compallstats(qfile,'mean') #Type of stat should either be 'mean' or 'median'
                                          #and applies only to some of the calculated stats
  allstats[i,]<-resallstats
  
  #get start and end dates of data for each station
  startdate[i] <- as.character(min(qfiletemp$dv_dt))
  enddate[i] <- as.character(max(qfiletemp$dv_dt))
}

#create matrix with stationID, startdate, enddate
  gagedates = data.frame(stationID=rgageall,startdate=startdate,enddate=enddate)
  write.table(gagedates,file="gagedates.out", col.names=TRUE, row.names=FALSE, quote=FALSE,sep="\t")

# Make allstats matrix a dataframe with gages added
statsout<-data.frame(allstats)

# Assign column names for the statsout dataframe
colnames(statsout)<-c('ma5','ma16','ma18','ma21','ma22','ma26','ma29','ma34',
  'ma37','ma39','ml13','ml14','ml17','ml18','mh14','mh16','mh26','fh2','fh3',
  'fh4','dh5','dh10','fl1','fl2','th1','th2','tl1','tl2','ra1','ra4','dl1',
  'dl2','dl5','dl6','dl9','dl10','dl18')
statsoutJEK <- data.frame(stationID=rgageall,statsout) #add stationID
write.table(statsoutJEK,file="allstats.out",col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")

#Standardize variables
statsstnd<-statsout[,2:m]
statsstnd2<-data.matrix(statsstnd)
statsoutstnd<-scale(statsstnd2, center = TRUE, scale = TRUE)
statsoutstndJEK <- data.frame(stationID=rgageall,statsoutstnd) # add stationID
write.table(statsoutstndJEK,file="allstatsstnd.out",col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")

#Convert back to dataframe and add gages
#statsoutstndf<-data.frame(rgageall,statsoutstnd)
#colnames(statsoutstndf)<-c('gagenames','ma5','ma16','ma18','ma21','ma22','ma26','ma29','ma34',
#  'ma37','ma39','ml13','ml14','ml17','ml18','mh14','mh16','mh26','fh2','fh3',
#  'fh4','dh5','dh10','fl1','fl2','th1','th2','tl1','tl2','ra1','ra4','dl1',
#  'dl2','dl5','dl6','dl9','dl10','dl18')






                    
