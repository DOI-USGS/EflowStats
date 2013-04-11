#' Function to return the DH22 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the mean of annual median days between flood events for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dh22 numeric containing the mean of annual median flood intervals for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data, stringsAsFactors=FALSE)
#' dh22(qfiletempf)
dh22 <- function(qfiletempf) {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  frank <- floor(findrank(length(sortq), 0.40))
  lfcrit <- sortq[frank]
  subset_crit <- subset(qfiletempf, qfiletempf$discharge>lfcrit)
  subset_crit$date <- as.Date(subset_crit$date, "%m/%d/%Y") 
  yrs_dur <- data.frame(year_val = rep(0,nrow(subset_crit)), cnt = rep(9,nrow(subset_crit)), set = rep(0,nrow(subset_crit)), date = rep(Sys.Date(),nrow(subset_crit)))
  yrs_dur$cnt[1] <- 0
  yrs_dur$year_val[1] <- subset_crit$year_val[1]
  yrs_dur$set[1] <- 0
  i <- 1
  counter <- 0
  set_cnt <- 1
  sub_length <- nrow(subset_crit)-1
  for (i in 1:sub_length) {
    yrs_dur$year_val[i+1] <- subset_crit$year_val[i+1]
    yrs_dur$date[i+1] <- subset_crit$date[i+1]
    if (subset_crit$jul_val[i+1] - subset_crit$jul_val[i] == 1) { 
      yrs_dur$cnt[i+1]<-1+counter
      counter <- counter+1
    }
    else {
      yrs_dur$cnt[i+1]<-0
      counter <- 0
      yrs_dur$set[i+1]<-set_cnt
      set_cnt <- set_cnt+1
    }
  }
  sets_cnt <- max(yrs_dur$set)-1
  days_btwn <- data.frame(year_val = rep(0,sets_cnt), num_days = rep(0,sets_cnt), set_num = rep(0,sets_cnt))
  for (j in 1:sets_cnt) {
    num <- match(j+1,yrs_dur$set) 
    num2 <- match(j,yrs_dur$set)
    days_btwn$year_val[j] <- yrs_dur$year_val[num-1]
    days_btwn$num_days[j] <- yrs_dur$date[num-1]-yrs_dur$date[num2-1]
    days_btwn$set_num[j] <- yrs_dur$set[num]
  }
  medbyyr <- aggregate(days_btwn$num_days, list(days_btwn$year_val), median, na.rm=TRUE)
  dh22 <- mean(medbyyr$x)
  return(dh22)
}