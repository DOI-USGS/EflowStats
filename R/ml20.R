#' Function to return the ML20 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates base flow index ML20. Divide the daily flow record into 5-day blocks. Find the minimum flow for 
#' each block. Assign the minimum flow as a base flow for that block if 90 percent of that minimum flow is less 
#' than the minimum flows for the blocks on either side. Otherwise, set it to zero. Fill in the zero values 
#' using linear interpolation. Compute the total flow for the entire record and the total base flow for the 
#' entire record. ML20 is the ratio of total base flow to total flow (dimensionless-spatial).
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return ml20 numeric value of ML20 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ml20(qfiletempf)
ml20 <- function(x) {
  sub_flow <- subset(x,na.rm=TRUE)
  numdays <- nrow(sub_flow)
  numsets <- floor(numdays/5)
  sets <- c(1:numsets)
  fiveday <- rep(999999999,numsets)
  for (i in 1:numsets) {
    for (j in (5*i-4):(5*i)) {
      fiveday[i] <- ifelse(fiveday[i]<sub_flow$discharge[j],fiveday[i],sub_flow$discharge[j])
    }
  }
  fiveday_lead <- c(9999999999,fiveday)
  fiveday_lag <- c(fiveday,999999999999)
  fiveday_lead <- fiveday_lead[1:numsets]
  fiveday_lag <- fiveday_lag[2:(numsets+1)]
  zeros <- rep(0,numsets)
  seq_nums <- seq(1,numsets)
  fiveday <- ifelse((.9*fiveday)<fiveday_lag & (.9*fiveday)<fiveday_lead,fiveday,zeros)
  fiveday_seq <- as.data.frame(cbind(seq_nums,fiveday))
  fiveday_seq$fiveday <- ifelse(fiveday_seq$fiveday==0,'',fiveday_seq$fiveday)
  fiveday_interp <- approx(fiveday_seq$seq_nums[fiveday_seq$fiveday!=''],fiveday_seq$fiveday[fiveday_seq$fiveday!=''],xout=fiveday_seq$seq_nums[fiveday_seq$fiveday==''],method="linear",rule=2)
  fiveday_merge <- merge(fiveday_seq,fiveday_interp,by.x="seq_nums",by.y="x",all=TRUE)
  fiveday_merge$y <- ifelse(is.na(fiveday_merge$y),as.numeric(fiveday_merge$fiveday),fiveday_merge$y)
  total_bf <- sum(fiveday_merge$y*5)+((numdays-(numsets*5))*fiveday_merge$y[numsets])
  total_flow <- sum(sub_flow$discharge)
  ml20 <- total_bf/total_flow
  return(ml20)
} 