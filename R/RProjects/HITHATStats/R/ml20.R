#' Function to return the ML20 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates ML20. Divide the daily flow record into 5-day blocks. Find the minimum flow for each block. 
#' Assign the minimum flow as a base flow for that block if 90% of that minimum flow is less than the 
#' minimum flows for the blocks on eitherside. Otherwise, set it to zero. Fill in the zero values using 
#' linear interpolation. Compute the total flow for the entire record and the total base flow for the 
#' entire record. ML20 is the ratio of total flow to total base flow.
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return ml20 numeric value of the ratio of total flow to total base flow for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' x<-read.csv(load_data)
#' ml20(x)
ml20 <- function(x) {
  sub_flow <- subset(x,x$discharge>0,na.rm=TRUE)
  numdays <- nrow(sub_flow)
  numsets <- ceiling(numdays/5)
  sets <- c(1:numsets)
  sets_merge <- as.data.frame(sort(rep.int(sets,5))[1:nrow(sub_flow)],stringsAsFactors=FALSE)
  merge_data <- cbind(sub_flow,sets_merge)
  colnames(merge_data) <- c("wy_val","date","discharge","month_val","year_val","day_val","jul_val","num_samples","seq_num")
  min5day <- aggregate(merge_data$discharge,list(merge_data$seq_num),FUN=min,na.rm=TRUE)
  merge_data <- merge(min5day,merge_data,by.x="Group.1",by.y="seq_num")
  colnames(merge_data) <- c("seq_num","base_flow","wy_val","date","discharge","month_val","year_val","day_val","jul_val","num_samples")
  base_flows <- unique(merge_data[, c("seq_num","base_flow")])
  base_flows$base_flow <- 0.9*base_flows$base_flow
  base_flows_lag <- unique(merge_data[, c("seq_num","base_flow")])
  base_flows_lag$seq_num <- base_flows_lag$seq_num-1
  base_flows_lead <- unique(merge_data[, c("seq_num","base_flow")])
  base_flows_lead$seq_num <- base_flows_lead$seq_num+1
  merge_base_flows_lag <- merge(base_flows,base_flows_lag,by="seq_num")
  merge_base_flows_lead <- merge(base_flows,base_flows_lead,by="seq_num")
  merge_base_flows <- merge(merge_base_flows_lag,merge_base_flows_lead,all=TRUE,by="seq_num")
  colnames(merge_base_flows) <- c("seq_num","base_flow","base_flow_lag","base_flow1","base_flow_lead")
  merge_base_flows["final_base_flow"] <- NA
  merge_base_flows$final_base_flow[1:numsets-1] <- ifelse(merge_base_flows$base_flow[1:numsets-1]<merge_base_flows$base_flow_lag[1:numsets-1],merge_base_flows$base_flow[1:numsets-1],0)
  merge_base_flows$final_base_flow[2:numsets] <- ifelse(merge_base_flows$base_flow1[2:numsets]<merge_base_flows$base_flow_lead[2:numsets],merge_base_flows$base_flow1[2:numsets],0)
  base_flows <- merge_base_flows$final_base_flow[merge_base_flows$final_base_flow!=0]
  base_flows_interp <- approx(merge_base_flows$seq_num[merge_base_flows$final_base_flow!=0],base_flows,xout=merge_base_flows$seq_num[merge_base_flows$final_base_flow==0],method="linear",rule=2)
  merge_base_flows_interp <- merge(merge_base_flows,base_flows_interp,by.x="seq_num",by.y="x",all=TRUE)
  merge_base_flows_interp[is.na(merge_base_flows_interp)] <- 0
  merge_base_flows_interp["merge_bf"]<-0
  merge_base_flows_interp$merge_bf <- ifelse(merge_base_flows_interp$final_base_flow>merge_base_flows_interp$y,merge_base_flows_interp$final_base_flow,merge_base_flows_interp$y)
  bfi_sub <- merge_base_flows_interp[,c("seq_num","merge_bf")]
  merge_data_final <- merge(merge_data,bfi_sub,by="seq_num")
  total_bf <- sum(merge_data_final$merge_bf)
  total_flow <- sum(merge_data_final$discharge)
  ml20 <- total_flow/total_bf
  return(ml20)
} 