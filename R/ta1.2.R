#' Function to return the TA1 and TA2 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the Colwell constancy and predictability for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ta1.2 list containing the constancy and predictability for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' ta1.2(qfiletempf)
ta1.2 <- function(qfiletempf) {
  colwell_mat <- matrix(-99999,365,11)
  mean_flow <- ma1(qfiletempf)
  for (i in 1:365) {
    m <- ifelse(i<93,i+273,i-92)
    qfile_sub <- qfiletempf[qfiletempf$jul_val==m,]
    colwell_mat[i,1] <- nrow(qfile_sub[log10(qfile_sub$discharge)<(.1*log10(mean_flow)),])
    colwell_mat[i,2] <- nrow(qfile_sub[log10(qfile_sub$discharge)>=(.1*log10(mean_flow)) & log10(qfile_sub$discharge)<(.25*log10(mean_flow)),])
    colwell_mat[i,3] <- nrow(qfile_sub[log10(qfile_sub$discharge)>=(.25*log10(mean_flow)) & log10(qfile_sub$discharge)<(.5*log10(mean_flow)),])
    colwell_mat[i,4] <- nrow(qfile_sub[log10(qfile_sub$discharge)>=(.5*log10(mean_flow)) & log10(qfile_sub$discharge)<(.75*log10(mean_flow)),])
    colwell_mat[i,5] <- nrow(qfile_sub[log10(qfile_sub$discharge)>=(.75*log10(mean_flow)) & log10(qfile_sub$discharge)<(1*log10(mean_flow)),])
    colwell_mat[i,6] <- nrow(qfile_sub[log10(qfile_sub$discharge)>=(1*log10(mean_flow)) & log10(qfile_sub$discharge)<(1.25*log10(mean_flow)),])
    colwell_mat[i,7] <- nrow(qfile_sub[log10(qfile_sub$discharge)>=(1.25*log10(mean_flow)) & log10(qfile_sub$discharge)<(1.5*log10(mean_flow)),])
    colwell_mat[i,8] <- nrow(qfile_sub[log10(qfile_sub$discharge)>=(1.5*log10(mean_flow)) & log10(qfile_sub$discharge)<(1.75*log10(mean_flow)),])
    colwell_mat[i,9] <- nrow(qfile_sub[log10(qfile_sub$discharge)>=(1.75*log10(mean_flow)) & log10(qfile_sub$discharge)<(2*log10(mean_flow)),])
    colwell_mat[i,10] <- nrow(qfile_sub[log10(qfile_sub$discharge)>=(2*log10(mean_flow)) & log10(qfile_sub$discharge)<(2.25*log10(mean_flow)),])
    colwell_mat[i,11] <- nrow(qfile_sub[log10(qfile_sub$discharge)>=(2.25*log10(mean_flow)),])
  }
  XJ <- rowSums(colwell_mat)
  YI <- colSums(colwell_mat)
  Z <- sum(colwell_mat)
  XJ_sub <- XJ[XJ>0]
  HX <- -sum((XJ_sub/Z)*log10(XJ_sub/Z))
  YI_sub <- YI[YI>0]
  HY <- -sum((YI_sub/Z)*log10(YI_sub/Z))
  colwell_sub <- colwell_mat[colwell_mat>0]
  HXY <- -sum((colwell_sub/Z)*log10(colwell_sub/Z))
  HxY <- HXY - HX
  ta1 <- 1-(HY/log10(11))
  ta2 <- 100*(1-(HxY/log10(11)))
  ta1.2<-list(ta1=ta1,ta2=ta2)
  return(ta1.2)
}