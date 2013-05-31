#' Function to return the TA1 and TA2 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' TA1; Constancy. Constancy is computed via the formulation of Colwell (see example in Colwell, 1974). A matrix of values 
#' is compiled where the rows are 365 (no February 29th) days of the year and the columns are 11 flow categories. The 
#' cell values are the number of times that a flow falls into a category on each day. The categories are: 
#' log(flow) < 0.1 × log(mean flow), 
#' 0.1 × log(mean flow) ??? log(flow) < 0.25 × log(mean flow)
#' 0.25 × log(mean flow) ??? log(flow) < 0.5 × log(mean flow)
#' 0.5 × log(mean flow) ??? log(flow) < 0.75 × log(mean flow)
#' 0.75 × log(mean flow) ??? log(flow) < 1.0 × log(mean flow)
#' 1.0 × log(mean flow) ??? log(flow) < 1.25 × log(mean flow)
#' 1.25 × log(mean flow) ???log(flow) < 1.5 × log(mean flow)
#' 1.5 × log(mean flow) ??? log(flow) < 1.75 × log(mean flow)
#' 1.75 × log(mean flow) ??? log(flow) < 2.0 × log(mean flow)
#' 2.0 ×log(mean flow) ??? log(flow) < 2.25 × log(mean flow)
#' log(flow) ??? 2.25 × log(mean flow)
#' The row totals, column totals, and grand total are computed. Using the equations for Shannon information theory 
#' parameters, constancy is computed as:
#' 1- (uncertainty with respect to state)/log (number of state) 
#' TA2; Predictability. Predictability is computed from the same matrix as constancy (see example in Colwell, 1974). It 
#' is computed as: 
#' 1- (uncertainty with respect to interaction of time and state - uncertainty with respect to time)/log (number of state) 
#' where uncertainty with respect to state = sum((YI_sub/Z)*log10(YI_sub/Z))
#' where YI_sub = the non-zero sums of the 11 categories and Z = the sum total of the Colwell matrix 
#' and where uncertainty with respect to time = sum((XJ_sub/Z)*log10(XJ_sub/Z)) 
#' where XJ_sub = the non-zero sums of the 365 days
#' and where uncertainty with respect to interaction of time and state = sum((colwell_sub/z)*log10(colwell_sub/Z))
#' where colwell_sub = the non-zero sums of the entire matrix 
#' and where number of state = number of categories = 11
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ta1.2 list containing TA1 and TA2 for the given data frame
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