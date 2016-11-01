#' Function to return the base flow index for a given data frame
#' 
#' This function accepts a vector of daily mean discharge values and 
#' calculates the base flow index of the daily flow values for the entire record
#' 
#' @param x A numeric vector of consecutive daily mean discharge values
#' @return bfi numeric value of the base flow index for the given data frame
#' @export
#' @importFrom RcppRoll roll_mean
#' @examples
#' x<-sampleData$discharge
#' bfi(x)
bfi <- function(x) {

    day7mean <- RcppRoll::roll_mean(x, 6, align = "right", partial = TRUE)
    min7daybyyear <- min(day7mean)
    meanflow <- mean(x)
    bfi <- min7daybyyear/meanflow 
  return(bfi)
}