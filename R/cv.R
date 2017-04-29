#' Function to return the coefficient of variation for a given data series
#' 
#' @details This functions accepts a numeric vector of flow values and calculates the coefficient of variation, defined as the mean/standard deviation
#' @param x numeric vector of flow values
#' @return numeric coefficient of variation for the given data frame
#' @export
#' @examples
#' cv(sampleData$discharge)
cv <- function(x) {
  x1 <- mean(x,na.rm=TRUE)
  x2 <- sd(x,na.rm=TRUE)
  cv <- x2/x1
  return(cv)
}
