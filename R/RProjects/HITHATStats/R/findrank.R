#' Function to return the rank of a given value
#' 
#' This function accepts the number of values in a data set and the desired percentile and then calculates the rank 
#' 
#' @param n number of values in a data set
#' @param p percentile desired
#' @return findrank numeric giving the position in the data set of the percentile
#' @export
#' @examples
#' n<-365
#' p<-0.05
#' findrank(n, p)
findrank <- function(n, p) {
  r <- (1 - p) * (n + 1)
  findrank <- floor(r)
  return(findrank)
}