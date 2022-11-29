#' Cuts the discharge time series to full water years
#' @param x data.frame containing a vector of date values in the first 
#' column and vector of numeric flow values in the second column.
#' @param wyMonth integer month of the year in which the water year starts. 
#' The water year begins on the first day of wyMonth.
#' @return data.frame in original structure, but cut to full water years
#' @details To ensure the input data is valid for use with other 
#' EflowStats functions. 
#' #' \enumerate{
#'   \item First column must be of class `Date`.
#'   \item Second must be of class `numeric`.
#'   \item `wyMonth`input must be of class `integer`. 
#' }
#' @examples
#' x <- sampleData[, c('date', 'discharge')]
#' cut_dataToWaterYear(x,10L)
#' @export
#' 
cut_dataToWaterYear <- function(x,wyMonth=10L) {
  ###rename dataframe for convenient use inside function
  old_names <- colnames(x)
  names(x) <- c("date","discharge")
  
  ###Order by date
  x = dplyr::arrange(x, date)
  x$year_val <- get_waterYear(x[,1], wyMonth)
  
  # get first and last water year integer of time series
  first_year <- x$year_val[1]
  last_year <- x$year_val[nrow(x)]
  
  # get the current days of the first and last water year of time series
  ndays_first_year <- nrow(x[x$year_val == first_year,])
  ndays_last_year <- nrow(x[x$year_val == last_year,])
  
  # get the target number of days (depends if water year is in a leap year or not)
  ndays_first_year_target <- ifelse(lubridate::leap_year(as.Date(paste(first_year,"01","01", sep = "-"))), 366, 365)
  ndays_last_year_target <- ifelse(lubridate::leap_year(as.Date(paste(last_year,"01","01", sep = "-"))), 366, 365)
  
  # remove the first and last year if number of days is less than target number
  if(ndays_first_year < ndays_first_year_target){
    x <- x[x$year_val != first_year,]  # remove the first year if it does not have complete data
  }
  if(ndays_last_year < ndays_last_year_target){
    x <- x[x$year_val != last_year,]  # remove the last year if it does not have complete data
  }
  # clean up the column names
  x$year_val <- NULL
  names(x) <- old_names
  return(x)
}
