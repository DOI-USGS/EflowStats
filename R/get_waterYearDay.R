#' Day of water year
#' @description Given a vector of dates, calculates day of water year accounting for leap years.
#' @param x A vector of class date.
#' @param wyMonth A numeric. The month of the year in which the water year starts 
#' (1=January, 12=December). The water year begins on the first day of wyMonth.

#' @return A numeric vector of day of water year
#' @importFrom lubridate leap_year
#' @importFrom lubridate yday
#' @export
#' @examples
#' x <- seq(from=as.Date("2010-01-01"),to=as.Date("2016-01-01"),by="1 days")
#' year_day <- get_waterYearDay(x, 2L)

get_waterYearDay <- function(x, wyMonth=10L) {
        
  doy <- lubridate::yday(x)  # day of year series
  year <- as.integer(lubridate::year(x))  # actual year series - this must be integer for the subtraction
  pyear <- year-1  # previous year series
  wy_date <- as.Date(paste(year,wyMonth,"01", sep = "-"))  # series with the date of the water year in the current year
  pwy_date <- as.Date(paste(pyear,wyMonth,"01", sep = "-"))  # series with the date of the water year in the previous year
  
  # the day of the year after the beginning of the water year can be calculated by subtracting dates:
  # if the current date is before the current water year date:
  #   year_day is the days between current date and the previous water year + 1 day
  # else (current date is after the current water year date):
  #   year_day is the days between current date and the actual water year + 1 day
  year_day <- ifelse(wy_date > x, x-pwy_date+1, x-wy_date+1)
  
  return(year_day)
        
}
