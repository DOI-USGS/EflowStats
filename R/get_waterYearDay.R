#' Day of water year
#' @description Given a vector of dates, calculates day of water year accounting for leap years.
#' @param x A vector of class date.
#' @param wyMonth A value of class integer
#' @return A numeric vector of day of water year
#' @importFrom lubridate leap_year
#' @importFrom lubridate yday
#' @export
#' @examples
#x <- seq(from=as.Date("2010-01-01"),to=as.Date("2016-01-01"),by="1 days")
#year_day <- get_waterYearDay(x, 2L)
#write.csv(year_day,"year_day_2.csv",row.names=FALSE)

#year_day_old <- get_waterYearDay_old(x)
#write.csv(year_day_old,"year_day_old.csv",row.names=FALSE)
#write.csv(x,"dates.csv",row.names=FALSE)


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

get_waterYearDay_old <- function(x) {
  
  year_day <- lubridate::yday(x)
  yrs_leap <- lubridate::leap_year(x)
  
  # October 1st (day 1 of water year) is day 274 (or 275 in leap year) of calendar year.
  # (274 + years_leap) == 275 for dates in leap year and 274 for dates not in leap year.
  # This provides a boolean selector for days before (false) / after (true) Oct 1.
  after_waterday <- year_day >= (274 + yrs_leap)
  
  # 273 (or 274 in leap year) days from January 1 to October 1
  # This gives us 1 for October 1st and corrects everything till December 31st.
  year_day[after_waterday] <- year_day[after_waterday] - (273 + yrs_leap[after_waterday])
  
  # 92 days from October 1 to January 1
  # This gives us 93 for January 1 and corrects everything till September 29th.
  year_day[!after_waterday] <- year_day[!after_waterday] + 92
  
  return(year_day)
  
}
