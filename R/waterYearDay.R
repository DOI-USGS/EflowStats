#' Day of water year
#' @description Given a vector of dates, calculates day of water year accounting for leap years.
#' @param x A vector of class date.
#' @return A numeric vector of day of water year
#' @importFrom lubridate leap_year
#' @importFrom lubridate yday
#' @export
#' @examples
#' x <- seq(from=as.Date("2010-01-01"),to=as.Date("2016-01-01"),by="1 days")
#' waterYearDay(x)


waterYearDay <- function(x) {
        
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
