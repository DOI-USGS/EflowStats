#' Day of water year
#' @description Given a vector of dates, calculates day of water year accounting for leap years.
#' @param x A vector of class date.
#' @return A numeric vector of day of water year
#' @importFrom lubridate year
#' @importFrom lubridate yday
#' @export
#' @examples
#' x <- seq(from=as.Date("2010-01-01"),to=as.Date("2016-01-01"),by="1 days")
#' waterYearDay(x)


waterYearDay <- function(x) {
        
        x <- data.frame(date = x,
                        jday = lubridate::yday(x))
        
        wyday <- ifelse(is.leapyear(lubridate::year(x$date)),
                          ifelse(x$jday>=275,
                                 x$jday-274,
                                 x$jday+92),
                          ifelse(x$jday>=274,
                                 x$jday-273,
                                 x$jday+92)
        )
        
        return(wyday)

}