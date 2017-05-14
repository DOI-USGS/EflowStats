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
        
        jday = lubridate::yday(x)
        yrs_leap = lubridate::leap_year(x)

        gt_lp275 = jday >= (274 + yrs_leap)

        jday[gt_lp275] = jday[gt_lp275] - (273 + yrs_leap[gt_lp275])
        jday[!gt_lp275] = jday[!gt_lp275] + 92

        return(jday)
        
}
