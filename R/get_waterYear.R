#' Function to return the water year for a given date
#' 
#' @param x A date vector
#' @param numeric Logical. Return values are numeric vector or vector of ordered factors
#' @param wyMonth A value of class integer
#' @return A vector of numeric water years for each date if \code{numeric = TRUE} otherwise an ordered factor of water years if \code{numeric = FALSE}
#' @export
#' @examples
#' get_waterYear(sampleData$date)
x <- seq(from=as.Date("2010-01-01"),to=as.Date("2016-01-01"),by="1 days")

get_waterYear <- function(x, wyMonth=10L, numeric=TRUE) {
        ## Coding history:
        ##    2005Jul14 DLLorenz Initial dated verion
        ##    2010Feb17 DLLorenz Added option to return numerics
        ##    2011Jun07 DLLorenz Conversion to R
        ##    2012Aug11 DLLorenz Integer fixes
        ##    2013Feb15 DLLorenz Prep for gitHub
        ##    2016Aug12 Joe Mills copied this from smwrBase
        x <- as.POSIXlt(x)
        yr <- x$year + 1900L
        mn <- x$mon + 1L
        ## adjust for water year
        yr <- yr + ifelse(mn < wyMonth, 0L, 1L)
        if(numeric)
                return(yr)
        ordered(yr)
}
