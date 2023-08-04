#' Function to return the water year for a given date
#' 
#' @param x A date vector
#' @param numeric Logical. Return values are numeric vector or vector of ordered factors
#' @param wyMonth A numeric. The month of the year in which the water year starts 
#' (1=January, 12=December). The water year begins on the first day of wyMonth.
#' @return A vector of numeric water years for each date if \code{numeric = TRUE} otherwise an ordered factor of water years if \code{numeric = FALSE}
#' @export
#' @examples
#' get_waterYear(sampleData$date)
#' 
get_waterYear <- function(x, wyMonth=10L, numeric=TRUE) {
        ## Coding history:
        ##    2005Jul14 DLLorenz Initial dated verion
        ##    2010Feb17 DLLorenz Added option to return numerics
        ##    2011Jun07 DLLorenz Conversion to R
        ##    2012Aug11 DLLorenz Integer fixes
        ##    2013Feb15 DLLorenz Prep for gitHub
        ##    2016Aug12 Joe Mills copied this from smwrBase
        ##    2023Aug04 Jens Kiesel added different calculation if wyMonth < 3
        ##              (see explanation below)
        x <- as.POSIXlt(x)
        yr <- x$year + 1900L
        mn <- x$mon + 1L  # x$mon starts at 0 for Jan!
        if (wyMonth < 3){
          # Jan must be associated to the previous year if wyMonth < 3 
          # because of leap years. If an assigned water year includes the 
          # 29th of Feb, it must be associated to a leap year:
          # 
          # If the water year starts in January (equal to the calendar year):
          # yr for all months must be equal to the current yr.
          # function "yr<-yr-ifelse(mn<wyMonth,1L,0L)" is then:
          # yr <- yr - 0 (ifelse condition is false in all cases)
          # 
          # If the water year starts in February:
          # yr for Jan must be the previous year and yr for all other
          # months must be the current year
          # function "yr<-yr-ifelse(mn<wyMonth,1L,0L)" for mn=1 < wyMonth=2:
          # yr <- yr - 1 (ifelse condition is true)
          # function "yr<-yr-ifelse(mn<wyMonth,1L,0L)" for mn>=2 == wyMonth=2:
          # yr <- yr - 0 (ifelse condition is false)
          # The first of February of a time series initiates a new
          # water year and if the time series contains a leap year,
          # the year for February must be kept as the current year
          # (with the original function yr<-yr+ifelse(mn<wyMonth,0L,1L), 
          #  February 2000 would be assigned water year 2001)
          yr <- yr - ifelse(mn < wyMonth, 1L, 0L)
        }else{
          # If the water year starts in March (or later):
          # yr for Jan and Feb must be the current year (as it can be a
          # leap year)
          # function "yr<-yr+ifelse(mn<wyMonth,0L,1L)":
          # yr <- yr + 0
          yr <- yr + ifelse(mn < wyMonth, 0L, 1L) 
        }
        if(numeric)
                return(yr)
        ordered(yr)
}
