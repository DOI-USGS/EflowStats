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
  ##    2023Aug04 Jens Kiesel added different calculation if wyMonth < 7
  ##              (see explanation below)
  x <- as.POSIXlt(x)
  yr <- x$year + 1900L
  mn <- x$mon + 1L  # x$mon starts at 0 for Jan!
  if (wyMonth < 7){
    # it is most intuitive that the water year value should be the calendar 
    # year with the most dates in the water year. Jan.-Jun. would be assigned
    # the previous calendar year as the water year value and Jul.-Dec. would be assigned

    # the current calendar year as the water year value.

    
    # If the water year is supposed to start in Jan-Jun (e.g. April):
    # yr for Jan-Mar must be the previous calendar year, 

    # yr for Apr-Dec must be the current calendar year 

    yr <- yr - ifelse(mn < wyMonth, 1L, 0L)
  }else{
    # If the water year is supposed to start in Jul-Dec (e.g. September):
    # yr for Jan-August must be the current calendar year, 

    # yr for Sep-Dec must be the next calendar year 

    yr <- yr + ifelse(mn < wyMonth, 0L, 1L) 
  }
  if(numeric)
    return(yr)
  ordered(yr)
}
