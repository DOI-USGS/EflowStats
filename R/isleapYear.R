#courtesy of R bloggers for is.leapyear
is.leapyear <- function(year, yearType='calendar', wyMonth=10L){
  if (yearType=='calendar' || wyMonth<3 || wyMonth>6) {
    #http://en.wikipedia.org/wiki/Leap_year
    return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0)) 
  }else if ((yearType=='water') && (wyMonth>=3 && wyMonth <=6)) {
    #
    # add 1 year if water year is supposed to start in Mar, Apr, May, Jun.
    # For these cases, the water year is assigned the previous
    # year value but contains the leap day. 
    #
    # Example: 
    # Time series: 01Apr1999 - 31Mar2000
    # wyMonth = 4
    # Assigned water year: 1999
    # Number of days: 366 (because it contains 29Feb2000)
    # -> Function must return true for 1999 (and any other year that 
    # precedes a leap year)
    # 
    return((((year+1) %% 4 == 0) & ((year+1) %% 100 != 0)) | ((year+1) %% 400 == 0)) 
  }
}