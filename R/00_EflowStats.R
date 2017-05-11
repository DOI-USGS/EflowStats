#' Sample Streamflow data.frame
#'
#' A dataset with a few years of complete water year based data.
#' 
#' @name sampleData
#' @format A data frame with 731 rows and 7 variables:
#' \describe{
#'   \item{wy_val}{integer of the water year}
#'   \item{date}{Date of the complete day}
#'   \item{discharge}{Discharge values}
#'   \item{month_val}{integer month value}
#'   \item{year_val}{integer year value}
#'   \item{day_val}{integer day value}
#'   \item{jul_val}{day of year}
#' }
NULL

# dplyr NSE variables to kill notes in check.
month_val <- discharge <- leapYear <- day <- 
        year_val <- roll3Mean <- roll7Mean <- 
        roll30Mean <- roll90Mean <- events <- 
        . <- highFlow <- CV <- block <- 
        peakQ <- flow <- maxFlow <-  
        minFlow <- event <- ".dplyr.var"
