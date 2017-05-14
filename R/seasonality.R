#' Function to compute the seasonal factors (amplitude and phase) for a given data series
#' 
#' This function accepts a data frame containing daily streamflow data, then computes seasonality 
#' variables by first standardizing flows, the fitting relation 
#' A*cos(2*pi*t) + B*sin(2*pi*t)1) Get decimal yearand returns the amplitude and phase
#' 
#' @param x A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
#' @param yearType A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
#' @importFrom stats .lm.fit coef
#' @return seasonality vector of seasonal factors (amplitude and phase)
#' @examples 
#' x <- sampleData[c("date","discharge")]
#' seasonality(x=x)
#' @export
seasonality <- function(x,yearType = "water") {
        
        #Check data inputs
        x <- dataCheck(x,yearType)
        
        #calculate some stuff for use later
        x$month_val <- lubridate::month(x$date)
        
        rawdates<-x$date
        dateaschar<-as.character(rawdates)
        jday<-lubridate::yday(x$date)
        decimal_year<-as.numeric(x$year_val)+(jday/365.25)
        #2) Standardize flows
        std_flows<-scale(x$discharge, center = TRUE, scale = TRUE)
        #3) Use linear model to fit 
        x_mat = cbind(1, sin(2*pi*decimal_year), cos(2*pi*decimal_year))
        seasonfit<-.lm.fit(x_mat, std_flows)
        b1<-as.vector(coef(seasonfit)[2])
        b2<-as.vector(coef(seasonfit)[3]) 
        #Now compute the amplitude and phase of the seasonal signal
        amplitude<-round(sqrt((b2^2)+(b1^2)),digits=2)
        #phase<-round(atan((-seasonB)/seasonA),digits=2)
        MaxDay <- function(b1,b2){
                version1 <- 365.25 * ((pi/2)-atan(b2/b1))/(2*pi)
                version2 <- 365.25 * ((pi/2)-pi-atan(b2/b1))/(2*pi)
                MaxDay <- if(b1 > 0) version1 else 365.25 + version2
                MaxDay <- if(b1 == 0 & b2 > 0) 365.25 else MaxDay
                MaxDay <- if(b1 == 0 & b2 < 0) 365.25/2 else MaxDay
                MaxDay <- if(b1 == 0 & b2 == 0) NA else MaxDay
                return(MaxDay)
        }
        phase <- MaxDay(b1,b2)
        seasonalityv <- cbind(amplitude,phase)
        return(seasonalityv)
}
