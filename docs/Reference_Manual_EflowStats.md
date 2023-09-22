# Vignettes
- [Introduction](intro.md)
- [Discrepencies](packageDiscrepencies.md)

# Function Reference

- [calc_allHIT](#calc_allHIT)
- [calc_ar1](#calc_ar1)
- [calc_bfi](#calc_bfi)
- [calc_durationHigh](#calc_durationHigh)
- [calc_durationLow](#calc_durationLow)
- [calc_frequencyHigh](#calc_frequencyHigh)
- [calc_frequencyLow](#calc_frequencyLow)
- [calc_magAverage](#calc_magAverage)
- [calc_magHigh](#calc_magHigh)
- [calc_magLow](#calc_magLow)
- [calc_magnifSeven](#calc_magnifSeven)
- [calc_rateChange](#calc_rateChange)
- [calc_timingAverage](#calc_timingAverage)
- [calc_timingHigh](#calc_timingHigh)
- [calc_timingLow](#calc_timingLow)
- [cut_dataToWaterYear](#cut_dataToWaterYear)
- [find_changeEvents](#find_changeEvents)
- [find_eventDuration](#find_eventDuration)
- [find_events](#find_events)
- [get_peakThreshold](#get_peakThreshold)
- [get_seasonality](#get_seasonality)
- [get_waterYear](#get_waterYear)
- [get_waterYearDay](#get_waterYearDay)
- [sampleData](#sampleData)
- [validate_data](#validate_data)


September 22, 2023

# DESCRIPTION

```
Package: EflowStats
Type: Package
Title: Hydrologic Indicator and Alteration Stats
Version: 5.2.0
Date: 2017-08-04
Authors@R: c( person("Joe", "Mills", role = c("aut"),
    email = "jmills@ucar.edu"),
              person("David", "Blodgett", role = c("aut","cre"),
    email = "dblodgett@usgs.gov"),
              person("Jared", "Smith", role = c("aut"),
    email = "jsmith@usgs.gov"),
              person("Jens", "Kiesel", role = c("ctb"),
    email = "jkiesel@hydrology.uni-kiel.de")
    )
Depends:
    R (>= 3.3)
Imports:
    lmom,
    dplyr,
    lubridate,
    RcppRoll,
    imputeTS
Suggests:
    xtable,
    knitr,
    testthat,
    rmarkdown,
    DT,
    dataRetrieval,
    readr
Description: A reimplementation of the Hydrologic Index Tool 
        (HIT; Henriksen et al, 2006) for calculating 171 hydrologic indices 
        for stream classification analysis. EflowStats also calculates 7 
        additional statistics used for streamflow classification reffered 
        to as the "Magnificent Seven" (MAG7, Archfield et al., 2013). 
        Unlike the original HIT, EflowStats has been redesigned to injest 
        general hydrologic timeseries data and is not restricted to data 
        formats used by the USGS National Water Information System.
License: CC0
LazyLoad: yes
LazyData: yes
VignetteBuilder: knitr
BuildVignettes: true
Copyright: This software is in the public domain because it contains materials
    that originally came from the United States Geological Survey, an agency of
    the United States Department of Interior. For more information, see the
    official USGS copyright policy at
    http://www.usgs.gov/visual-id/credit_usgs.html#copyright
RoxygenNote: 7.2.3
Encoding: UTF-8
```


# `calc_allHIT`

Function to calculate all 171 biologically relevant hydrologic indice statistics described in the USGS Hydrologic Index Tool


## Description

Function to calculate all 171 biologically relevant hydrologic indice statistics described in the USGS Hydrologic Index Tool


## Usage

```r
calc_allHIT(
  x,
  yearType = "water",
  wyMonth = 10L,
  stats = "all",
  digits = 3,
  pref = "mean",
  drainArea = NULL,
  floodThreshold = NULL
)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
`yearType`     |     A character of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
`wyMonth`     |     A numeric. The month of the year in which the water year starts (1=January, 12=December). The water year begins on the first day of wyMonth.
`stats`     |     Character vector statistic groups. Choices are "all","calc_magAverage", "calc_magLow", "calc_magHigh", "calc_frequencyLow", "calc_frequencyHigh", "calc_durationLow", "calc_durationHigh", "calc_timingAverage", "calc_timingLow", "calc_timingHigh", "calc_rateChange"
`digits`     |     A numeric. Number of digits to round indice values
`pref`     |     A character of either "mean" or "median", indicating whether to use mean or median. See details.
`drainArea`     |     Numeric drainage area. Only required for some statistics. Typically units of square miles, see details of specific statistics for more.
`floodThreshold`     |     Numeric flood threshold as the flow equivalent for a flood recurrence of 1.67 years


## Details

This function is a wrapper for the lower level functions to calculate groups of indice statistics.
 Desired groups of indices can be specified using the `stats` argument.
 Different arguments are required depending on indices selected and are described below.
 Detailed descriptions of the indices and `pref` argument behavior are provided in the documentation for individual indice group functions
  

*  calc_magAverage Indices describing magnitude of the average flow condition. `drainArea` argument must be specified for the ma41 statistic. 

*  calc_magLow Indices describing magnitude of the low flow condition. `drainArea` argument must be specified for the ml22 statistic. 

*  calc_magHigh Indices describing magnitude of the peak flow condition. `drainAre` argument must be specified for the mh20 statistic. 

*  calc_frequencyLow Indices describing frequency of low flow events. No additional arguments required. 

*  calc_frequencyHigh Indices describing frequency of high flow events. No additional arguments required. 

*  calc_durationLow Indices describing duration of low flow events. No additional arguments required. 

*  calc_durationHigh Indices describing duration of high flow events. floodThreshold needs to be supplied. See [`calc_durationHigh`](#calcdurationhigh) . 

*  calc_timingAverage Indices describing timing of average flow events. No additional arguments required. 

*  calc_timingLow Indices describing timing of low flow events. No additional arguments required. 

*  calc_timingHigh Indices describing timing of high flow events. No additional arguments required. 

*  calc_rateChange Indices describing rate of change of flow. No additional arguments required.


## Examples

```r
x <- sampleData[c("date","discharge")]
calc_allHIT(x=x,yearType="water",stats="all",pref="mean",drainArea=50)
```


# `calc_ar1`

calc_ar1 correlation coefficient


## Description

Function to compute the AR(1) correlation coefficient for a given timeseries of discharge
 
 This function accepts a data frame containing daily streamflow data and returns the AR(1)
 correlation coefficient


## Usage

```r
calc_ar1(x, yearType = "water", wyMonth = 10L, digits = 3)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
`yearType`     |     A character of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
`wyMonth`     |     A numeric. The month of the year in which the water year starts (1=January, 12=December). The water year begins on the first day of wyMonth.
`digits`     |     A numeric. Number of digits to round indice values


## Value

calc_ar1 AR(1) correlation coefficient


## Examples

```r
x <- sampleData[c("date","discharge")]
calc_ar1(x)
```


# `calc_bfi`

Function to return the base flow index for a given data frame


## Description

This function accepts a vector of daily mean discharge values and
 calculates the base flow index of the daily flow values for the entire record


## Usage

```r
calc_bfi(x)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     A numeric vector of consecutive daily mean discharge values


## Details

The mean for a 7 day right-aligned moving window is calculated for the supplied flow vector. The baseflow index is calculated as the minimum 7day average flow divided by the mean flow


## Value

calc_bfi numeric value of the base flow index for the given data frame


## Examples

```r
x<-sampleData$discharge
calc_bfi(x)
```


# `calc_durationHigh`

Indices describing duration of high flow events.


## Description

Calculates 24 indices used to describe the duration of high flow conditions.
 See Table X in the EflowStats package vignette for a full description of indices.


## Usage

```r
calc_durationHigh(
  x,
  yearType = "water",
  wyMonth = 10L,
  digits = 3,
  pref = "mean",
  floodThreshold = NULL,
  ...
)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
`yearType`     |     A character of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
`wyMonth`     |     A numeric. The month of the year in which the water year starts (1=January, 12=December). The water year begins on the first day of wyMonth.
`digits`     |     A numeric. Number of digits to round indice values
`pref`     |     A character of either "mean" or "median", indicating whether to use mean or median. See details.
`floodThreshold`     |     Numeric value of flood threshold as the flow equivalent for a flood recurrence of 1.67 years. Can be calculated using the `get_peakThreshold` function.
`...`     |     Optional arguments needed for `calc_allHIT` function


## Details

Descriptions of indices.
  

*  dh1 Annual maximum daily flow. Compute the maximum of a 1-day moving average flow for each year. dh1 is the mean (or median-Use Preference option) of these values. 

*  dh2 Annual maximum of 3-day moving average flows. Compute the maximum of a 3-day moving average flow for each year. dh2 is the mean (or median-Use Preference option) of these values; 

*  dh3 Annual maximum of 7-day moving average flows. Compute the maximum of a 7-day moving average flow for each year. dh3 is the mean (or median-Use Preference option) of these values. 

*  dh4 Annual maximum of 30-day moving average flows. Compute the maximum of a 30-day moving average flow for each year. dh4 is the mean (or median-Use Preference option) of these values. 

*  dh5 Annual maximum of 90-day moving average flows. Compute the maximum of a 90-day moving average flow for each year. dh5 is the mean (or median-Use Preference option) of these values. 

*  dh6 Variability of annual maximum daily flows. Compute the standard deviation for the maximum 1-day moving averages. dh6 is 100 times the standard deviation divided by the mean. 

*  dh7 Variability of annual maximum of 3-day moving average flows. Compute the standard deviation for the maximum 3-day moving averages. dh7 is 100 times the standard deviation divided by the mean. 

*  dh8 Variability of annual maximum of 7-day moving average flows. Compute the standard deviation for the maximum 7-day moving averages. dh8 is 100 times the standard deviation divided by the mean. 

*  dh9 Variability of annual maximum of 30-day moving average flows. Compute the standard deviation for the maximum 30-day moving averages. dh9 is 100 times the standard deviation divided by the mean. 

*  dh10 Variability of annual maximum of 90-day moving average flows. Compute the standard deviation for the maximum 90-day moving averages. dh10 is 100 times the standard deviation divided by the mean. 

*  dh11 Annual maximum of 1-day moving average flows divided by the median for the entire record. Compute the maximum of a 1-day moving average flow for each year. dh11 is the mean of these values divided by the median for the entire record. 

*  dh12 Annual maximum of 7-day moving average flows divided by the median for the entire record. Compute the maximum of a 7-day moving average flow for each year. dh12 is the mean of these values divided by the median for the entire record. 

*  dh13 Annual maximum of 30-day moving average flows divided by the median for the entire record. Compute the maximum of a 30-day moving average flow for each year. dh13 is the mean of these values divided by the median for the entire record. 

*  dh14 Flood duration. Compute the mean of the mean monthly flow values. Find the 95th percentile for the mean monthly flows. dh14 is the 95th percentile value divided by the mean of the monthly means. 

*  dh15 High flow pulse duration. Compute the average duration for flow events with flows above a threshold equal to the 75th percentile value for each year in the flow record. dh15 is the median of the yearly average durations. 

*  dh16 Variability in high flow pulse duration. Compute the standard deviation for the yearly average high pulse durations. dh16 is 100 times the standard deviation divided by the mean of the yearly average high pulse durations. 

*  dh17 High flow duration. Compute the average duration of flow events with flows above a threshold equal to the median flow value for the entire flow record. dh17 is the mean duration of the events. 

*  dh18 High flow duration. Compute the average duration of flow events with flows above a threshold equal to three times the median flow value for the entire flow record. dh18 is the mean duration of the events. 

*  dh19 High flow duration. Compute the average duration of flow events with flows above a threshold equal to seven times the median flow value for the entire flow record. dh19 is the mean duration of the events . 

*  dh20 High flow duration. Compute the 75th percentile value for the entire flow record. Compute the average duration of flow events with flows above a threshold equal to the 75th percentile value for the median annual flows. dh20 is the average duration of the events. 

*  dh21 High flow duration. Compute the 25th percentile value for the entire flow record. Compute the average duration of flow events with flows above a threshold equal to the 25th percentile value for the entire set of flows. dh21 is the average duration of the events. 

*  dh22 Flood interval. Compute the flood threshold as the flow equivalent for a flood recurrence of 1.67 years. Determine the median number of days between flood events for each year. dh22 is the mean (or median-Use Preference option) of the yearly median number of days between flood events. 

*  dh23 Flood duration. Compute the flood threshold as the flow equivalent for a flood recurrence of 1.67 years. Determine the number of days each year that the flow remains above the flood threshold. DH23 is the mean (or median-Use Preference option) of the number of flood days for years in which floods occur. 

*  dh24 Flood-free days. Compute the flood threshold as the flow equivalent for a flood recurrence of 1.67 years. Compute the maximum number of days that the flow is below the threshold for each year. DH24 is the mean (or median-Use Preference option) of the maximum yearly no-flood days.


## Value

A data.frame of flow statistics


## Examples

```r
x <- sampleData[c("date","discharge")]
yearType = "water"
calc_durationHigh(x=x,yearType=yearType)
```


# `calc_durationLow`

Indices describing duration of low flow events.


## Description

Calculates 24 indices used to describe the duration of low flow conditions.
 See Table X in the EflowStats package vignette for a full description of indices.


## Usage

```r
calc_durationLow(
  x,
  yearType = "water",
  wyMonth = 10L,
  digits = 3,
  pref = "mean",
  ...
)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
`yearType`     |     A character of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
`wyMonth`     |     A numeric. The month of the year in which the water year starts (1=January, 12=December). The water year begins on the first day of wyMonth.
`digits`     |     A numeric. Number of digits to round indice values
`pref`     |     A character of either "mean" or "median", indicating whether to use mean or median. See details.
`...`     |     Optional arguments needed for `calc_allHIT` function


## Details

Descriptions of indices.
  

*  dl1 Annual minimum daily flow. Compute the minimum 1-day average flow for each year. DL1 is the mean (or median-Use Preference option) of these values. 

*  dl2 Annual minimum of 3-day moving average flow. Compute the minimum of a 3-day moving average flow for each year. DL2 is the mean (or median-Use Preference option) of these values. 

*  dl3 Annual minimum of 7-day moving average flows. Compute the minimum of a 7-day moving average flow for each year. DL3 is the mean (or median-Use Preference option) of these values. 

*  dl4 Annual minimum of 30-day moving average flows. Compute the minimum of a 30-day moving average flow for each year. DH4 is the mean (or median-Use Preference option) of these values. 

*  dl5 Annual minimum of 90-day moving average flows. Compute the minimum of a 90-day moving average flow for each year. DH5 is the mean (or median-Use Preference option) of these values. 

*  dl6 Variability of annual minimum daily average flow. Compute the standard deviation for the minimum daily average flow. DL6 is 100 times the standard deviation divided by the mean. 

*  dl7 Variability of annual minimum of 3-day moving average flows. Compute the standard deviation for the minimum 3-day moving averages. DL7 is 100 times the standard deviation divided by the mean. 

*  dl8 Variability of annual minimum of 7-day moving average flows. Compute the standard deviation for the minimum 7-day moving averages. DL8 is 100 times the standard deviation divided by the mean. 

*  dl9 Variability of annual minimum of 30-day moving average flows. Compute the standard deviation for the minimum 30-day moving averages. DL9 is 100 times the standard deviation divided by the mean. 

*  dl10 Variability of annual minimum of 90-day moving average flows. Compute the standard deviation for the minimum 90-day moving averages. DH10 is 100 times the standard deviation divided by the mean. 

*  dl11 Annual minimum daily flow divided by the median for the entire record. Compute the minimum daily flow for each year. DL11 is the mean of these values divided by the median for the entire record. 

*  dl12 Annual minimum of 7-day moving average flows divided by the median for the entire record. Compute the minimum of a 7-day moving average flow for each year. DL12 is the mean of these values divided by the median for the entire record. 

*  dl13 Annual minimum of 30-day moving average flows divided by the median for the entire record. Compute the minimum of a 30-day moving average flow for each year. DL13 is the mean of these values divided by the median for the entire record. 

*  dl14 Low exceedence flows. Compute the 75-percent exceedence value for the entire flow record. DL14 is the exceedence value divided by the median for the entire record. 

*  dl15 Low exceedence flows. Compute the 90-percent exceedence value for the entire flow record. DL15 is the exceedence value divided by the median for the entire record. 

*  dl16 Low flow pulse duration. Compute the average pulse duration for each year for flow events below a threshold equal to the 25th percentile value for the entire flow record. DL16 is the median of the yearly average durations. 

*  dl17 Variability in low pulse duration. Compute the standard deviation for the yearly average low pulse durations. DL17 is 100 times the standard deviation divided by the mean of the yearly average low pulse durations. 

*  dl18 Number of zero-flow days. Count the number of zero-flow days for the entire flow record. DL18 is the mean (or median-Use Preference option) annual number of zero flow days. 

*  dl19 Variability in the number of zero-flow days. Compute the standard deviation for the annual number of zero-flow days. DL19 is 100 times the standard deviation divided by the mean annual number of zero-flow days. 

*  dl20 Number of zero-flow months. While computing the mean monthly flow values, count the number of months in which there was no flow over the entire flow record.


## Value

A data.frame of flow statistics


## Examples

```r
x <- sampleData[c("date","discharge")]
yearType = "water"
calc_durationLow(x=x,yearType=yearType)
```


# `calc_frequencyHigh`

Indices describing frequency of high flow events.


## Description

Calculates 11 indices used to describe the frequency of high flow conditions.
 See Table X in the EflowStats package vignette for a full description of indices.


## Usage

```r
calc_frequencyHigh(
  x,
  yearType = "water",
  wyMonth = 10L,
  digits = 3,
  pref = "mean",
  floodThreshold = NULL,
  ...
)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
`yearType`     |     A character of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
`wyMonth`     |     A numeric. The month of the year in which the water year starts (1=January, 12=December). The water year begins on the first day of wyMonth.
`digits`     |     A numeric. Number of digits to round indice values
`pref`     |     A character of either "mean" or "median", indicating whether to use mean or median. See details.
`floodThreshold`     |     Numeric flood threshold as the flow equivalent for a flood recurrence of 1.67 years
`...`     |     Optional arguments needed for `calc_allHIT` function


## Details

Descriptions of indices.
  

*  fh1 High flood pulse count. Compute the average number of flow events with flows above a threshold equal to the 75th percentile value for the entire flow record. FH1 is the average (or median-Use Preference option) number of events. 

*  fh2 Variability in high pulse count. Compute the standard deviation in the annual pulse counts for FH1. FH2 is 100 times the standard deviation divided by the mean pulse count (number of events/year-spatial). 

*  fh3 High flood pulse count. Compute the average number of days per year that the flow is above a threshold equal to three times the median flow for the entire record. FH3 is the mean (or median-Use Preference option) of the annual number of days for all years. 

*  fh4 High flood pulse count. Compute the average number of days per year that the flow is above a threshold equal to seven times the median flow for the entire record. FH4 is the mean (or median - Use Preference option) of the annual number of days for all years. 

*  fh5 Flood frequency. Compute the average number of flow events with flows above a threshold equal to the median flow value for the entire flow record. FH5 is the average (or median - Use Preference option) number of events. 

*  fh6 Flood frequency. Compute the average number of flow events with flows above a threshold equal to three times the median flow value for the entire flow record. FH6 is the average (or median-Use Preference option) number of events. 

*  fh7 Flood frequency. Compute the average number of flow events with flows above a threshold equal to seven times the median flow value for the entire flow record. FH7 is the average (or median-Use Preference option) number of events. 

*  fh8 Flood frequency. Compute the average number of flow events with flows above a threshold equal to 25-percent exceedence value for the entire flow record. FH8 is the average (or median-Use Preference option) number of events. 

*  fh9 Flood frequency. Compute the average number of flow events with flows above a threshold equal to 75-percent exceedence value for the entire flow record. FH9 is the average (or median-Use Preference option) number of events. 

*  fh10 Flood frequency. Compute the average number of flow events with flows above a threshold equal to median of the annual minima for the entire flow record. FH10 is the average (or median-Use Preference option) number of events. 

*  fh11 Flood frequency. Compute the average number of flow events with flows above a threshold equal to flow corresponding to a 1.67-year recurrence interval. FH11 is the average (or median-Use Preference option) number of events.


## Value

A data.frame of flow statistics


## Examples

```r
x <- sampleData[c("date","discharge")]
yearType = "water"
floodThreshold = 1158
calc_frequencyHigh(x=x,yearType=yearType,floodThreshold = 1158)
```


# `calc_frequencyLow`

Indices describing frequency of low flow events.


## Description

Calculates 3 indices used to describe the frequency of low flow conditions.
 See Table X in the EflowStats package vignette for a full description of indices.


## Usage

```r
calc_frequencyLow(
  x,
  yearType = "water",
  wyMonth = 10L,
  digits = 3,
  pref = "mean",
  ...
)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
`yearType`     |     A character of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
`wyMonth`     |     A numeric. The month of the year in which the water year starts (1=January, 12=December). The water year begins on the first day of wyMonth.
`digits`     |     A numeric. Number of digits to round indice values
`pref`     |     A character of either "mean" or "median", indicating whether to use mean or median. See details.
`...`     |     Optional arguments needed for `calc_allHIT` function


## Details

Descriptions of indices.
  

*  FL1; Low flood pulse count. Compute the average number of flow events with flows below a threshold equal to the 25th percentile value for the entire flow record. FL1 is the average (or median-Use Preference option) number of events. 

*  FL2; Variability in low pulse count. Compute the standard deviation in the annual pulse counts for FL1. FL2 is 100 times the standard deviation divided by the mean pulse count. 

*  FL3; Frequency of low pulse spells. Compute the average number of flow events with flows below a threshold equal to 5 percent of the mean flow value for the entire flow record. FL3 is the average (or median-Use Preference option) number of events.


## Value

A data.frame of flow statistics


## Examples

```r
x <- sampleData[c("date","discharge")]
yearType = "water"
calc_frequencyLow(x=x,yearType=yearType)
```


# `calc_magAverage`

Indices describing magnitude of the average flow condition.


## Description

Calculates 45 indices used to describe the magnitude of the average flow condition.
 See Table X in the EflowStats package vignette for a full description of indices.


## Usage

```r
calc_magAverage(
  x,
  yearType = "water",
  wyMonth = 10L,
  digits = 3,
  drainArea = NULL,
  pref = "mean",
  ...
)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
`yearType`     |     A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
`wyMonth`     |     A numeric. The month of the year in which the water year starts (1=January, 12=December). The water year begins on the first day of wyMonth.
`digits`     |     A numeric. Number of digits to round indice values
`drainArea`     |     A numeric specifying the drainage area. Only required for ma41 statistic. Typically square miles, see details.
`pref`     |     A character of either "mean" or "median", indicating whether to use mean or median. See details.
`...`     |     Optional arguments needed for `calc_allHIT` function


## Details

Descriptions of indices.
  

*  ma1  Mean of the daily mean flow values for the entire flow record 

*  ma2  Median of the daily mean flow values for the entire flow record 

*  ma3  Mean (or median - use preference option) of the coefficients of variation (standard deviation/mean) for each year. Compute the coefficient of variation for each year of daily flows. Compute the mean of the annual coefficients of variation 

*  ma4  Standard deviation of the percentiles of the logs of the entire flow record divided by the mean of percentiles of the logs. Compute the log(10) of the daily flows for the entire record. Compute the 5th, 10th, 15th, 20th, 25th, 30th, 35th, 40th, 45th, 50th, 55th, 60th, 65th, 70th, 75th, 80th, 85th, 90th and 95th percentiles for the logs of the entire flow record. Percentiles are computed by interpolating between the ordered (ascending) logs of the flow values. Compute the standard deviation and mean for the percentile values. Divide the standard deviation by the mean 

*  ma5  The skewness of the entire flow record is computed as the mean for the entire flow record (ma1) divided by the median (ma2) for the entire flow record 

*  ma6  Range in daily flows is the ratio of the 10-percent to 90-percent exceedence values for the entire flow record. Compute the 5-percent to 95-percent exceedence values for the entire flow record. Exceedence is computed by interpolating between the ordered (descending) flow values. Divide the 10-percent exceedence by the 90-percent value 

*  ma7  Range in daily flows is computed in the same way as ma6 except using the 20-percent and 80-percent exceedence values. Divide the 20-percent exceedence value by the 80-percent value 

*  ma8  Range in daily flows is computed in the same way as ma6 except using the 25-percent and 75-percent exceedence values. Divide the 25-percent exceedence value by the 75-percent value 

*  ma9  Spread in daily flows is the ratio of the difference between the 90th and 10th percentile of the logs of the flow data to the log of the median of the entire flow record. Compute the log(10) of the daily flows for the entire record. Compute the 5th, 10th, 15th, 20th, 25th, 30th, 35th, 40th, 45th, 50th, 55th, 60th, 65th, 70th, 75th, 80th, 85th, 90th and 95th percentiles for the logs of the entire flow record. Percentiles are computed by interpolating between the ordered (ascending) logs of the flow values. Compute ma9 as (90th-10th)/log10(ma2) 

*  ma10  Spread in daily flows is computed in the same way as ma9 except using the 20th and 80th percentiles 

*  ma11  Spread in daily flows is computed in the same way as ma9 except using the 25th and 75th percentiles. 

*  ma12_23  Requires pref argument to be either "mean" or "median" specifying monthly aggregation function. Default is "mean". Means (or medians - use preference option) of monthly flow values. Compute the means for each month over the entire flow record. For example, ma12 is the mean of all January flow values over the entire record. 

*  ma24_35  Variability (coefficient of variation) of monthly flow values. Compute the standard deviation for each month in each year over the entire flow record. Divide the standard deviation by the mean for each month. Take the mean (or median - use preference option) of these values for each month across all years. 

*  ma36_40  Variability and skewness across monthly flows. ma36 - compute the minimum, maximum and mean flows for each month in the entire flow record. ma36 is the maximum monthly flow minus the minimum monthly flow divided by the median monthly flow. ma37 - compute the first (25th percentile) and the third (75th percentile) quartiles. ma37 is the third quartile minus the first quartile divided by the median of the monthly means. ma38 - compute the 10th and 90th percentiles for the monthly means. ma38 is the 90th percentile minus the 10th percentile divided by the median of the monthly means. ma39 - compute the standard deviation for the monthly means. ma39 is the standard deviation times 100 divided by the mean of the monthly means. ma40 - skewness in the monthly flows. ma40 is the mean of the monthly flow means minus the median of the monthly means divided by the median of the monthly means. 

*  ma41_45 ma41 requires drainArea to be specified. Annual runoff and the variability and skewness across annual flows. ma41 - compute the annual mean daily flows. ma41 is the mean of the annual means divided by the drainage area. ma42 is the maximum annual flow minus the minimum annual flow divided by the median annual flow. ma43 - compute the first (25th percentile) and third (75th percentile) quartiles for the annual means. ma43 is the third quartile minus the first quartile divided by the median of the annual means. ma44 - compute the 10th and 90th percentiles for the annual means. ma44 is the 90th percentile minus the 10th percentile divided by the median of the annual means. ma45 - skewness in the annual flows. ma45 is the mean of the annual flow means minus the median of the annual means divided by the median of the annual means.


## Value

A data.frame flow statistics


## Examples

```r
x <- sampleData[c("date","discharge")]
drainArea <- 50
yearType = "water"
calc_magAverage(x=x,yearType=yearType,drainArea=drainArea)
```


# `calc_magHigh`

Indices describing magnitude of the peak flow condition.


## Description

Calculates 27 indices used to describe the magnitude of the peak flow condition.
 See Table X in the EflowStats package vignette for a full description of indices.


## Usage

```r
calc_magHigh(
  x,
  yearType = "water",
  wyMonth = 10L,
  digits = 3,
  drainArea = NULL,
  pref = "mean",
  ...
)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
`yearType`     |     A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
`wyMonth`     |     A numeric. The month of the year in which the water year starts (1=January, 12=December). The water year begins on the first day of wyMonth.
`digits`     |     A numeric. Number of digits to round indice values
`drainArea`     |     A numeric specifying the drainage area. Only required for mh20 statistic. Typically squiare miles, see details.
`pref`     |     A character of either "mean" or "median", indicating whether to use mean or median. See details.
`...`     |     Optional arguments needed for `calc_allHIT` function


## Details

Descriptions of indices.
  

*  mh1_12  Requires pref argument to be either "mean" or "median" specifying monthly aggregation function. Default is "mean". Means (or medians - use preference option) of maximum daily flow values for each month. For example, mh1 is the mean of all January maximum flow values over the entire record. 

*  mh13 variability (coefficient of variation) across maximum monthly flow values. Compute the mean and standard deviation for the maximum monthly flows over the entire flow record. MH13 is the standard deviation times 100 divided by the mean maximum monthly flow for all years. 

*  mh14 median of annual maximum flows. Compute the annual maximum flows from monthly maximum flows. Compute the ratio of annual maximum flow to median annual flow for each year. MH14 is the median of these ratios. 

*  mh15_17 MH15; High flow discharge index. Compute the 1-percent exceedence value for the entire data record. MH15 is the 1-percent exceedence value divided by the median flow for the entire record. MH16; Compute the 10-percent exceedence value for the entire data record. MH16 is the 10-percent exceedence value divided by the median flow for the entire record. MH17; Compute the 25-percent exceedence value for the entire data record. MH17 is the 25-percent exceedence value divided by the median flow for the entire record. 

*  mh18 variability across annual maximum flows. Compute the logs (log10) of the maximum annual flows. Find the standard deviation and mean for these values. MH18 is the standard deviation times 100 divided by the mean. 

*  mh19 the skewness in annual maximum flows (dimensionless-spatial). Use the equation: MH19   numerator =   N2 ? sum(qm3)-3N ? sum(qm) ? sum(qm2) + 2 ? (sum(qm))3 denominator = N ? (N-1) ? (N-2) ? stddev3 Where:  N = Number of years qm = Log10 (annual maximum flows) stddev = Standard deviation of the annual maximum flows 

*  mh20 specific mean annual maximum flow. MH20 is the mean (or median-Use Preference option) of the annual maximum flows divided by the drainage area (cubic feet per second/square mile-temporal). 

*  mh21_27 high flow volume indices. Compute the average volume for flow events above a threshold. Thresholds are equal to the median flow for the entire record for mh21, 3 times the median flow for the entire record for mh22,and 7 times the median flow for the entire record for mh23. Thresholds are equal to the median flow for the entire record for mh24, 3 times the median flow for the entire record for mh25, 7 times the median flow for the entire record for mh26, and the 75th percentile for the entire record for mh27. MH21 through 23 are the average volumes divided by the median flow for the entire record. MH24 through 27 are the average peak flows divided by the median flow for the entire record.


## Value

A data.frame of flow statistics


## Examples

```r
x <- sampleData[c("date","discharge")]
drainArea <- 50
yearType = "water"
calc_magHigh(x=x,yearType=yearType,drainArea=drainArea)
```


# `calc_magLow`

Indices describing magnitude of the low flow condition.


## Description

Calculates 27 indices used to describe the magnitude of the peak flow condition.
 See Table X in the EflowStats package vignette for a full description of indices.


## Usage

```r
calc_magLow(
  x,
  yearType = "water",
  wyMonth = 10L,
  digits = 3,
  drainArea = NULL,
  pref = "mean",
  ...
)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
`yearType`     |     A character of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
`wyMonth`     |     A numeric. The month of the year in which the water year starts (1=January, 12=December). The water year begins on the first day of wyMonth.
`digits`     |     A numeric. Number of digits to round indice values
`drainArea`     |     A numeric specifying the drainage area. Only required for ml22 statistic. Typically squiare miles, see details.
`pref`     |     A character of either "mean" or "median", indicating whether to use mean or median. See details.
`...`     |     Optional arguments needed for `calc_allHIT` function


## Details

Descriptions of indices.
  

*  mh1_12  Requires pref argument to be either "mean" or "median" specifying monthly aggregation function. Default is "mean". Means (or medians - use preference option) of maximum daily flow values for each month. For example, mh1 is the mean of all January maximum flow values over the entire record. 

*  mh13 variability (coefficient of variation) across minimum monthly flow values. Compute the mean and standard deviation for the maximum monthly flows over the entire flow record. MH13 is the standard deviation times 100 divided by the mean maximum monthly flow for all years. 

*  ml14 Mean of annual minimum annual flows.  ML14 is the mean of the ratios of minimum annual flows to the median flow for each year. 

*  ml15 Low flow index. ML15 is the mean (or median-Use Preference option) of the ratios of minimum annual flows to the mean flow for each year. 

*  ml16 Median of annual minimum flows.  ML16 is the median of the ratios of minimum annual flows to the median flow for each year. 

*  ml17 Baseflow 1. Compute the mean annual flows. Compute the minimum of a 7-day moving average flow for each year and divide them by the mean annual flow for that year. ML17 is the mean (or median-Use Preference option) of those ratios. 

*  ml18 Variability in baseflow 1. Compute the standard deviation for the ratios of minimum 7-day moving average flows to mean annual flows for each year.  ML18 is the standard deviation times 100 divided by the mean of the ratios. 

*  ml19 Baseflow 2. Compute the ratios of the minimum annual flow to mean annual flow for each year. ML19 is the mean (or median-Use Preference option) of these ratios times 100. 

*  ml20 Baseflow 3. Divide the daily flow record into 5-day blocks. Find the minimum flow for each block. Assign the minimum flow as a base flow for that block if 90 percent of that minimum flow is less than the minimum flows for the blocks on either side. Otherwise, set it to zero. Fill in the zero values using linear interpolation. Compute the total flow for the entire record and the total base flow for the entire record. ML20 is the ratio of total base flow to total flow. 

*  ml21 Variability across annual minimum flows. Compute the mean and standard deviation for the annual minimum flows. ML21 is the standard deviation times 100 divided by the mean. 

*  ml22 Specific mean annual minimum flow.  ML22 is the mean (or median-Use Preference option) of the annual minimum flows divided by the drainage area.


## Value

A data.frame of flow statistics


## Examples

```r
x <- sampleData[c("date","discharge")]
drainArea <- 50
yearType = "water"
calc_magLow(x=x,yearType=yearType,drainArea=drainArea)
```


# `calc_magnifSeven`

Function to return the magnificent seven statistics for a given data series


## Description

Function to return the magnificent seven statistics for a given data series


## Usage

```r
calc_magnifSeven(x, yearType = "water", wyMonth = 10L, digits = 3)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
`yearType`     |     A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
`wyMonth`     |     A numeric. The month of the year in which the water year starts (1=January, 12=December). The water year begins on the first day of wyMonth.
`digits`     |     A numeric. Number of digits to round indice values


## Details

This is a function to compute the 7 statistics of daily streamflow
 used by Archfield et al., under revision (June 2013).


## Value

data.frame of calculated statistics


## Examples

```r
x<-sampleData[c("date","discharge")]
magSeven <- calc_magnifSeven(x)
```


# `calc_rateChange`

Indices describing rate of change of flow.


## Description

Calculates 9 indices used to describe the rate of change of flow conditions.
 See Table X in the EflowStats package vignette for a full description of indices.


## Usage

```r
calc_rateChange(
  x,
  yearType = "water",
  wyMonth = 10L,
  digits = 3,
  pref = "mean",
  ...
)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
`yearType`     |     A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
`wyMonth`     |     A numeric. The month of the year in which the water year starts (1=January, 12=December). The water year begins on the first day of wyMonth.
`digits`     |     A numeric. Number of digits to round indice values
`pref`     |     A character of either "mean" or "median", indicating whether to use mean or median. See details.
`...`     |     Optional arguments needed for `calc_allHIT` function


## Details

Descriptions of indices.
  

*  ra1; Rise rate. Compute the change in flow for days in which the change is positive for the entire flow record. RA1 is the mean (or median-Use Preference option) of these values. 

*  ra2; Variability in rise rate. Compute the standard deviation for the positive flow changes. RA2 is 100 times the standard deviation divided by the mean. 

*  ra3; Fall rate. Compute the change in flow for days in which the change is negative for the entire flow record. RA3 is the mean (or median-Use Preference option) of these values. 

*  ra4; Variability in fall rate. Compute the standard deviation for the negative flow changes. RA4 is 100 times the standard deviation divided by the mean. 

*  ra5; Number of day rises. Compute the number of days in which the flow is greater than the previous day. RA5 is the number of positive gain days divided by the total number of days in the flow record. 

*  ra6; Change of flow. Compute the log of the flows for the entire flow record. Compute the change in log of flow for days in which the change is positive for the entire flow record. RA6 is the median of these values. 

*  ra7; Change of flow. Compute the log of the flows for the entire flow record. Compute the change in log of flow for days in which the change is negative for the entire flow record. RA7 is the median of these log values. 

*  ra8; Number of reversals. Compute the number of days in each year when the change in flow from one day to the next changes direction. RA8 is the average (or median - Use Preference option) of the yearly values. 

*  ra9; Variability in reversals. Compute the standard deviation for the yearly reversal values. RA9 is 100 times the standard deviation divided by the mean.


## Value

A data.frame of flow statistics


## Examples

```r
x <- sampleData[c("date","discharge")]
yearType = "water"
calc_rateChange(x=x,yearType=yearType)
```


# `calc_timingAverage`

Indices describing timing of average flow events.


## Description

Calculates 3 indices used to describe the timing of average flow conditions.
 See Table X in the EflowStats package vignette for a full description of indices.


## Usage

```r
calc_timingAverage(
  x,
  yearType = "water",
  wyMonth = 10L,
  digits = 3,
  pref = "mean",
  floodThreshold = NULL,
  ...
)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
`yearType`     |     A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
`wyMonth`     |     A numeric. The month of the year in which the water year starts (1=January, 12=December). The water year begins on the first day of wyMonth.
`digits`     |     A numeric. Number of digits to round indice values
`pref`     |     A character of either "mean" or "median", indicating whether to use mean or median. See details.
`floodThreshold`     |     Numeric flood threshold as the flow equivalent for a flood recurrence of 1.67 years
`...`     |     Optional arguments needed for `calc_allHIT` function


## Details

Descriptions of indices.
  

*  ta1; Constancy. Constancy is computed via the formulation of Colwell (see example in Colwell, 1974). A matrix of values is compiled where the columns are 11 flow categories and the rows are 365 days of the year (no leap years) defined as either calendar or water year. February 29th is removed on leap years. The cell values are the number of times that a flow falls into a category on each day. The categories are listed below. The row totals, column totals, and grand total are computed. Using the equations for Shannon information theory parameters, constancy is computed as 1-(uncertainty with respect to state)/log10(number of state)  

*  log(flow) < .1 x log(mean flow) 

*  .1 x log(mean flow) <= log(flow) < .25 x log(mean flow) 

*  .25 x log(mean flow) <= log(flow) < .5 x log(mean flow) 

*  .5 x log(mean flow) <= log(flow) < .75 x log(mean flow) 

*  .75 x log(mean flow) <= log(flow) < 1.0 x log(mean flow) 

*  1.0 x log(mean flow) <= log(flow) < 1.25 x log(mean flow) 

*  1.25 x log(mean flow) <= log(flow) < 1.5 x log(mean flow) 

*  1.5 x log(mean flow) <= log(flow) < 1.75 x log(mean flow) 

*  1.75 x log(mean flow) <= log(flow) < 2.0 x log(mean flow) 

*  2.0 x log(mean flow) <= log(flow) < 2.25 x log(mean flow) 

*  log(flow) >= 2.25 x log(mean flow)  

*  ta2; Predictability. Predictability is computed from the same matrix as constancy (see example in Colwell, 1974). It is computed as: 1- (uncertainty with respect to interaction of time and state - uncertainty with respect to time)/log10(number of state) 

*  ta3; Seasonal predictability of flooding. Divide years up into 2-month periods (that is, Oct-Nov, Dec-Jan, and so forth). Count the number of flood days (flow events with flows > 1.67-year flood) in each period over the entire flow record. TA3 is the maximum number of flood days in any one period divided by the total number of flood days.


## Value

A data.frame of flow statistics


## Examples

```r
x <- sampleData[c("date","discharge")]
yearType = "water"
floodThreshold = 1158
calc_timingAverage(x=x,yearType = yearType,floodThreshold =floodThreshold)
```


# `calc_timingHigh`

Indices describing timing of high flow events.


## Description

Calculates 3 indices used to describe the timing of high flow conditions.
 See Table X in the EflowStats package vignette for a full description of indices.


## Usage

```r
calc_timingHigh(
  x,
  yearType = "water",
  wyMonth = 10L,
  digits = 3,
  pref = "mean",
  floodThreshold = NULL,
  ...
)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
`yearType`     |     A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
`wyMonth`     |     A numeric. The month of the year in which the water year starts (1=January, 12=December). The water year begins on the first day of wyMonth.
`digits`     |     A numeric. Number of digits to round indice values
`pref`     |     A character of either "mean" or "median", indicating whether to use mean or median. See details.
`floodThreshold`     |     Numeric flood threshold as the flow equivalent for a flood recurrence of 1.67 years
`...`     |     Optional arguments needed for `calc_allHIT` function


## Details

Descriptions of indices.
  

*  th1 Julian date of annual maximum. Determine the Julian date that the maximum flow occurs for each year. Transform the dates to relative values on a circular scale (radians or degrees). Compute the x and y components for each year and average them across all years. Compute the mean angle as the arc tangent of y-mean divided by x-mean. Transform the resultant angle back to Julian date. 

*  th2 Variability in Julian date of annual maxima. Compute the coefficient of variation for the mean x and y components and convert to a date. 

*  th3 Seasonal predictability of nonflooding. Computed as the maximum proportion of a 365-day year that the flow is less than the 1.67-year flood threshold and also occurs in all years. Accumulate nonflood days that span all years. TH3 is maximum length of those flood-free periods divided by 365. 
 Note: In these definitions, "Julian date" should be interpreted as the count of days starting with 1 on January
 first of a given year, ending at 365 or 366 on December 31st.


## Value

A data.frame of flow statistics


## Examples

```r
x <- sampleData[c("date","discharge")]
yearType = "water"
floodThreshold = 1158
calc_timingHigh(x=x,yearType=yearType,floodThreshold=floodThreshold)
```


# `calc_timingLow`

Indices describing timing of low flow events.


## Description

Calculates 3 indices used to describe the timing of high flow conditions.
 See Table X in the EflowStats package vignette for a full description of indices.


## Usage

```r
calc_timingLow(
  x,
  yearType = "water",
  wyMonth = 10L,
  digits = 3,
  pref = "mean",
  floodThreshold = NULL,
  ...
)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
`yearType`     |     A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
`wyMonth`     |     A numeric. The month of the year in which the water year starts (1=January, 12=December). The water year begins on the first day of wyMonth.
`digits`     |     A numeric. Number of digits to round indice values
`pref`     |     A character of either "mean" or "median", indicating whether to use mean or median. See details.
`floodThreshold`     |     Numeric flood threshold as the flow equivalent for a flood recurrence of 1.67 years
`...`     |     Optional arguments needed for `calc_allHIT` function


## Details

Descriptions of indices.
  

*  tl1; Julian date of annual minimum. Determine the Julian date that the minimum flow occurs for each water year. Transform the dates to relative values on a circular scale (radians or degrees). Compute the x and y components for each year and average them across all years. Compute the mean angle as the arc tangent of y-mean divided by x-mean. Transform the resultant angle back to Julian date. 

*  tl2 Variability in Julian date of annual minima. Compute the coefficient of variation for the mean x and y components and convert to a date. 

*  tl3 Seasonal predictability of low flow. Divide years up into 2-month periods (that is, Oct-Nov, Dec-Jan, and so forth). Count the number of low flow events (flow events with flows <= 5 year flood threshold) in each period over the entire flow record. TL3 is the maximum number of low flow events in any one period divided by the total number of low flow events. 

*  tl4 Seasonal predictability of non-low flow. Compute the number of days that flow is above the 5-year flood threshold as the ratio of number of days to 365 or 366 (leap year) for each year. TL4 is the maximum of the yearly ratios. 
 Note: In these definitions, "Julian date" should be interpreted as the count of days starting with 1 on January
 first of a given year, ending at 365 or 366 on December 31st.


## Value

A data.frame of flow statistics


## Examples

```r
x <- sampleData[c("date","discharge")]
yearType = "water"
floodThreshold = 1161.38
calc_timingLow(x=x,yearType=yearType,floodThreshold=floodThreshold)
```


# `cut_dataToWaterYear`

Cuts the discharge time series to full water years


## Description

Cuts the discharge time series to full water years


## Usage

```r
cut_dataToWaterYear(x, yearType, wyMonth = 10L)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     data.frame containing a vector of date values in the first column and vector of numeric flow values in the second column.
`yearType`     |     A character of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
`wyMonth`     |     A numeric. The month of the year in which the water year starts (1=January, 12=December). The water year begins on the first day of wyMonth.


## Details

To ensure the input data is valid for use with other
 EflowStats functions.
 #'   

*  First column must be of class `Date`.  

*  Second must be of class `numeric`.  

*  `yearType` input must be either "water" or "calendar".  

*  `wyMonth`input must be of class `integer`.


## Value

data.frame in original structure, but cut to full water years


## Examples

```r
x <- sampleData[, c('date', 'discharge')]
cut_dataToWaterYear(x,10L)
```


# `find_changeEvents`

Function to decompose a flow series into flow direction change events


## Description

Function to decompose a flow series into flow direction change events


## Usage

```r
find_changeEvents(x)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     A vector of flow values, should be sorted chronologically.


## Details

This function accepts a vector of flow values, tests if each value in the flow vector
 is greater or less than the preceding value of flow, and classifies the flow vector into events. An event
 is defined as when a change in flow from one day to the next changes direction, e.g. rising limb to falling limb.
 If there is no change in flow from one day to the next it is considered part of the preceding event.
 If there is no change in flow for the whole timeseries, the timeseries is assigned event 0.


## Value

A dataframe with columns "flow" and "event"


## Examples

```r
x <- sampleData$discharge
find_changeEvents(x)
```


# `find_eventDuration`

Function to decompose a flow series into high flow events defined as flows above a given threshold.


## Description

This function accepts a vector of flow values, tests if each value in the flow vector
 is above or below a defined threshold, classifies the flow vector into events, and returns the
 average event duration or all event durations if average = FALSE. An event
 is defined as consecutive entries above the given threshold.


## Usage

```r
find_eventDuration(
  x,
  threshold,
  aggType = "average",
  type = "high",
  pref = "mean",
  trim = FALSE
)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     A vector of numeric flow values
`threshold`     |     Numeric threshold value for high flow events
`aggType`     |     Character vector indicating type of aggregation to use, if any. Choices are "average", "min", "max" or "none". If "average", mean or median of events is returned depending on "pref" setting If "none", returns the duration for each individual event
`type`     |     Character of either "high" or "low" indicating if events are defined as flows below or above the given threshold, respectively
`pref`     |     A character of either "mean" or "median", indicating whether to use mean or median. See details.
`trim`     |     Logical. Events that start or end at the beginning or end of the record are dropped if `TRUE` because an accurate duration can't be calculated if teh start or end time is unknown.


## Value

A numeric of the mean event duration in units of number of rows if average = TRUE. Otherwise, returns a datafarme of durations for each event.


## Examples

```r
x <- sampleData$discharge
threshold <- median(x,na.rm=TRUE)
find_eventDuration(x,threshold)
```


# `find_events`

Function to decompose a flow series into high or low flow events.


## Description

This function accepts a vector of flow values, tests if each value in the flow vector
 is above or below a defined threshold, and classifies the flow vector into events. An event
 is defined as consecutive values above or below the given threshold.


## Usage

```r
find_events(x, threshold, type = "high")
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     A numeric vector of flow values
`threshold`     |     Numeric threshold value defining event
`type`     |     character indicating type of event. High flags events above threshold, low flags events below threshold.


## Value

A dataframe with columns "flow" and "event"


## Examples

```r
x <- sampleData$discharge
threshold <- median(x,na.rm=TRUE)
find_events(x,threshold)
```


# `get_peakThreshold`

Function to return a specified flood threshold


## Description

This function calculates flood thresholds for specified recurrence intervals.


## Usage

```r
get_peakThreshold(x, peakValues, perc = 0.6, yearType = "water", wyMonth = 10L)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
`peakValues`     |     A dataframe containing a vector of date values in the first column and vector of numeric annual peak flow values in the second column.
`perc`     |     value containing the desired percentile to be calculated
`yearType`     |     A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
`wyMonth`     |     A numeric. The month of the year in which the water year starts (1=January, 12=December). The water year begins on the first day of wyMonth.


## Details

Compute the log10 of the daily
 flows for the peak annual flow days. Calculate the coefficients for a linear
 regression equation for logs of peak annual flow versus logs of average daily
 flow for peak days. Using the log peak flow for the 1.67-year recurrence
 interval (60th percentile, `perc=0.6` ) as input to the regression
 equation, predict the log10 of the average daily flow. The threshold is 10 to
 the log10 (average daily flow) power (cubic feet per second). for the 5-year
 recurrence interval (80th percentile, `perc=0.8` ), used by indices TL3
 and TL4, follow the same process, inputing a different 'perc' value.


## Value

thresh numeric containing the flood threshold for recurence interval
 specified by `perc`


## Examples

```r
library(dataRetrieval)
x <- sampleData[c("date","discharge")]
sites<-"02178400"
peakValues <- readNWISpeak(sites)
peakValues <- peakValues[c("peak_dt","peak_va")]
get_peakThreshold(x,peakValues,.6,yearType="water")
```


# `get_seasonality`

Function to compute the seasonal factors (amplitude and phase) for a given data series


## Description

This function accepts a data frame containing daily streamflow data, then computes get_seasonality
 variables by first standardizing flows, the fitting relation
 A*cos(2*pi*t) + B*sin(2*pi*t)1) Get decimal yearand returns the amplitude and phase


## Usage

```r
get_seasonality(x, yearType = "water", wyMonth = 10L)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
`yearType`     |     A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
`wyMonth`     |     A numeric. The month of the year in which the water year starts (1=January, 12=December). The water year begins on the first day of wyMonth.


## Value

get_seasonality vector of seasonal factors (amplitude and phase)


## Examples

```r
x <- sampleData[c("date","discharge")]
get_seasonality(x=x)
```


# `get_waterYear`

Function to return the water year for a given date


## Description

Function to return the water year for a given date


## Usage

```r
get_waterYear(x, wyMonth = 10L, numeric = TRUE)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     A date vector
`wyMonth`     |     A numeric. The month of the year in which the water year starts (1=January, 12=December). The water year begins on the first day of wyMonth.
`numeric`     |     Logical. Return values are numeric vector or vector of ordered factors


## Value

A vector of numeric water years for each date if `numeric = TRUE` otherwise an ordered factor of water years if `numeric = FALSE`


## Examples

```r
get_waterYear(sampleData$date)
```


# `get_waterYearDay`

Day of water year


## Description

Given a vector of dates, calculates day of water year accounting for leap years.


## Usage

```r
get_waterYearDay(x, wyMonth = 10L)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     A vector of class date.
`wyMonth`     |     A numeric. The month of the year in which the water year starts (1=January, 12=December). The water year begins on the first day of wyMonth.


## Value

A numeric vector of day of water year


## Examples

```r
x <- seq(from=as.Date("2010-01-01"),to=as.Date("2016-01-01"),by="1 days")
year_day <- get_waterYearDay(x, 2L)
```


# `sampleData`

Sample Streamflow data.frame


## Description

A dataset with a few years of complete water year based data.


## Format

A data frame with 731 rows and 7 variables:
 list("\n", "  ", list(list("wy_val"), list("integer of the water year")), "\n", "  ", list(list("date"), list("Date of the complete day")), "\n", "  ", list(list("discharge"), list("Discharge values")), "\n", "  ", list(list("month_val"), list("integer month value")), "\n", "  ", list(list("year_val"), list("integer year value")), "\n", "  ", list(list("day_val"), list("integer day value")), "\n", "  ", list(list("jul_val"), list("day of year")), "\n")


# `validate_data`

validate_data Discharge timeseries screening


## Description

Function to check dataframe inputs for appropriate data
 classes and screen for missing values.


## Usage

```r
validate_data(x, yearType, wyMonth = 10L)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
`yearType`     |     A character of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
`wyMonth`     |     A numeric. The month of the year in which the water year starts (1=January, 12=December). The water year begins on the first day of wyMonth.


## Details

Checks performed ensure the data is valid for use with other
 EflowStats functions.
 #'   

*  First column must be of class `Date`.  

*  Second must be of class `numeric`.  

*  `yearType` input must be either "water" or "calendar".  

*  Every year as defined by the `yearType` input must be complete.


## Value

data.frame with rows sorted by date. Boolean FALSE if data is not
 found to be valid. (See details)


## Examples

```r
x <- sampleData[c("date","discharge")]
yearType = "water"
validate_data(x=x,yearType=yearType)
```


