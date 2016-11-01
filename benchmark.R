library(EflowStatslite)

dateVector <- seq(from=as.Date("1970-10-01"),
                  to= as.Date("2000-09-30"),
                  by="1 days")
qVector <- runif(length(dateVector),1,1000)

Qdata <- data.frame(date=dateVector,
                    discharge = qVector)
system.time({
for(i in 1:20)
{
mhStats <- magHigh(Qdata)
#mlStats <- magLow(Qdata)
#maStats <- magAverage(Qdata)
}
})

detach("package:EflowStatslite", unload=TRUE)

library(EflowStats)
library(lubridate)

system.time({
for(i in 1:20)
{
Qdata$wy_val <- EflowStatslite::waterYear(Qdata$date,numeric=TRUE)
Qdata$year_val <- EflowStatslite::waterYear(Qdata$date,numeric=TRUE)

Qdata$month_val <- month(Qdata$date)

stats <- mh1.12(Qdata)
stats <- mh13(Qdata)
stats <- mh14(Qdata)
stats <- mh15.17(Qdata)
stats <- mh18(Qdata)
stats <- mh19(Qdata)
stats <- mh20(Qdata,drainarea = 50)
stats <- mh21(Qdata)
stats <- mh22(Qdata)
stats <- mh23(Qdata)
stats <- mh24(Qdata)
stats <- mh25(Qdata)
stats <- mh26(Qdata)
stats <- mh27(Qdata)
}
})

detach("package:EflowStats", unload=TRUE)

