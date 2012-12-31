# wps.des: id=test_stats, title = test stats, abstract = Finds the mean daily flow median daily flow and skewness of daily flow in the input dataset;
# wps.in: sos_url, string, SOS Endpoint, A fully formed SOS GetObservations request that will return a SWE common CSV block holding date and flow;
# wps.in: sites, string, list of sites, A list of sites;
# wps.in: property, string, Observed Property, The SOS observed property to request;

library(XML)
library(zoo)
library(chron)
library(doBy)
#library(dataRetrieval)
SWE_CSV_IHA <- function(input) {
	cat(paste("Retrieving data from: \n", input, "\n", 
		sep = " "))
	flow <- read.delim(header = F, comment.char = "", 
		as.is = T, sep = ",", text = xpathApply(xmlParse(input), 
		  "//swe:values", xmlValue)[[1]])
	nms <- c("date", "discharge")
	names(flow) <- nms
	flow$date <- as.POSIXct(strptime(flow$date, format = "%Y-%m-%dT%H:%M:%SZ"))
	flow$discharge <- as.numeric(flow$discharge)
	flow <- as.data.frame(flow)
	attr(flow, "SRC") <- input
	class(flow) <- c("flow", "data.frame")
	cat("Finished!\n")
	return(flow)
}

medflowbyyear <- function(qfiletempf) {
	aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
		median)
}

meanflowbyyear <- function(qfiletempf) {
	aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
		mean)
}

bfi <- function(qfiletempf) {
	day7mean <- rollmean(qfiletempf$discharge, 7, align = "right", 
		na.pad = TRUE)
	rollingavg <- data.frame(qfiletempf, day7mean)
	rollingavgs7day <- subset(rollingavg, rollingavg$day7mean != 
		"NA")
	min7daybyyear <- aggregate(rollingavgs7day$day7mean, 
		list(rollingavgs7day$year_val), min)
	meanflow <- meanflowbyyear(qfiletempf)
	compbfi <- merge(min7daybyyear, meanflow, by = "Group.1")
	colnames(compbfi) <- c("year", "day7min", "meandaily")
	bfi <- compbfi$day7min/compbfi$meandaily
}

findrank <- function(n, p) {
	r <- (1 - p) * (n + 1)
	findrank <- floor(r)
}

ma1 <- function(x) {
	ma1 <- mean(x$discharge)
}

ma2 <- function(x) {
	ma2 <- median(x$discharge)
}

ma3 <- function(qfiletempf, pref = "mean") {
	sdbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
		FUN = sd)
	colnames(sdbyyr) <- c("Year", "sdq")
	meanbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
		mean)
	colnames(meanbyyr) <- c("Year", "meanq")
	dfcvbyyr <- data.frame(meanbyyr$Year, sdbyyr$sdq, 
		meanbyyr$meanq)
	colnames(dfcvbyyr) <- c("Year", "sdq", "meanq")
	cvbyyr <- dfcvbyyr$sdq/dfcvbyyr$meanq
	dfcvbyyrf <- data.frame(dfcvbyyr, cvbyyr)
	colnames(dfcvbyyrf) <- c("Year", "sdq", "meanq", 
		"cvq")
	if (pref == "median") {
		medcv <- median(dfcvbyyrf$cvq)
		ma3 <- (medcv) * 100
	}
	else {
		meancv <- mean(dfcvbyyrf$cvq)
		ma3 <- (meancv) * 100
	}
}

ma5 <- function(x) {
	x1 <- ma1(x)
	x2 <- ma2(x)
	ma5 <- x1/x2
}

ma12.23 <- function(qfiletempf, pref = "mean") {
	if (pref == "median") {
		medmon <- aggregate(qfiletempf$discharge, list(qfiletempf$month_val), 
		  median)
		ma12.23 <- data.frame(medmon)
	}
	else {
		meanmon <- aggregate(qfiletempf$discharge, list(qfiletempf$month_val), 
		  mean)
		ma12.23 <- data.frame(meanmon)
	}
}

ma5 <- function(x) {
	x1 <- ma1(x)
	x2 <- ma2(x)
	ma5 <- x1/x2
}

ma24.35 <- function(qfiletempf, pref = "mean") {
	sdmonbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val, 
		qfiletempf$month_val), FUN = sd)
	colnames(sdmonbyyr) <- c("Year", "Month", "sdq")
	meanmonbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val, 
		qfiletempf$month_val), FUN = mean)
	colnames(meanmonbyyr) <- c("Year", "Month", "meanq")
	dfcvmonbyyr <- data.frame(meanmonbyyr$Year, meanmonbyyr$Month, 
		sdmonbyyr$sdq, meanmonbyyr$meanq)
	colnames(dfcvmonbyyr) <- c("Year", "Month", "sdq", 
		"meanq")
	cvmonbyyr <- dfcvmonbyyr$sdq/dfcvmonbyyr$meanq
	dfcvmonbyyrf <- data.frame(dfcvmonbyyr, cvmonbyyr)
	colnames(dfcvmonbyyrf) <- c("Year", "Month", "sdq", 
		"meanq", "cvq")
	if (pref == "median") {
		medmoncv <- aggregate(dfcvmonbyyrf$cvq, list(dfcvmonbyyrf$Month), 
		  median)
		ma24.35 <- data.frame(medmoncv[2] * 100)
	}
	else {
		meanmoncv <- aggregate(dfcvmonbyyrf$cvq, list(dfcvmonbyyrf$Month), 
		  mean)
		ma24.35 <- data.frame(meanmoncv[2] * 100)
	}
}

ma37 <- function(qfiletempf) {
	sdmonbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val, 
		qfiletempf$month_val), FUN = mean)
	colnames(sdmonbyyr) <- c("Year", "Month", "meanmo")
	quart <- summary(sdmonbyyr$meanmo)
	thirdquart <- quart[5]
	firstquart <- quart[2]
	diffquart <- thirdquart - firstquart
	medianquart <- quart[3]
	ma37 <- diffquart[1]/medianquart[1]
}

ma39 <- function(qfiletempf) {
	sdmonbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val, 
		qfiletempf$month_val), FUN = mean)
	colnames(sdmonbyyr) <- c("Year", "Month", "meanmo")
	sdmonflows <- sd(sdmonbyyr$meanmo)
	meanmonflows <- mean(sdmonbyyr$meanmo)
	ma39 <- (sdmonflows * 100)/meanmonflows
}

ma40 <- function(qfiletempf) {
	numer1 <- colMeans(ma12.23(qfiletempf)[2])
	numer2 <- apply(ma12.23(qfiletempf, pref = "median")[2], 
		2, median)
	ma40 <- (numer1 - numer2)/numer2
}

ml13 <- function(qfiletempf) {
	minmonbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val, 
		qfiletempf$month_val), FUN = min)
	colnames(minmonbyyr) <- c("Year", "Month", "minmo")
	sdminmonflows <- sd(minmonbyyr$minmo)
	meanminmonflows <- mean(minmonbyyr$minmo)
	ml13 <- (sdminmonflows * 100)/meanminmonflows
}

ml14 <- function(qfiletempf) {
	min1daybyyear <- aggregate(qfiletempf$discharge, 
		list(qfiletempf$year_val), min)
	medflow <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
		median)
	computeml14 <- merge(min1daybyyear, medflow, by = "Group.1")
	colnames(computeml14) <- c("year", "day1min", "meddaily")
	dfml14 <- computeml14$day1min/computeml14$meddaily
	ml14 <- mean(dfml14)
}

ml17 <- function(qfiletempf, pref = "mean") {
	bfibyyear <- bfi(qfiletempf)
	if (pref == "median") {
		ml17 <- median(bfibyyear)
	}
	else {
		ml17 <- mean(bfibyyear)
	}
}

ml18 <- function(qfiletempf) {
	bfibyyear <- bfi(qfiletempf)
	sdbfi <- sd(bfibyyear)
	meanbfi <- mean(bfibyyear)
	ml18 <- meanbfi/sdbfi
}

mh14 <- function(qfiletempf) {
	maxmonbymoyr <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val, 
		qfiletempf$month_val), FUN = max)
	colnames(maxmonbymoyr) <- c("Year", "Month", "momax")
	maxmonbyyrr <- aggregate(maxmonbymoyr$momax, list(maxmonbymoyr$Year), 
		FUN = max)
	colnames(maxmonbyyrr) <- c("Year", "yrmax")
	medflowbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
		FUN = median)
	colnames(medflowbyyr) <- c("Year", "yrmed")
	ratiomaxmed <- maxmonbyyrr$yrmax/medflowbyyr$yrmed
	mh14 <- median(ratiomaxmed)
}

mh16 <- function(qfiletempf) {
	isolateq <- qfiletempf$discharge
	sortq <- sort(isolateq)
	frank <- floor(findrank(length(sortq), 0.1))
	hfcrit <- sortq[frank]
	mh16 <- hfcrit/ma2(x)
}

mh26 <- function(qfiletempf) {
	hfcrit <- 7 * ma2(qfiletempf)
	isolateq <- qfiletempf$discharge
	exchfcrit <- subset(isolateq, isolateq > hfcrit)
	meanex <- mean(exchfcrit)
	mh26 <- meanex/ma2(x)
}

fl1 <- function(qfiletempf, pref = "mean") {
	isolateq <- qfiletempf$discharge
	sortq <- sort(isolateq)
	frank <- floor(findrank(length(sortq), 0.75))
	lfcrit <- sortq[frank]
	noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
		FUN = median)
	colnames(noyears) <- c("Year", "momax")
	noyrs <- length(noyears$Year)
	lfcountbyyr <- rep(0, noyrs)
	counter <- 0
	for (i in as.character(noyears$Year[1]):as.character(noyears$Year[noyrs])) {
		subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$year_val) == 
		  i)
		echfcrit <- subset(subsetyr, subsetyr$discharge < 
		  lfcrit)
		counter <- counter + 1
		lfcountbyyr[counter] <- length(echfcrit$discharge)
	}
	lfcntbyyr <<- lfcountbyyr
	if (pref == "median") {
		fl1 <- median(lfcntbyyr)
	}
	else {
		fl1 <- mean(lfcntbyyr)
	}
}

fl2 <- function(qfiletempf) {
	meanfl1 <- fl1(qfiletempf, pref = "mean")
	stdevfl1 <- sd(lfcntbyyr)
	fl2 <- (stdevfl1 * 100)/meanfl1
}

fh1 <- function(qfiletempf, pref = "mean") {
	isolateq <- qfiletempf$discharge
	sortq <- sort(isolateq)
	frank <- floor(findrank(length(sortq), 0.25))
	hfcrit <- sortq[frank]
	noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
		FUN = median)
	colnames(noyears) <- c("Year", "momax")
	noyrs <- length(noyears$Year)
	hfcountbyyr <- rep(0, noyrs)
	counter <- 0
	for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])) {
		subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$year_val) == 
		  i)
		echfcrit <- subset(subsetyr, subsetyr$discharge > 
		  hfcrit)
		counter <- counter + 1
		hfcountbyyr[counter] <- length(echfcrit$discharge)
	}
	hfcntbyyr <<- hfcountbyyr
	if (pref == "median") {
		fh1 <- median(hfcntbyyr)
	}
	else {
		fh1 <- mean(hfcntbyyr)
	}
}

fh2 <- function(qfiletempf) {
	meanfh1 <- fh1(qfiletempf, pref = "mean")
	stdevfl1 <- sd(hfcntbyyr)
	fh2 <- (stdevfl1 * 100)/meanfh1
}

fh3 <- function(qfiletempf, pref = "mean") {
	hfcrit <- 3 * ma2(qfiletempf)
	noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
		FUN = median)
	colnames(noyears) <- c("Year", "momax")
	noyrs <- length(noyears$Year)
	hfcountbyyrfh3 <- rep(0, noyrs)
	counter <- 0
	for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])) {
		subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$year_val) == 
		  i)
		echfcrit <- subset(subsetyr, subsetyr$discharge > 
		  hfcrit)
		counter <- counter + 1
		hfcountbyyrfh3[counter] <- length(echfcrit$discharge)
	}
	hfcntbyyrfh3 <<- hfcountbyyrfh3
	if (pref == "median") {
		fh3 <- median(hfcntbyyrfh3)
	}
	else {
		fh3 <- mean(hfcntbyyrfh3)
	}
}

fh4 <- function(qfiletempf, pref = "mean") {
	hfcrit <- 7 * ma2(qfiletempf)
	noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
		FUN = median)
	colnames(noyears) <- c("Year", "momax")
	noyrs <- length(noyears$Year)
	hfcountbyyrfh4 <- rep(0, noyrs)
	counter <- 0
	for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])) {
		subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$year_val) == 
		  i)
		echfcrit <- subset(subsetyr, subsetyr$discharge > 
		  hfcrit)
		counter <- counter + 1
		hfcountbyyrfh4[counter] <- length(echfcrit$discharge)
	}
	hfcntbyyrfh4 <<- hfcountbyyrfh4
	if (pref == "median") {
		fh4 <- median(hfcntbyyrfh4)
	}
	else {
		fh4 <- mean(hfcntbyyrfh4)
	}
}

dl1 <- function(qfiletempf, pref = "mean") {
	day1mean <- rollmean(qfiletempf$discharge, 1, align = "right", 
		na.pad = TRUE)
	day1rollingavg <- data.frame(qfiletempf, day1mean)
	rollingavgs1day <- subset(day1rollingavg, day1rollingavg$day1mean != 
		"NA")
	min1daybyyear <<- aggregate(rollingavgs1day$day1mean, 
		list(rollingavgs1day$year_val), min)
	if (pref == "median") {
		dl1 <- median(min1daybyyear$x)
	}
	else {
		dl1 <- mean(min1daybyyear$x)
	}
}

dl2 <- function(qfiletempf, pref = "mean") {
	day3mean <- rollmean(qfiletempf$discharge, 3, align = "right", 
		na.pad = TRUE)
	day3rollingavg <- data.frame(qfiletempf, day3mean)
	rollingavgs3day <- subset(day3rollingavg, day3rollingavg$day3mean != 
		"NA")
	min3daybyyear <<- aggregate(rollingavgs3day$day3mean, 
		list(rollingavgs3day$year_val), min)
	if (pref == "median") {
		dl2 <- median(min3daybyyear$x)
	}
	else {
		dl2 <- mean(min3daybyyear$x)
	}
}

dl4 <- function(qfiletempf, pref = "mean") {
	day30mean <- rollmean(qfiletempf$discharge, 30, align = "right", 
		na.pad = TRUE)
	day30rollingavg <- data.frame(qfiletempf, day30mean)
	rollingavgs30day <- subset(day30rollingavg, day30rollingavg$day30mean != 
		"NA")
	min30daybyyear <<- aggregate(rollingavgs30day$day30mean, 
		list(rollingavgs30day$year_val), min)
	if (pref == "median") {
		dl5 <- median(min30daybyyear$x)
	}
	else {
		dl5 <- mean(min30daybyyear$x)
	}
}

dl5 <- function(qfiletempf, pref = "mean") {
	day90mean <- rollmean(qfiletempf$discharge, 90, align = "right", 
		na.pad = TRUE)
	day90rollingavg <- data.frame(qfiletempf, day90mean)
	rollingavgs90day <- subset(day90rollingavg, day90rollingavg$day90mean != 
		"NA")
	min90daybyyear <<- aggregate(rollingavgs90day$day90mean, 
		list(rollingavgs90day$year_val), min)
	if (pref == "median") {
		dl5 <- median(min90daybyyear$x)
	}
	else {
		dl5 <- mean(min90daybyyear$x)
	}
}

dl6 <- function(qfiletempf) {
	meandl6 <- dl1(qfiletempf, pref = "mean")
	sddl6 <- sd(min1daybyyear$x)
	dl6 <- (sddl6 * 100)/meandl6
}

dl9 <- function(qfiletempf) {
	meandl9 <- dl4(qfiletempf, pref = "mean")
	sddl9 <- sd(min30daybyyear$x)
	dl9 <- (sddl9 * 100)/meandl9
}

dl10 <- function(qfiletempf) {
	meandl10 <- dl5(qfiletempf, pref = "mean")
	sddl10 <- sd(min90daybyyear$x)
	dl10 <- (sddl10 * 100)/meandl10
}

dl18 <- function(qfiletempf, pref = "mean") {
	noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
		FUN = median)
	colnames(noyears) <- c("Year", "momax")
	noyrs <- length(noyears$Year)
	hfcountzeros <- rep(0, noyrs)
	counter <- 0
	for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])) {
		subsetyr <- subset(qfiletempf, qfiletempf$year_val == 
		  i)
		echfcrit <- subset(subsetyr, subsetyr$discharge < 
		  0.001)
		counter <- counter + 1
		hfcountzeros[counter] <- length(echfcrit$discharge)
	}
	hfcntzeros <<- hfcountzeros
	if (pref == "median") {
		dl18 <- median(hfcntzeros)
	}
	else {
		dl18 <- mean(hfcntzeros)
	}
}

dh5 <- function(qfiletempf, pref = "mean") {
	day90mean <- rollmean(qfiletempf$discharge, 90, align = "right", 
		na.pad = TRUE)
	day90rollingavg <- data.frame(qfiletempf, day90mean)
	rollingavgs90day <- subset(day90rollingavg, day90rollingavg$day90mean != 
		"NA")
	max90daybyyear <<- aggregate(rollingavgs90day$day90mean, 
		list(rollingavgs90day$year_val), max)
	if (pref == "median") {
		dh5 <- median(max90daybyyear$x)
	}
	else {
		dh5 <- mean(max90daybyyear$x)
	}
}

dh10 <- function(qfiletempf) {
	meandh10 <- dh5(qfiletempf, pref = "mean")
	sddh10 <- sd(max90daybyyear$x)
	dh10 <- (sddh10 * 100)/meandh10
}

tl1 <- function(qfiletempf, pref = "mean") {
	min1daybyyear <- aggregate(qfiletempf$discharge, 
		list(qfiletempf$year_val), min)
	noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
		FUN = median)
	colnames(noyears) <- c("Year", "momin")
	noyrs <- length(noyears$Year)
	juldaymin <- rep(0, noyrs)
	counter <- 0
	for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])) {
		counter <- counter + 1
		subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$year_val) == 
		  i)
		findjulday <- subset(subsetyr, subsetyr$discharge == 
		  min1daybyyear$x[counter])
		juldaymin[counter] <- findjulday$jul_val[1]
	}
	minjulday <<- juldaymin
	if (pref == "median") {
		tl1 <- median(minjulday)
	}
	else {
		tl1 <- mean(minjulday)
	}
}

tl2 <- function(qfiletempf) {
	meantl2 <- tl1(qfiletempf, pref = "mean")
	sddtl2 <- sd(minjulday)
	tl2 <- (sddtl2 * 100)/meantl2
}

th1 <- function(qfiletempf, pref = "mean") {
	max1daybyyear <- aggregate(qfiletempf$discharge, 
		list(qfiletempf$year_val), max)
	noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
		FUN = median)
	colnames(noyears) <- c("Year", "momax")
	noyrs <- length(noyears$Year)
	juldaymax <- rep(0, noyrs)
	counter <- 0
	for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])) {
		counter <- counter + 1
		subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$year_val) == 
		  i)
		findjulday <- subset(subsetyr, subsetyr$discharge == 
		  max1daybyyear$x[counter])
		juldaymax[counter] <- findjulday$jul_val[1]
	}
	maxjulday <<- juldaymax
	if (pref == "median") {
		th1 <- median(maxjulday)
	}
	else {
		th1 <- mean(maxjulday)
	}
}

th2 <- function(qfiletempf) {
	meanth2 <- th1(qfiletempf, pref = "mean")
	sddth2 <- sd(maxjulday)
	th2 <- (sddth2 * 100)/meanth2
}

ra1 <- function(qfiletempf, pref = "mean") {
	diffbtdays <- diff(qfiletempf$discharge, lag = 1, 
		differences = 1)
	findrisevalues <<- subset(diffbtdays, diffbtdays > 
		0)
	if (pref == "median") {
		ra1 <- median(findrisevalues)
	}
	else {
		ra1 <- mean(findrisevalues)
	}
}

ra3 <- function(qfiletempf, pref = "mean") {
	diffbtdays <- diff(qfiletempf$discharge, lag = 1, 
		differences = 1)
	findfallvalueneg <- subset(diffbtdays, diffbtdays < 
		0)
	findfallvalues <<- abs(findfallvalueneg)
	if (pref == "median") {
		ra3 <- median(findfallvalues)
	}
	else {
		ra3 <- mean(findfallvalues)
	}
}

ra4 <- function(qfiletempf) {
	meanra4 <- ra3(qfiletempf, pref = "mean")
	sddra4 <- sd(findfallvalues)
	ra4 <- (sddra4 * 100)/meanra4
}
a<-read.csv(header=F,colClasses=c("character"),text=sites)
a2<-length(a)
yv<-vector(length=a2)
ma1v<-vector(length=a2)
ma2v<-vector(length=a2)
ma3v<-vector(length=a2)
ma5v<-vector(length=a2)
ma12v<-vector(length=a2)
ma13v<-vector(length=a2)
ma14v<-vector(length=a2)
ma15v<-vector(length=a2)
ma16v<-vector(length=a2)
ma17v<-vector(length=a2)
ma18v<-vector(length=a2)
ma19v<-vector(length=a2)
ma20v<-vector(length=a2)
ma21v<-vector(length=a2)
ma22v<-vector(length=a2)
ma23v<-vector(length=a2)
ma24v<-vector(length=a2)
ma25v<-vector(length=a2)
ma26v<-vector(length=a2)
ma27v<-vector(length=a2)
ma28v<-vector(length=a2)
ma29v<-vector(length=a2)
ma30v<-vector(length=a2)
ma31v<-vector(length=a2)
ma32v<-vector(length=a2)
ma33v<-vector(length=a2)
ma34v<-vector(length=a2)
ma35v<-vector(length=a2)
ma37v<-vector(length=a2)
ma39v<-vector(length=a2)
ma40v<-vector(length=a2)
ml13v<-vector(length=a2)
ml14v<-vector(length=a2)
mh14v<-vector(length=a2)
mh16v<-vector(length=a2)
mh26v<-vector(length=a2)
ml17v<-vector(length=a2)
ml18v<-vector(length=a2)
fl1v<-vector(length=a2)
fl2v<-vector(length=a2)
fh1v<-vector(length=a2)
fh2v<-vector(length=a2)
fh3v<-vector(length=a2)
fh4v<-vector(length=a2)
dl1v<-vector(length=a2)
dl2v<-vector(length=a2)
dl4v<-vector(length=a2)
dl5v<-vector(length=a2)
dl6v<-vector(length=a2)
dl9v<-vector(length=a2)
dl10v<-vector(length=a2)
dl18v<-vector(length=a2)
dh5v<-vector(length=a2)
dh10v<-vector(length=a2)
tl1v<-vector(length=a2)
tl2v<-vector(length=a2)
th1v<-vector(length=a2)
th2v<-vector(length=a2)
ra1v<-vector(length=a2)
ra3v<-vector(length=a2)
ra4v<-vector(length=a2)

for (i in 1:length(a)){
  site<-a[i]
  url<-paste(sos_url,'?request=GetObservation&service=SOS&version=1.0.0&offering=',site,'&observedProperty=',property,sep='',collapse=NULL)
  x<-SWE_CSV_IHA(url)
  colnames(x)<-c("date","discharge")
  x2<-(x$date)
  x<-data.frame(strptime(x2, "%Y-%m-%d"),x$discharge)
  colnames(x)<-c("date","discharge")
  yv[i]<-as.character(min(x$date))
  selqfile<-x
  tempdatafr<-NULL
  tempdatafr<-data.frame(selqfile)
  month_val<-rep(0,length(tempdatafr$date))
  year_val<-rep(0,length(tempdatafr$date))
  day_val<-rep(0,length(tempdatafr$date))
  jul_val<-rep(0,length(tempdatafr$date))
  qfiletempf<-data.frame(tempdatafr$date,tempdatafr$discharge,month_val,year_val,day_val,jul_val)
  colnames(qfiletempf)<-c('date','discharge','month_val','year_val','day_val','jul_val')
  qfiletempf$month_val<-substr(x$date,6,7)
  as.numeric(qfiletempf$month_val)
  qfiletempf$year_val<-substr(x$date,3,4)
  as.numeric(qfiletempf$year_val)
  qfiletempf$day_val<-substr(x$date,9,10)
  as.numeric(qfiletempf$day_val)
  qfiletempf$jul_val<-strptime(x$date, "%Y-%m-%d")$yday+1
  as.numeric(qfiletempf$jul_val)
  flowdata<-data.frame(qfiletempf$date,qfiletempf$discharge,qfiletempf$month_val,qfiletempf$year_val,qfiletempf$day_val,qfiletempf$jul_val)
  colnames(flowdata)<-c('date','discharge','month_val','year_val','day_val','jul_val')
	ma1v[i]<-ma1(x)
	ma2v[i]<-ma2(x)
	ma3v[i]<-ma3(qfiletempf)
	ma5v[i]<-ma5(x)
  ma12v[i]<-ma12.23(qfiletempf)[1:1,2:2]
  ma13v[i]<-ma12.23(qfiletempf)[2:2,2:2]
  ma14v[i]<-ma12.23(qfiletempf)[3:3,2:2]
  ma15v[i]<-ma12.23(qfiletempf)[4:4,2:2]
  ma16v[i]<-ma12.23(qfiletempf)[5:5,2:2]
  ma17v[i]<-ma12.23(qfiletempf)[6:6,2:2]
  ma18v[i]<-ma12.23(qfiletempf)[7:7,2:2]
  ma19v[i]<-ma12.23(qfiletempf)[8:8,2:2]
  ma20v[i]<-ma12.23(qfiletempf)[9:9,2:2]
  ma21v[i]<-ma12.23(qfiletempf)[10:10,2:2]
  ma22v[i]<-ma12.23(qfiletempf)[11:11,2:2]
  ma23v[i]<-ma12.23(qfiletempf)[12:12,2:2]
  ma24v[i]<-ma24.35(qfiletempf)[1,1]
  ma25v[i]<-ma24.35(qfiletempf)[2,1]
  ma26v[i]<-ma24.35(qfiletempf)[3,1]
  ma27v[i]<-ma24.35(qfiletempf)[4,1]
  ma28v[i]<-ma24.35(qfiletempf)[5,1]
  ma29v[i]<-ma24.35(qfiletempf)[6,1]
  ma30v[i]<-ma24.35(qfiletempf)[7,1]
  ma31v[i]<-ma24.35(qfiletempf)[8,1]
  ma32v[i]<-ma24.35(qfiletempf)[9,1]
  ma33v[i]<-ma24.35(qfiletempf)[10,1]
  ma34v[i]<-ma24.35(qfiletempf)[11,1]
  ma35v[i]<-ma24.35(qfiletempf)[12,1]
	ma37v[i]<-unname(ma37(qfiletempf))
	ma39v[i]<-ma39(qfiletempf)
	ma40v[i]<-unname(ma40(qfiletempf))
  ml13v[i]<-ml13(qfiletempf)
  ml14v[i]<-ml14(qfiletempf)
  ml17v[i]<-ml14(qfiletempf)
  ml18v[i]<-ml18(qfiletempf)
  mh14v[i]<-mh14(qfiletempf)
  mh16v[i]<-mh16(qfiletempf)
  mh26v[i]<-mh26(qfiletempf)
  fl1v[i]<-fl1(qfiletempf)
  fl2v[i]<-fl2(qfiletempf)
  fh1v[i]<-fh1(qfiletempf)
  fh2v[i]<-fh2(qfiletempf)
  fh3v[i]<-fh3(qfiletempf)
  fh4v[i]<-fh4(qfiletempf)
  dl1v[i]<-dl1(qfiletempf)
  dl2v[i]<-dl2(qfiletempf)
  dl4v[i]<-dl4(qfiletempf)
  dl5v[i]<-dl5(qfiletempf)
  dl6v[i]<-dl6(qfiletempf)
  dl9v[i]<-dl9(qfiletempf)
  dl10v[i]<-dl10(qfiletempf)
  dl18v[i]<-dl18(qfiletempf)
  dh5v[i]<-dh5(qfiletempf)
  dh10v[i]<-dh10(qfiletempf)
  tl1v[i]<-tl1(qfiletempf)
  tl2v[i]<-tl2(qfiletempf)
  th1v[i]<-th1(qfiletempf)
  th2v[i]<-th2(qfiletempf)
  ra1v[i]<-ra1(qfiletempf)
  ra3v[i]<-ra3(qfiletempf)
  ra4v[i]<-ra4(qfiletempf)
}
statsout<-data.frame(t(a),yv,ma1v,ma2v,ma3v,ma5v,ma12v,ma13v,ma14v,ma15v,ma16v,ma17v,ma18v,ma19v,ma20v,ma21v,ma22v,ma23v,
ma24v,ma25v,ma26v,ma27v,ma28v,ma29v,ma30v,ma31v,ma32v,ma33v,ma34v,ma35v,ma37v,ma39v,ma40v,ml13v,ml14v,mh14v,mh16v,
mh26v,ml17v,ml18v,fl1v,fl2v,fh1v,fh2v,fh3v,fh4v,dl1v,dl2v,dl4v,dl5v,dl6v,dl9v,dl10v,dl18v,dh5v,dh10v,tl1v,tl2v,th1v,th2v,ra1v,ra3v,ra4v)
colnames(statsout)<-c('site_no','min_date','ma1_mean_disc','ma2_median_disc','ma3_mean_annual_var','ma5_skew','ma12_jan_mean','ma13_feb_mean','ma14_mar_mean','ma15_apr_mean',
'ma16_may_mean','ma17_june_mean','ma18_july_mean','ma19_aug_mean','ma20_sep_mean','ma21_oct_mean','ma22_nov_mean','ma23_dec_mean','ma24_jan_var','ma25_feb_var',
'ma26_mar_var','ma27_apr_var','ma28_may_var','ma29_jun_var','ma30_july_var','ma31_aug_var','ma32_sep_var','ma33_oct_var','ma34_nov_var','ma35_dec_var',
'ma37_var_across_months','ma39_monthly_std_dev','ma40_monthly_skewness','ml13_min_monthly_var','ml14_min_annual_flow','mh14_med_annual_max',
'mh16_high_flow_index','mh26_high_peak_flow','ml17_base_flow','ml18_base_flow_var','fl1_low_flood_pulse','fl2_low_pulse_var',
'fh1_high_pulse_count','fh2_high_pulse_var','fh3_high_pulse_count_three','fh4_high_pulse_count_seven','dl1_min_daily_flow',
'dl2_min_3_day_avg','dl4_min_30_day_avg','dl5_min_90_day_avg','dl6_min_flow_var','dl9_min_30_day_var','dl10_min_90_day_var',
'dl18_zero_flow_days','dh5_max_90_day_avg','dh10_max_90_day_var','tl1_min_flow_julian_day','tl2_min_julian_var','th1_max_flow_julian_day',
'th2_max_julian_var','ra1_rise_rate','ra3_fall_rate','ra4_fall_rate_var')
output="output.txt"
write.table(statsout,file="output.txt",col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")

# wps.out: output, text, output_file, A file containing the mean daily flow median daily flow and skewness of daily flow;