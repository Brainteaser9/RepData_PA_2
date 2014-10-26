---
title: "Major Storm Impacts and Consequences"
author: "Brainteaser9"
date: "Sunday, October 26, 2014"
output: html_document
---

### Synopsis

In this report we would like to identify weather event types that are most harmful with respect to population health and those that have the greatest economic consequences. To identify these weather events we obtained data from the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. The events in the database start in the year 1950 and end in November 2011. In the earlier years of the database there are generally fewer events recorded, most likely due to a lack of good records. More recent years should be considered more complete. From these data, we found that tornado is significantly the most danger weather event type from the aspect of population health and estimated property damage as well.

### Loading and Processing the Raw Data

I have obtained the data from the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database from the below link:

<https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2>

There is also some documentation of the database available. I have downloaded them from the following sources:

National Weather Service Storm Data Documentation:

<https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf>

National Climatic Data Center Storm Events FAQ:

<https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf>


#### Reading in the Data

The data come in the form of a comma-separated-value file compressed via the bzip2 algorithm to reduce its size. The events in the database start in the year 1950 and end in November 2011. In the earlier years of the database there are generally fewer events recorded, most likely due to a lack of good records. More recent years should be considered more complete.

We first read in the data from the CSV file included in the archive.


```r
setwd("T:/LEARNING/Data Science Specialization/5 - Reproducible Research/RepData_PA2")
library(data.table)
data <- read.csv("repdata_data_StormData.csv", header = TRUE)
```

After reading in the data we check the first few rows (there are 902,297) and the variable name (there are 37) in this dataset.

```r
dim(data)
```

```
## [1] 902297     37
```

```r
head(data)
```

```
##   STATE__           BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE
## 1       1  4/18/1950 0:00:00     0130       CST     97     MOBILE    AL
## 2       1  4/18/1950 0:00:00     0145       CST      3    BALDWIN    AL
## 3       1  2/20/1951 0:00:00     1600       CST     57    FAYETTE    AL
## 4       1   6/8/1951 0:00:00     0900       CST     89    MADISON    AL
## 5       1 11/15/1951 0:00:00     1500       CST     43    CULLMAN    AL
## 6       1 11/15/1951 0:00:00     2000       CST     77 LAUDERDALE    AL
##    EVTYPE BGN_RANGE BGN_AZI BGN_LOCATI END_DATE END_TIME COUNTY_END
## 1 TORNADO         0                                               0
## 2 TORNADO         0                                               0
## 3 TORNADO         0                                               0
## 4 TORNADO         0                                               0
## 5 TORNADO         0                                               0
## 6 TORNADO         0                                               0
##   COUNTYENDN END_RANGE END_AZI END_LOCATI LENGTH WIDTH F MAG FATALITIES
## 1         NA         0                      14.0   100 3   0          0
## 2         NA         0                       2.0   150 2   0          0
## 3         NA         0                       0.1   123 2   0          0
## 4         NA         0                       0.0   100 2   0          0
## 5         NA         0                       0.0   150 2   0          0
## 6         NA         0                       1.5   177 2   0          0
##   INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP WFO STATEOFFIC ZONENAMES
## 1       15    25.0          K       0                                    
## 2        0     2.5          K       0                                    
## 3        2    25.0          K       0                                    
## 4        2     2.5          K       0                                    
## 5        2     2.5          K       0                                    
## 6        6     2.5          K       0                                    
##   LATITUDE LONGITUDE LATITUDE_E LONGITUDE_ REMARKS REFNUM
## 1     3040      8812       3051       8806              1
## 2     3042      8755          0          0              2
## 3     3340      8742          0          0              3
## 4     3458      8626          0          0              4
## 5     3412      8642          0          0              5
## 6     3450      8748          0          0              6
```

The columns we are interested in are FATALITIES, INJURIES, PROPDMG, PROPDMGEXP. The first 2 variables indicates the number of injuries and fatalities per each weather event. The last 2 variables estimate the property damage as actual dollar amounts per each weather event.

### Results

#### First we identify those types of events (as indicated in the EVTYPE variable) that are most harmful with respect to population health.

Now we count the total number of injuries and fatalaties per each event type.

```r
inpertype <- aggregate(.~EVTYPE,data=data[,c(8,24)],sum)
mostin <- inpertype[order(-inpertype$INJURIES)[1:10],]
mean(inpertype$INJURIES)
```

```
## [1] 142.7
```

```r
fapertype <- aggregate(.~EVTYPE,data=data[,c(8,23)],sum)
mostfa <- fapertype[order(-fapertype$FATALITIES)[1:10],]
mean(fapertype$FATALITIES)
```

```
## [1] 15.38
```

As we can see in the below pictures, tornado is significantly the most danger weather event type ever.

```r
barplot(mostfa$FATALITIES, las=2, names.arg=mostfa$EVTYPE)
title(main="Total Number of fatalities per event types")
```

![plot of chunk plots](figure/plots1.png) 

```r
barplot(mostin$INJURIES, las=2, names.arg=mostin$EVTYPE)
title(main="Total Number of injuries per event types")
```

![plot of chunk plots](figure/plots2.png) 

#### Regarding the economic impacts we will analyzing the PROPDMG, PROPDMGEXP variables.

Some reshaping is needed due to different multipliers in column PROPDMGEXP. We use only those observations where this variavle is in ("K", "M", "B").


```r
ecodata <- subset(data[data$PROPDMGEXP %in% c("B", "M", "K"),c(8, 25, 26)])
ecodata[ecodata$PROPDMGEXP == "K", 4] <- ecodata[ecodata$PROPDMGEXP == "K",]$PROPDMG / 1000000
ecodata[ecodata$PROPDMGEXP == "M", 4] <- ecodata[ecodata$PROPDMGEXP == "M",]$PROPDMG / 1000
colnames(ecodata)[4] <- "DOLLARB"
ecodata <- ecodata[, c(1, 4)]
head(ecodata)
```

```
##    EVTYPE DOLLARB
## 1 TORNADO 2.5e-05
## 2 TORNADO 2.5e-06
## 3 TORNADO 2.5e-05
## 4 TORNADO 2.5e-06
## 5 TORNADO 2.5e-06
## 6 TORNADO 2.5e-06
```

After reshaping and subsetting our data in DOLLARB column we can see the expected property damages in billion dollars per each type of weather event. Now we count the sum of DOLLARB per weather event type and sort them in a descending order.


```r
dpertype <- aggregate(.~EVTYPE,data=ecodata[,c(1:2)],sum)
mostd <- dpertype[order(-dpertype$DOLLARB)[1:10],]
mostd
```

```
##                EVTYPE DOLLARB
## 329           TORNADO  51.626
## 62              FLOOD  22.158
## 50        FLASH FLOOD  15.141
## 103              HAIL  13.927
## 169         HURRICANE   6.168
## 342         TSTM WIND   4.485
## 154         HIGH WIND   3.970
## 184         ICE STORM   3.945
## 176 HURRICANE/TYPHOON   3.806
## 384          WILDFIRE   3.725
```

As we can see in the below figure, tornado is the most danger from the aspect of property damage as well.


```r
barplot(mostd$DOLLARB, las=2, names.arg=mostd$EVTYPE)
title(main="Estimated Total property damage of event types (billion dollars)")
```

![plot of chunk dollarplot](figure/dollarplot.png) 
