---
title: "Major Storm Impacts and Consequences"
author: "Brainteaser9"
date: "Sunday, October 26, 2014"
output: pdf_document
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

```{r loaddata}
setwd("T:/LEARNING/Data Science Specialization/5 - Reproducible Research/RepData_PA2")
library(data.table)
data <- read.csv("repdata_data_StormData.csv", header = TRUE)
```

After reading in the data we check the first few rows (there are 902,297) and the variable name (there are 37) in this dataset.
```{r dim}
dim(data)
head(data)
```

The columns we are interested in are FATALITIES, INJURIES, PROPDMG, PROPDMGEXP. The first 2 variables indicates the number of injuries and fatalities per each weather event. The last 2 variables estimate the property damage as actual dollar amounts per each weather event.

### Results

#### First we identify those types of events (as indicated in the EVTYPE variable) that are most harmful with respect to population health.

Now we count the total number of injuries and fatalaties per each event type.
```{r totals}
inpertype <- aggregate(.~EVTYPE,data=data[,c(8,24)],sum)
mostin <- inpertype[order(-inpertype$INJURIES)[1:10],]
mean(inpertype$INJURIES)
fapertype <- aggregate(.~EVTYPE,data=data[,c(8,23)],sum)
mostfa <- fapertype[order(-fapertype$FATALITIES)[1:10],]
mean(fapertype$FATALITIES)
```

As we can see in the below pictures, tornado is significantly the most danger weather event type ever.
```{r plots}
barplot(mostfa$FATALITIES, las=2, names.arg=mostfa$EVTYPE)
title(main="Total Number of fatalities per event types")

barplot(mostin$INJURIES, las=2, names.arg=mostin$EVTYPE)
title(main="Total Number of injuries per event types")
```

#### Regarding the economic impacts we will analyzing the PROPDMG, PROPDMGEXP variables.

Some reshaping is needed due to different multipliers in column PROPDMGEXP. We use only those observations where this variavle is in ("K", "M", "B").

```{r ecodata}
ecodata <- subset(data[data$PROPDMGEXP %in% c("B", "M", "K"),c(8, 25, 26)])
ecodata[ecodata$PROPDMGEXP == "K", 4] <- ecodata[ecodata$PROPDMGEXP == "K",]$PROPDMG / 1000000
ecodata[ecodata$PROPDMGEXP == "M", 4] <- ecodata[ecodata$PROPDMGEXP == "M",]$PROPDMG / 1000
colnames(ecodata)[4] <- "DOLLARB"
ecodata <- ecodata[, c(1, 4)]
head(ecodata)
```

After reshaping and subsetting our data in DOLLARB column we can see the expected property damages in billion dollars per each type of weather event. Now we count the sum of DOLLARB per weather event type and sort them in a descending order.

```{r dollar}
dpertype <- aggregate(.~EVTYPE,data=ecodata[,c(1:2)],sum)
mostd <- dpertype[order(-dpertype$DOLLARB)[1:10],]
mostd
```

As we can see in the below figure, tornado is the most danger from the aspect of property damage as well.

```{r dollarplot}
barplot(mostd$DOLLARB, las=2, names.arg=mostd$EVTYPE)
title(main="Estimated Total property damage of event types (billion dollars)")
```