---
title: "Reproducible Research PGA"
author: "Rajeet"
date: "5/3/2021"
output: html_document
---
The goal of this assignment is to explore the data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

1. Loading and preprocessing the data
--------------------------------------------------

Unzip data to obtain a csv file.

``` {r}
library("data.table")
library(ggplot2)

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'), method = "curl")
unzip("repdata%2Fdata%2Factivity.zip",exdir = "data")
```

Reading csv data into a data table
---------------------------------

``` {r}
activityDT <- data.table::fread(input = "data/activity.csv")
```

What is mean total number of steps taken per day?
-------------------------------------------------

Calculating the total number of steps taken per day

``` {r}
Total_Steps <- activityDT[, c(lapply(.SD, sum, na.rm = FALSE)), .SDcols = c("steps"), by = .(date)] 

head(Total_Steps, 10)
```

##           date steps
##  1: 2012-10-01    NA
##  2: 2012-10-02   126
##  3: 2012-10-03 11352
##  4: 2012-10-04 12116
##  5: 2012-10-05 13294
##  6: 2012-10-06 15420
##  7: 2012-10-07 11015
##  8: 2012-10-08    NA
##  9: 2012-10-09 12811
## 10: 2012-10-10  9900

2. Histogram with the frequency of total number of steps each day
--------------------------------------------------------------------
``` {r}
p <- ggplot(Total_Steps, aes(x = steps)) +
    geom_histogram(fill = "blue", binwidth = 1000) +
    labs(title = "Daily Steps", x = "Steps", y = "Frequency")
    
  print (p)
```
##https://github.com/RKG99/RepData_PeerAssessment1/blob/master/Plot1.png

3. Calculating mean and median of the total number of steps taken per day
----------------------------------------------------------------------------

``` {r}
Total_Steps[, .(Mean_Steps = mean(steps, na.rm = TRUE), Median_Steps = median(steps, na.rm = TRUE))]
```

##    Mean_Steps Median_Steps
## 1:   10766.19        10765


4. What is the average daily activity pattern?
------------------------------------------------

Making a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

##https://github.com/RKG99/RepData_PeerAssessment1/blob/master/plot2.png

``` {r}
IntervalDT <- activityDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)] 

ggplot(IntervalDT, aes(x = interval , y = steps)) + geom_line(color="blue", size=1) + labs(title = "Avg. Daily Steps", x = "Interval", y = "Avg. Steps per day")
```


5. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
-------------------------------------------------------------------

``` {r}
IntervalDT[steps == max(steps), .(max_interval = interval)]
```

##    max_interval
## 1:          835


6.Imputing missing values
---------------------------
Calculating the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

``` {r}
activityDT[is.na(steps), .N ]
```

## [1] 2304
   
Filling in missing values with median of dataset. 

``` {r}

activityDT[is.na(steps), "steps"] <- activityDT[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
```

Creating a new dataset with the missing data filled in the original dataset.

``` {r}
data.table::fwrite(x = activityDT, file = "data/tidyData.csv", quote = FALSE)
```

7.Histogram of the total number of steps taken each day after missing values are imputed
--------------------------------------------------------------------------------

``` {r}
# total number of steps taken per day
Total_Steps <- activityDT[, c(lapply(.SD, sum)), .SDcols = c("steps"), by = .(date)] 

# mean and median total number of steps taken per day
Total_Steps[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]
```

##    Mean_Steps Median_Steps
## 1:    9354.23        10395
    
Histogram with the new frequencies of total number of steps after imputing.

``` {r}
ggplot(Total_Steps, aes(x = steps)) + geom_histogram(fill = "blue", binwidth = 1000) + labs(title = "Daily Steps", x = "Steps", y = "Frequency")
```

##https://github.com/RKG99/RepData_PeerAssessment1/blob/master/plot3.png


Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

Creating a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

``` {r}
# Just recreating activityDT from scratch then making the new factor variable. (No need to, just want to be clear on what the entire process is.) 
activityDT <- data.table::fread(input = "data/activity.csv")
activityDT[, date := as.POSIXct(date, format = "%Y-%m-%d")]
activityDT[, `Day of Week`:= weekdays(x = date)]
activityDT[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `Day of Week`), "weekday or weekend"] <- "weekday"
activityDT[grepl(pattern = "Saturday|Sunday", x = `Day of Week`), "weekday or weekend"] <- "weekend"
activityDT[, `weekday or weekend` := as.factor(`weekday or weekend`)]
head(activityDT, 10)
```

##     steps       date interval Day of Week weekday or weekend
##  1:    NA 2012-10-01        0      Monday            weekday
##  2:    NA 2012-10-01        5      Monday            weekday
##  3:    NA 2012-10-01       10      Monday            weekday
##  4:    NA 2012-10-01       15      Monday            weekday
##  5:    NA 2012-10-01       20      Monday            weekday
##  6:    NA 2012-10-01       25      Monday            weekday
##  7:    NA 2012-10-01       30      Monday            weekday
##  8:    NA 2012-10-01       35      Monday            weekday
##  9:    NA 2012-10-01       40      Monday            weekday
## 10:    NA 2012-10-01       45      Monday            weekday

8. Plotting the number of steps for all 5-min intervals, averaged across weekdays and weekends separately.
-----------------------------------------------------------------

``` {r}
activityDT[is.na(steps), "steps"] <- activityDT[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
IntervalDT <- activityDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `weekday or weekend`)] 

ggplot(IntervalDT , aes(x = interval , y = steps, color=`weekday or weekend`)) + geom_line() + labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps") + facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2)
```

##https://github.com/RKG99/RepData_PeerAssessment1/blob/master/plot%204.png


