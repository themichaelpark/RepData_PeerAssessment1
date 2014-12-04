# Reproducible Research: Peer Assessment 1
Michael Park  


## Loading and preprocessing the data
First, we need to get the data from the course website. The code below will download the zip file to the working directory, unzip it, and load the unzipped data. 


```r
download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "activity.zip")
unzip("activity.zip")
activity <- read.csv("activity.csv", header=TRUE, stringsAsFactors = FALSE)
```

At this point, the only transformation necessary for the data is to convert the date values from character to date format.

```r
activity[,2]<-as.Date(activity[,2])
```
## What is mean total number of steps taken per day?


```r
library(plyr)
dailysum <- ddply(activity, "date", summarise, 
                  perday = sum(steps, na.rm = TRUE))
hist(dailysum[,2], xlab = "Steps per day", main = "")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
dailymean <- mean(dailysum[,2])
dailymed <- median(dailysum[,2])
dailymean
```

```
## [1] 9354.23
```

```r
dailymed
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
intmean <- ddply(activity, "interval", summarise,
                perint = mean(steps, na.rm = TRUE))  # computes the average number of teps for each 5 minute interval
intmax <- intmean[which.max(intmean[,2]),1]  #return the 5 minute interval which has the maximum average number of steps
```
## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
