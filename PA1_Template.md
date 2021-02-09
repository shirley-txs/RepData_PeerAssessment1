---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data

1. Code for reading in the dataset and/or processing the data


```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="activity.zip")
unzip("activity.zip")

activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)
```

## What is mean total number of steps taken per day?

2. Histogram of the total number of steps taken each day


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 4.0.3
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
steps_per_day <- activity %>% group_by(date) %>% summarize(totalsteps = sum(steps, na.rm = TRUE)) 
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
hist(steps_per_day$totalsteps, main = "Histogram of Daily Steps", xlab = "Steps")
```

![](PA1_Template_files/figure-html/2-1.png)<!-- -->

3. Mean and median number of steps taken each day


```r
mean <- mean(steps_per_day$totalsteps)
median <- median(steps_per_day$totalsteps)

print(paste("The mean is: ", mean))
```

```
## [1] "The mean is:  9354.22950819672"
```

```r
print(paste("The median is: ", median))
```

```
## [1] "The median is:  10395"
```

## What is the average daily activity pattern?

4. Time series plot of the average number of steps taken


```r
steps_per_interval <- activity %>% group_by(interval) %>% summarize(meansteps = mean(steps, na.rm = TRUE)) 
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
with(steps_per_interval, plot(interval,meansteps, type = "l", main = "Average Number of Steps by Interval", xlab = "Interval", ylab = "Mean Steps"))
```

![](PA1_Template_files/figure-html/4-1.png)<!-- -->

5. The 5-minute interval that, on average, contains the maximum number of steps


```r
maxinterval <- steps_per_interval$interval[which.max(steps_per_interval$meansteps)]
print(paste("The 5-minute interval that, on average, contains the maximum number of steps is ", maxinterval))
```

```
## [1] "The 5-minute interval that, on average, contains the maximum number of steps is  835"
```

## Imputing missing values

6.Code to describe and show a strategy for imputing missing data


```r
print(paste("The total number of missing values: ",sum(is.na(activity))))
```

```
## [1] "The total number of missing values:  2304"
```

7.Histogram of the total number of steps taken each day after missing values are imputed


```r
#replace NA values to average for the corresponding interval 
activity_adjusted <- activity  
for (i in 1:nrow(activity)){
        if(is.na(activity$steps[i])){
                activity_adjusted$steps[i]<- steps_per_interval$meansteps[activity_adjusted$interval[i] == steps_per_interval$interval]
        }
}

#sum total steps per day for new dataset 
steps_per_day_adjusted <- activity_adjusted %>% group_by(date) %>% summarize(totalsteps = sum(steps, na.rm = TRUE)) 
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
#plot histogram
hist(steps_per_day_adjusted$totalsteps, main = "Histogram of Daily Steps", xlab = "Steps")
```

![](PA1_Template_files/figure-html/7-1.png)<!-- -->

```r
#compute new mean and median
new_mean <- mean(steps_per_day_adjusted$totalsteps)
new_median <- median(steps_per_day_adjusted$totalsteps)

print(paste("The new mean is ", new_mean, ", as compared to previous mean of", mean))
```

```
## [1] "The new mean is  10766.1886792453 , as compared to previous mean of 9354.22950819672"
```

```r
print(paste("The new median is ", new_median, ", as compared to previous median of", median))
```

```
## [1] "The new median is  10766.1886792453 , as compared to previous median of 10395"
```
The mean and median has increased after imputing missing data on the estimates of the total daily number of steps. 

## Are there differences in activity patterns between weekdays and weekends?

8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
#add new column in dataset
activity_adjusted$day <- ifelse(weekdays(activity_adjusted$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

#creating new datasets

activity_wkday <- filter(activity_adjusted, activity_adjusted$day == "weekday")
activity_wkend <- filter(activity_adjusted, activity_adjusted$day == "weekend")


steps_per_interval_wkday <- activity_wkday %>% group_by(interval) %>% summarize(meansteps = mean(steps, na.rm = TRUE)) 
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
steps_per_interval_wkday$day <- "weekday" 

steps_per_interval_wkend <- activity_wkend %>% group_by(interval) %>% summarize(meansteps = mean(steps, na.rm = TRUE)) 
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
steps_per_interval_wkend$day <- "weekend"


#combine new datasets
steps_per_interval_new<- rbind(steps_per_interval_wkday, steps_per_interval_wkend)
steps_per_interval_new$day <- as.factor(steps_per_interval_new$day)

#plot graph 
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.0.3
```

```r
ggplot (steps_per_interval_new, aes (interval, meansteps)) + geom_line() + facet_grid (day~.) + labs(y = "Number of Steps") + labs(x = "Interval") + ggtitle("Average Number of Steps - Weekday vs. Weekend")
```

![](PA1_Template_files/figure-html/8-1.png)<!-- -->
