---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
# Initial options for knitr
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

# Checking if zip file was unzipped
fileZip = "activity.zip"

if (!file.exists("activity.csv")) { 
    unzip(fileZip) 
}

# load the dataset as activity
activity = read.csv("activity.csv")

# head(activity)
# str(activity)
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
library(dplyr)
library(ggplot2)

totalStepsDay <- activity[!is.na(activity$step),] %>% group_by(date) %>% summarise_all(sum)
# totalStepsDay <- aggregate(steps ~ date, activity, sum)

head(totalStepsDay)
```

```
## # A tibble: 6 × 3
##   date       steps interval
##   <chr>      <int>    <int>
## 1 2012-10-02   126   339120
## 2 2012-10-03 11352   339120
## 3 2012-10-04 12116   339120
## 4 2012-10-05 13294   339120
## 5 2012-10-06 15420   339120
## 6 2012-10-07 11015   339120
```

2. Make a histogram of the total number of steps taken each dayMake a histogram of the total number of steps taken each day


```r
g <- ggplot(totalStepsDay, aes(x = date, y = steps))

g + geom_histogram(stat = 'identity') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme_bw(base_family = "Avenir", base_size = 10) +
    labs(x = "Day", y = "Steps (total count per day)") +
    labs(title = "Total Number of Steps per Day") +
    theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
stepsMean <- mean(totalStepsDay$steps)
stepsMean <- format(stepsMean, scientific = FALSE)
stepsMedian <- median(totalStepsDay$steps)
```

The mean of total number of steps per day is **10766.19** steps, and the median is **10765** steps.


## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l"type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
meanStepsInterval <- activity[!is.na(activity$step),] %>% group_by(interval) %>% summarise_all(mean)
head(meanStepsInterval)
```

```
## # A tibble: 6 × 3
##   interval  steps  date
##      <int>  <dbl> <dbl>
## 1        0 1.72      NA
## 2        5 0.340     NA
## 3       10 0.132     NA
## 4       15 0.151     NA
## 5       20 0.0755    NA
## 6       25 2.09      NA
```




```r
g <- ggplot(meanStepsInterval, aes(x = interval, y = steps))

g + geom_line(color = "blue") +
    theme_bw(base_family = "Avenir", base_size = 10) +
    labs(x = "Interval (minutes)", y = "Mean Steps per Interval") +
    labs(title = "5-minutes Interval Average Numbers of Steps Taken") +
    theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/time series plot-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxInterval <- meanStepsInterval[grep(max(meanStepsInterval$steps), meanStepsInterval$steps), ]
```

The interval with the maximum number of steps is **835, 206.1698113, NA**.

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with **NAs**)


```r
data.frame(stepsNA = sum(is.na(activity$steps)),
           intervalNA = sum(is.na(activity$interval)),
           dataNA = sum(is.na(activity$data)))
```

```
##   stepsNA intervalNA dataNA
## 1    2304          0      0
```

2. Devise a strategy for filling in all of the missing values in the dataset.

The strategy used will be to assign at each interval od the dataset with missing values the mean of that interval.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
So:


```r
missingIndex<-is.na(activity[,1])
imputedSteps <- activity
meanStepsIntervalRep <- meanStepsInterval[rep(seq_len(nrow(meanStepsInterval)), nlevels(factor(imputedSteps$date))), ]
imputedSteps$steps[missingIndex] <- meanStepsIntervalRep$steps[missingIndex]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
totalStepsDayImputed<- imputedSteps %>% group_by(date) %>% summarise_all(sum)
# totalStepsDay <- aggregate(steps ~ date, activity, sum)

# head(totalStepsDayImputed)

g <- ggplot(totalStepsDayImputed, aes(x = date, y = steps))

g + geom_histogram(stat = 'identity') +
    theme_bw(base_size = 10) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x = "Day", y = "Steps (total count per day)") +
    labs(title = "Total Number of Steps per Day Imputed Data") +
    theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
stepsMeanImputed <-format(mean(totalStepsDayImputed$steps), scientific = FALSE)
stepsMedianImputed <- format(median(totalStepsDayImputed$steps), scientific = FALSE)
```

The mean of total number of steps per day is **10766.19** steps, and the median is **10766.19** steps.

It seems that the value for the mean hasn't changed, but the median has been displaced to the value of the mean. It could make sense since we added average values to impute the missing ones.

## Are there differences in activity patterns between weekdays and weekends?

1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
imputedSteps$date <- as.Date(activity$date)
imputedSteps <- imputedSteps %>% mutate(dayType = ifelse(weekdays(imputedSteps$date) %in% c("dissabte", "diumenge"), "weekend", "weekday"))
```

2.Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = “𝚕”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
meanStepsIntervalImputed <- imputedSteps %>% group_by(dayType, interval) %>% summarise_all(mean)

g <- ggplot(meanStepsIntervalImputed, aes(x = interval, y = steps, color = dayType))

g + geom_line() +
    facet_grid(.~dayType) +
    theme_bw(base_size = 10) +
    labs(x = "Interval (minutes)", y = "Mean Steps per Interval") +
    labs(title = "5-minutes Interval Average Numbers of Steps Taken") +
    theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
