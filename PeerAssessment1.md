---
title: 'Reproducible Research: Peer Assessment 1'
author: "Jose Sierra"
date: "1/1/2020"
output: 
  html_document: 
    keep_md: yes
---


## Loading and preprocessing the data


```r
    unzip("repdata_data_activity.zip")
    
    data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day


```r
TotalSteps <- aggregate(steps~date, data, FUN=sum)
```

2. Make a histogram of the total number of steps taken each day


```r
hist(TotalSteps$steps)
```

![](PeerAssessment1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


3. Calculate and report the mean and median of the total number of steps taken per day

Mean number of steps per day:

```r
mean(TotalSteps$steps)
```

```
## [1] 10766.19
```
Median number of steps per day:

```r
median(TotalSteps$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
StepsInterval <- aggregate(steps ~ interval, data, FUN=mean)
plot(StepsInterval,col = "red", type="l",main="Average Steps per 5 Minute Intervals",
     xlab="5 Minute Intervals", ylab="Average Steps Taken")
```

![](PeerAssessment1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

  
  ```r
  StepsInterval$interval[which.max(StepsInterval$steps)]
  ```
  
  ```
  ## [1] 835
  ```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)


```r
sum(is.na(data))
```

```
## [1] 2304
```


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
FillByMean <- aggregate(steps~interval,data=data,mean,na.rm=TRUE)
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
StepsDataFilled <- merge(data, StepsInterval, by = "interval", suffixes = c("", 
    ".y"))
Nas <- is.na(StepsDataFilled$steps)
StepsDataFilled$steps[Nas] <- StepsDataFilled$steps.y[Nas]
StepsDataFilled <- StepsDataFilled[, c(1:3)]
```
4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
TotalStepsFilled <- aggregate(steps~date,data=StepsDataFilled,sum)

hist(TotalStepsFilled$steps, xlab = "Total Steps", main = "Total Number of Steps Taken", breaks = 15)
```

![](PeerAssessment1_files/figure-html/unnamed-chunk-11-1.png)<!-- -->



```r
   ##4b. Calculate and report the mean and median total number of steps taken per day.
mean(TotalStepsFilled$steps)
```

```
## [1] 10766.19
```

```r
median(TotalStepsFilled$steps)
```

```
## [1] 10766.19
```
The mean total number of steps taken per day is 1.0766189\times 10^{4} steps.  
The median total number of steps taken per day is 1.0766189\times 10^{4} steps.

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

No Change in the mean value but a slight uptick in the median value (+1.19)

## Are there differences in activity patterns between weekdays and weekends?
For this part the \color{red}{\verb|weekdays()|}weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

Separate the weekday and weekend data:

```r
StepsDataFilled$date <- as.Date(StepsDataFilled$date)
WeekendDays <- c("Saturday","Sunday")
StepsDataFilled$daytype <- as.factor(sapply(StepsDataFilled$date, function(x) ifelse(weekdays(x) %in% WeekendDays,"weekend","weekday")))
```

2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

Plot:

```r
par(mfrow=c(2,1))
for (type in c("weekend", "weekday")) {
  StepsIntervalFilled <- aggregate(steps ~ interval,
                          data=StepsDataFilled,
                          subset=StepsDataFilled$daytype==type,
                          FUN=mean)
  plot(StepsIntervalFilled, col = "red", type="l", main=type)
}
```

![](PeerAssessment1_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

