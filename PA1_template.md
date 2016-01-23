
#Reproducible Research: Peer Assessment 1
Fernanda Melo

#Introduction

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals throughout the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

This document presents the results from Project Assignment 1 in the Coursera course Reproducible Research, written in a single R markdown document that can be processed by knitr and transformed into an HTML file

##R preparations

In this document code will be represented to show how the results have been achieved. Set the default of echo to be true throughout the document:


```r
library(knitr)
opts_chunk$set(echo = TRUE)


library(dplyr)

library(lubridate)
library(ggplot2)
```

## Loading and preprocessing the data

##### 1. Load the data (i.e. ```read.csv()```)

The data is loaded using the ```read.csv()```.

NOTE: It is assumed that you have already downloaded the activity.csv and saved it in your working directory. 


```r
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}

activity<- read.csv("activity.csv", header = TRUE, sep = ',', 
                    colClasses = c("numeric", "character","integer"))
```

##### 2. Process/transform the data (if necessary) into a format suitable for your analysis

Change the date into dateformat using ```lubridate```:


```r
activity$date <- ymd(activity$date)
```

Check the data with ```names()```,```str()``` and ```head()```:


```r
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```



```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : POSIXct, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


Now everything is ready and set up for solving some problems.

 
## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

2. Make a histogram of the total number of steps taken each day

3. Calculate and report the mean and median of the total number of steps taken per day


###Methodology and Result

1.Calculate the total number of steps per day using ```dplyr``` and group by ```date```:


```r
steps <- activity%>%
        filter(!is.na(steps)) %>%
        group_by(date) %>%
        summarize(steps = sum(steps)) %>%
        print
```

```
## Source: local data frame [53 x 2]
## 
##          date steps
##        (time) (dbl)
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## ..        ...   ...
```


2.Use ```ggplot2``` for making the histogram:


```r
ggplot(steps, aes(x = steps)) +
        geom_histogram(fill = "blue",color="red", binwidth = 1000) +
        labs(title = "Histogram of Steps per day", x = "Steps per day", y = "Frequency")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)


3. Calculate the mean and median of the total number of steps taken per day:


```r
mean_steps <- mean(steps$steps, na.rm = TRUE)

median_steps <- median(steps$steps, na.rm = TRUE)
```


```r
mean_steps
```

```
## [1] 10766.19
```



```r
median_steps
```

```
## [1] 10765
```

Mean steps are 10766 and median steps are 10765.

## What is the average daily activity pattern?


1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?.

###Methodology and Result

1.Calculate the average number of steps taken in each 5-minute interval per day using ```dplyr``` and group by ```interval```:



```r
interval <- activity %>%
        filter(!is.na(steps)) %>%
        group_by(interval) %>%
        summarize(steps = mean(steps))
```

Use ```ggplot2``` for making the time series of the 5-minute interval and average steps taken:


```r
## plot time series
ggplot(interval, aes(x=interval, y=steps)) +
        geom_line(color = "blue")+
        labs(title = "Time Series Plot of the 5-Minute Interval\n Averaged Across All Days", 
        x = "5-Minute Interval", y = "Average Number of Steps Taken,\n Averaged Across All Days")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)


2.Use ```which.max()``` to find out the maximum steps, on average, across all the days:


```r
interval[which.max(interval$steps),]
```

```
## Source: local data frame [1 x 2]
## 
##   interval    steps
##      (int)    (dbl)
## 1      835 206.1698
```


The interval 835 has, on average, the highest count of steps, with 206 steps.



## Imputing missing values


Note that there are a number of days/intervals where there are missing values (coded as ```NA```). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

###Methodology and Result

1.Summarize all the missing values:


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```



Missing values are 2304.

2. Let's take the approach to fill in a missing ```NA``` with the average number of steps in the same 5-min interval.

3. Create a new dataset as the original and use ```tapply``` for filling in the missing values with the average number of steps per 5-minute interval:



```r
full_activity <- activity

nas <- is.na(full_activity$steps)

avg_interval <- tapply(full_activity$steps, full_activity$interval, mean, na.rm=TRUE, simplify=TRUE)

full_activity$steps[nas] <- avg_interval[as.character(full_activity$interval[nas])]
```

Check that there are no missing values:


```r
sum(is.na(full_activity$steps))
```

```
## [1] 0
```


No more missing values.

4.Calculate the number of steps taken in each 5-minute interval per day using ```dplyr``` and group by ```date```.

Use ```ggplot2``` for making the histogram:



```r
full_steps <- full_activity %>%
        filter(!is.na(steps)) %>%
        group_by(date) %>%
        summarize(steps = sum(steps)) %>%
        print
```

```
## Source: local data frame [61 x 2]
## 
##          date    steps
##        (time)    (dbl)
## 1  2012-10-01 10766.19
## 2  2012-10-02   126.00
## 3  2012-10-03 11352.00
## 4  2012-10-04 12116.00
## 5  2012-10-05 13294.00
## 6  2012-10-06 15420.00
## 7  2012-10-07 11015.00
## 8  2012-10-08 10766.19
## 9  2012-10-09 12811.00
## 10 2012-10-10  9900.00
## ..        ...      ...
```


```r
ggplot(full_steps, aes(x = steps)) +
    geom_histogram(fill = "blue",color="red", binwidth = 1000) +
    labs(title = "Histogram of Steps per day, including missing values", x = "Steps per day", y = "Frequency")
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19-1.png)


Calculate the mean and median steps with the filled in values:


```r
mean_full_steps <- mean(full_steps$steps, na.rm = TRUE)
median_full_steps <- median(full_steps$steps, na.rm = TRUE)
```


```r
mean_full_steps
```

```
## [1] 10766.19
```


```r
median_full_steps
```

```
## [1] 10766.19
```



The impact of imputing missing data with the average number of steps in the same 5-min interval is that both the mean and the median are equal to the same value: 10766.



## Are there differences in activity patterns between weekdays and weekends?


For this part the weekdays() will come handy. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

###Methodology and Result

1.Use ```dplyr``` and ```mutate``` to create a new column, weektype, and apply whether the day is weekend or weekday:



```r
# Changing my system local to English because the date comes in portuguese:

## preserve the existing locale
my_locale <- Sys.getlocale("LC_ALL")

## change locale to English
Sys.setlocale("LC_ALL", "English")
```

```
## [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"
```

```r
full_activity <- mutate(full_activity, weektype = ifelse(weekdays(full_activity$date) =="Saturday"
                        | weekdays(full_activity$date) =="Sunday", "Weekend", "Weekday"))

full_activity$weektype <- as.factor(full_activity$weektype)

## restore local
Sys.setlocale("LC_ALL", my_locale)
```

```
## Warning in Sys.setlocale("LC_ALL", my_locale): OS reports
## request to set locale to "LC_COLLATE=English_United States.
## 1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.
## 1252;LC_NUMERIC=C;LC_TIME=English_United States.1252" cannot be honored
```

```
## [1] ""
```

```r
head(full_activity)
```

```
##       steps       date interval weektype
## 1 1.7169811 2012-10-01        0  Weekday
## 2 0.3396226 2012-10-01        5  Weekday
## 3 0.1320755 2012-10-01       10  Weekday
## 4 0.1509434 2012-10-01       15  Weekday
## 5 0.0754717 2012-10-01       20  Weekday
## 6 2.0943396 2012-10-01       25  Weekday
```


2. Calculate the average steps in the 5-minute interval and use ```ggplot2``` for making the time series of the 5-minute interval for weekday and weekend, and compare the average steps:



```r
interval_full <- full_activity %>%
        group_by(interval, weektype) %>%
        summarise(steps = mean(steps))


p <- ggplot(interval_full, aes(x=interval, y=steps, color = weektype)) +
        geom_line() +
        facet_wrap(~weektype, ncol = 1, nrow=2)+
        labs(title = "Time Series Plot of the 5-Minute Interval\n Averaged Across All Weekday Days or Weekend Days", 
             x = "5-Minute Interval", y = "Average Number of Steps Taken")

print(p)
```

![plot of chunk unnamed-chunk-24](figure/unnamed-chunk-24-1.png)


From the two plots it seems that the test object is more active earlier in the day during weekdays compared to weekends, but more active throughout the weekends compared with weekdays (probably because the oject is working during the weekdays, hence moving less during the day).
