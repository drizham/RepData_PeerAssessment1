# Reproducible Research Assignment 1

The data consists of two month of data from a personal activity monitoring device like fitbit or jawbone collected during the months of October and November 2012. 

## Loading and preprocessing the data
Load data


```r
data <- read.csv(file = "activity.csv", header = T)
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?
The total steps taken by day calculated by aggregating day (factor),


```r
daily_steps <- aggregate(data$steps, by = list(data$date), FUN = sum)
daily_total_steps <- daily_steps[!(is.na(daily_steps$x)), 2]
hist(daily_total_steps, xlab = "Daily Total Steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


The mean and median is calculated for the total steps.


```r
mean(daily_steps$x, na.rm = T)
```

```
## [1] 10766
```

```r
median(daily_steps$x, na.rm = T)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
The average steps in 5 minutes interval is calculated and ploted.


```r
interval_means <- aggregate(data$steps, by = list(data$interval), FUN = mean, 
    na.rm = TRUE)
plot(interval_means, type = "l", xlab = "minutes", ylab = "number of steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

The interval with the maximum steps over all days is calculated.

```r
max_mean <- interval_means[interval_means$x == max(interval_means$x), ]
max_mean
```

```
##     Group.1     x
## 104     835 206.2
```


## Missing data
There are missing data in the data set.

The numbers of meassurments with NA for steps are:

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

Those meassurments with NA will be replaced by the mean of the coresponding interval (which has been calculated in the above step).


```r
merged <- merge(x = data, y = interval_means, by.x = "interval", by.y = "Group.1")
my.na <- is.na(merged$steps)
merged[my.na, 2] <- merged[my.na, 4]
data2 <- data.frame(steps = merged$steps, date = merged$date, interval = merged$interval)

interval_means <- aggregate(data2$steps, by = list(data2$interval), FUN = mean)
plot(interval_means$x, type = "l", xlab = "minutes", ylab = "number of steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

The mean and median is calculated for the total steps of the new data set.


```r
daily_steps <- aggregate(data2$steps, by = list(data2$date), FUN = sum)
mean(daily_steps$x, na.rm = T)
```

```
## [1] 10766
```

```r
median(daily_steps$x, na.rm = T)
```

```
## [1] 10766
```

With that replacement only the median has changed for just one step.

## Weekday and Weekend
Creating a new column with the factor weekday and weekend.


```r
library(timeDate)
data2 = within(data2, {
    daytype = as.factor(ifelse(isWeekday(data2$date), "weekday", "weekend"))
})
str(data2)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  1.72 0 0 0 0 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 54 28 37 55 46 20 47 38 56 ...
##  $ interval: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ daytype : Factor w/ 2 levels "weekday","weekend": 1 1 2 1 2 1 2 1 1 2 ...
```


Than two plots are created to show the average over weekdays and weekends.

```r
par(mfrow = c(2, 1))
X <- split(data2, data2$daytype)
weekday_interval_means <- aggregate(X[[1]]$steps, by = list(X[[1]]$interval), 
    FUN = mean)
weekend_interval_means <- aggregate(X[[2]]$steps, by = list(X[[2]]$interval), 
    FUN = mean)
plot(weekday_interval_means$x, type = "l", xlab = "minutes", ylab = "number of steps", 
    main = "Weekday")
plot(weekend_interval_means$x, type = "l", xlab = "minutes", ylab = "number of steps", 
    main = "Weekend")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

