# Reproducible Research: Peer Assessment 1 - J Sieber 5/17/14


# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
  filename<-"activity.csv"
  activity_data <- read.csv(filename,header=TRUE,na.strings="NA")
 total_steps_daily <- aggregate(steps~date,activity_data,sum,na.rm=TRUE)
```

## What are the mean and median total number of steps taken per day?

```r
   hist(total_steps_daily$steps,main="Total number of steps taken per day frequencies",xlab="Intervals for total steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
  mean(total_steps_daily$steps)
```

```
## [1] 10766
```

```r
  median(total_steps_daily$steps)  
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
  mean_steps_interval <- aggregate(steps~interval,activity_data,mean,na.rm=TRUE)
  plot(mean_steps_interval$interval,mean_steps_interval$steps,'l')
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

Most active interval


```r
  max_index <- which(max(mean_steps_interval$steps)==mean_steps_interval$steps)
  mean_steps_interval[max_index,]$interval
```

```
## [1] 835
```
## Imputing missing values
Number of missing values:

```r
length(which(is.na(activity_data$steps)==TRUE))
```

```
## [1] 2304
```



```r
 modified_activity_data <- activity_data

  for (i in 1:nrow(activity_data))
  {
    if (is.na(activity_data$steps[i])) # missing value 
    {
      interval_var <- activity_data$interval[i] # interval at which data was missing
      # assign to new frame, the mean of steps taken corress. to that interval.
      modified_activity_data$steps[i]<-mean_steps_interval$steps[which(mean_steps_interval==interval_var)]
    }
  }
  total_steps_daily_mod <- aggregate(steps~date,modified_activity_data,sum)
  hist(total_steps_daily_mod$steps,main="Total number of steps taken per day frequencies",xlab="Intervals for total steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

Mean and Median steps taken per day after importing missing values:


```r
  mean(total_steps_daily_mod$steps)
```

```
## [1] 10766
```

```r
  median(total_steps_daily_mod$steps)
```

```
## [1] 10766
```


## Are there differences in activity patterns between weekdays and weekends?

```r
 library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.0.3
```

```r
 weekday_list <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
                    "Friday")
  modified_activity_data$weekday <- (weekdays(as.Date(modified_activity_data$date)) %in% weekday_list)
  mean_steps_interval_bywday<- aggregate(steps~interval+weekday,modified_activity_data,mean)
  mean_steps_interval_bywday$weekday[mean_steps_interval_bywday$weekday=="TRUE"]="weekday"
  mean_steps_interval_bywday$weekday[mean_steps_interval_bywday$weekday=="FALSE"]="weekend"
  mean_steps_interval_bywday$weekday<-as.factor(mean_steps_interval_bywday$weekday)
  xyplot(steps~interval|weekday,mean_steps_interval_bywday,type="l",layout=c(1,2)) 
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 
