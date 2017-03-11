# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.3.1
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
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.3.3
```

```r
setwd("E:/Dropbox/Dropbox/Data Science Courses/Data Science Specialization/05 - Reproducible Research/Assignment 1/repdata%2Fdata%2Factivity")
Data = read.csv("activity.csv")
```

The *Data* data frame has the original data loaded and the *Data_DateTransform* data frame has the transformed data with Dates in R format.

```r
Data_DateTransform <- Data %>%
  mutate(date=as.Date(date, "%Y-%m-%d"))
```

## What is mean total number of steps taken per day?


```r
Data_CC <- Data_DateTransform[complete.cases(Data_DateTransform),]

Data_sum_by_day <- Data_CC %>%
  group_by(date) %>%
  summarise(total=sum(steps))
```

Histogram of the total number of steps taken each day

```r
hist(Data_sum_by_day$total)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)\

Mean and median number of steps taken each day

```r
mean_steps <- mean(Data_sum_by_day$total)
print(mean_steps)
```

```
## [1] 10766.19
```

```r
median_steps <- median(Data_sum_by_day$total)
print(median_steps)
```

```
## [1] 10765
```

Mean Number of Steps Per Day: 10766
Median Number of Steps Per Day: 10765

## What is the average daily activity pattern?

Time series plot of the average number of steps taken

```r
Data_Interval <- Data_CC %>%
  group_by(interval) %>%
  summarise(average=mean(steps))

plot(x=Data_Interval$interval, y=Data_Interval$average, type='l')
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)\

The 5-minute interval that, on average, contains the maximum number of steps

```r
interval_max = Data_Interval$interval[which.max(Data_Interval$average)]
print(interval_max)
```

```
## [1] 835
```
The most activity happens around 8:35am.

## Imputing missing values


```r
Data_NA <- Data_DateTransform[!complete.cases(Data_DateTransform),]
print(nrow(Data_NA))
```

```
## [1] 2304
```

```r
missing_values<-nrow(Data_NA)
```
Total Number of Missing Values: 2304

Fill in the missing values with the mean.

```r
df_incomplete <- Data_DateTransform[!complete.cases(Data_DateTransform),]
df_merge <- merge(df_incomplete, Data_Interval)
Data_Impute <- Data_DateTransform
Data_Impute[!complete.cases(Data_Impute),c('steps')] <-df_merge[,c('average')]
```

Histogram of the total number of steps taken each day after missing values are imputed

```r
Data_Impute_sum_by_day <- Data_Impute %>%
  group_by(date) %>%
  summarise(total=sum(steps))

hist(Data_Impute_sum_by_day$total)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)\


## Are there differences in activity patterns between weekdays and weekends?


```r
fun_day_type <- function(x) {
  x <-  as.Date(x)
  day_type <- 'weekday'
  if ( (weekdays(x) == 'Saturday') | (weekdays(x)=='Sunday') ) day_type='weekend'
  day_type
}

Data_Impute$daytype <- apply(Data_Impute, 1, function(x) { fun_day_type(x[2]) } )

df_comp <- Data_Impute %>% 
  group_by(daytype,interval) %>%
  summarise(average=mean(steps))
  
xyplot(average~interval|daytype, data=df_comp, type='l',layout=(c(1,2)))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)\

Yes, there is a difference in activity patterns between weekdays and weekends.
