---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

```r
library(knitr)
opts_chunk$set(message = FALSE)
options(scipen=999)
```

```r
library(tidyverse)
library(lubridate)
```
## Loading and preprocessing the data

Load the data:


```r
dat <- read_csv("./data/activity.csv")
```

Process/transform the data (if necessary) into a format suitable for your analysis:

```r
str(dat)
```

```
## tibble [17,568 × 3] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ steps   : num [1:17568] NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date[1:17568], format: "2012-10-01" "2012-10-01" ...
##  $ interval: num [1:17568] 0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   steps = col_double(),
##   ..   date = col_date(format = ""),
##   ..   interval = col_double()
##   .. )
```

## What is mean total number of steps taken per day?

Calculate the total number of steps taken per day:


```r
dat %>% 
  group_by(date) %>% 
  summarise(sum = sum(steps, na.rm = TRUE))
```

```
## # A tibble: 61 x 2
##    date         sum
##    <date>     <dbl>
##  1 2012-10-01     0
##  2 2012-10-02   126
##  3 2012-10-03 11352
##  4 2012-10-04 12116
##  5 2012-10-05 13294
##  6 2012-10-06 15420
##  7 2012-10-07 11015
##  8 2012-10-08     0
##  9 2012-10-09 12811
## 10 2012-10-10  9900
## # … with 51 more rows
```

Make a histogram of the total number of steps taken each day:


```r
dat %>% 
  group_by(date) %>% 
  summarise(sum = sum(steps, na.rm = TRUE)) %>% 
  ggplot(aes(sum)) + geom_histogram()
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Calculate and report the mean and median of the total number of steps taken per day:


```r
dat %>% 
  group_by(date) %>% 
  summarise(sum = sum(steps, na.rm = TRUE)) %>% 
  summarise(mean = mean(sum, na.rm = TRUE))
```

```
## # A tibble: 1 x 1
##    mean
##   <dbl>
## 1 9354.
```


```r
dat %>% 
  group_by(date) %>% 
  summarise(sum = sum(steps, na.rm = TRUE)) %>% 
  summarise(median = median(sum, na.rm = TRUE))
```

```
## # A tibble: 1 x 1
##   median
##    <dbl>
## 1  10395
```

## What is the average daily activity pattern?

Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):


```r
dat_intervalmean <- dat %>% 
  group_by(interval) %>% 
  summarise(mean = mean(steps, na.rm = T))

dat_intervalmean %>% 
  ggplot(aes(interval, mean)) + geom_line() 
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
dat_intervalmean$interval[which.max(dat_intervalmean$mean)]
```

```
## [1] 835
```

## Imputing missing values:

Calculate and report the total number of missing values in the dataset:


```r
sum(!complete.cases(dat))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset:

I will use the mean of the 5 minute interval.

Create a new dataset that is equal to the original dataset but with the missing data filled in:


```r
dat_imputed <- left_join(dat, dat_intervalmean)

dat_imputed <- dat_imputed %>% 
  mutate(steps = ifelse(is.na(steps),mean,steps))
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
dat_imputed %>% 
  group_by(date) %>% 
  summarize(sum = sum(steps)) %>% 
  ggplot(aes(sum)) + geom_histogram()
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


```r
dat_imputed %>% 
  group_by(date) %>% 
  summarise(sum = sum(steps)) %>% 
  summarise(mean = mean(sum))
```

```
## # A tibble: 1 x 1
##     mean
##    <dbl>
## 1 10766.
```


```r
dat_imputed %>% 
  group_by(date) %>% 
  summarise(sum = sum(steps)) %>% 
  summarise(median = median(sum))
```

```
## # A tibble: 1 x 1
##   median
##    <dbl>
## 1 10766.
```

The mean without imputation was 9354.2295082 and the mean **with** imputation was 10766.  
The median without imputation was 10395 and the median **with** imputation was 10766.  
Imputation slightly increased the estimates of the total daily number of steps.

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day:


```r
dat_imputed <- dat_imputed %>% 
  mutate(day_of_week = wday(date, label = TRUE)) %>% 
  mutate(day_of_week = ifelse(day_of_week == "Sat" | day_of_week == "Sun","Weekend","Weekday")) %>% 
  mutate(day_of_week = as_factor(day_of_week))
```

Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis): 

```r
dat_imputed %>% 
  group_by(day_of_week, interval) %>% 
  summarise(mean_per_interval = mean(steps)) %>% 
  ggplot(aes(interval, mean_per_interval)) + geom_line() + 
  facet_grid(day_of_week ~ .) + ylab("steps")  
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->
