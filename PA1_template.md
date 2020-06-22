---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(tidyverse)
```


```r
raw_data <- read_csv("./activity.zip")
```

```
## Parsed with column specification:
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```

```r
data <- raw_data %>% drop_na()
```

## What is mean total number of steps taken per day?

```r
daily_steps <- data %>%
  group_by(date) %>%
  summarise(steps = sum(steps))
ggplot(daily_steps, aes(x = steps)) +
  geom_histogram() +
  labs(title = "Histogram of the total number of steps taken each day") +
  geom_vline(aes(xintercept = median(steps)))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

We notice that the mean: 1.0766189\times 10^{4} is equal to the median: 1.0765\times 10^{4}

## What is the average daily activity pattern?
Let group by intervals and compute the average across all days.


```r
interval_steps  <- data %>%
  group_by(interval) %>%
  summarise(steps = mean(steps))

ggplot(interval_steps, aes(x = interval, y = steps)) +
  labs(title = "Average number of steps taken accross all days per interval") +
  geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

We can immediately see that the interval: **835** contains, on average across all the days, the maximum number of steps: **206.1698113**. 

## Imputing missing values

There is **2304** missing values from the `steps`variable.

To handle the missing data we could use the mean for that 5-minute interval accross all days as follow


```r
filled_data <- raw_data

filled_data[is.na(filled_data$steps), "steps"] <-
  left_join(
    filled_data[is.na(filled_data$steps), ],
    interval_steps,
     by = "interval"
)$steps.y

summary(filled_data)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```


```r
daily_steps <- filled_data %>%
  group_by(date) %>%
  summarise(steps = sum(steps))
ggplot(daily_steps, aes(x = steps)) +
  geom_histogram() +
  labs(title = "Histogram of the total number of steps taken each day") +
  geom_vline(aes(xintercept = median(steps)))
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

We notice that the mean: 1.0766189\times 10^{4} is equal to the median: 1.0766189\times 10^{4}, this is not different with the values previously found. We also notice that the new histogram is more centered around the mean value than the first one.

## Are there differences in activity patterns between weekdays and weekends?


```r
filled_data  <- filled_data %>% mutate(
    daytype  = factor(
      weekdays(date) %in% c("Sunday", "Saturday"),
      labels = c("weekday", "weekend")
    )
  )

interval_steps  <- filled_data %>%
  group_by(interval, daytype) %>%
  summarise(steps = mean(steps))

ggplot(interval_steps, aes(x = interval, y = steps)) +
  labs(title = "Average number of steps taken accross all days per interval") +
  geom_line() +
  facet_wrap(~ daytype, ncol = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


We can notice that the activity during the weekend is reducing early in the mornings but slightly increases after that compared to weekdays. A potential explanation is the daily work routine where commuting increases the activity while office hours decreases it.

