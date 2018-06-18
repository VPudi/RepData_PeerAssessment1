---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

```{r setoptions, echo=TRUE}```

## Loading and preprocessing the data
### Load the dataset into a dataframe and convert the date to a Date column


```r
ds1 <- read.csv("activity.csv");

ds1$date <- as.Date(ds1$date);
```

## What is mean total number of steps taken per day?



```r
library(dplyr);
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
plot1DS <- summarize(group_by(ds1,date),TotalSteps=sum(steps));

hist(plot1DS$TotalSteps, xlab="Total Steps Per Day", main="Histogram of Total Steps Per Day");
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
summary(plot1DS$TotalSteps);
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10765   10766   13294   21194       8
```


## What is the average daily activity pattern?


```r
plot2DS <- summarize(group_by(ds1,interval),AverageSteps=mean(steps, na.rm=TRUE));

with (plot2DS, plot(interval, AverageSteps, type="l"));
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
intmax <- subset(plot2DS, AverageSteps==max(plot2DS$AverageSteps))$interval;
```

The 5-min interval with the Max Avg steps is 835


## Imputing missing values


```r
NARows <- sum(!complete.cases(ds1));
```

The number of rows with missing values = 2304

create a new dataset by merging the original dataset with plot2DS which has the mean of steps taken in a given interval across all days


```r
dsnew <- merge(ds1, plot2DS, by="interval");
dsnew <- arrange(dsnew, date, interval);
```

impute missing values by setting them to mean of that interval across all days


```r
dsnew$steps[is.na(dsnew$steps)] <- dsnew$AverageSteps[is.na(dsnew$steps)];
```

Make a histogram of the total number of steps taken each day


```r
plot3DS <- summarize(group_by(dsnew,date),TotalSteps=sum(steps));
hist(plot3DS$TotalSteps, xlab="Total Steps Per Day", main="Histogram of Total Steps Per Day");
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

calculate the mean and median of the total number of steps taken per day


```r
summary(plot3DS$TotalSteps);
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

As we can see the mean and median do not change after imputing the missing values

The impact of imputing missing values is not significant in this case


## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
dsnew$daytype <- ifelse(weekdays(dsnew$date,TRUE)=="Sat" |weekdays(dsnew$date,TRUE)=="Sun", "Weekend", "Weekday");
dsnew$daytype <- factor(dsnew$daytype);
```

create dataset for the activity pattern panel plot


```r
plot4DS <- summarize(group_by(dsnew,interval,daytype),AvgSteps=mean(steps));
```

Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(ggplot2);
qplot(interval, AvgSteps, data=plot4DS, facets=daytype ~., geom="line")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


*From the plot above, we can see that the activity pattern during weekend is*
*higher during interval 1000 - 2000*









