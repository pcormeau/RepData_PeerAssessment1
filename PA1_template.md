# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Load the data using the *read.csv* standard function:

```r
a <- read.csv(unz("activity.zip","activity.csv"))
a$date <- as.Date(a$date,"%Y-%m-%d")
```
The data-frame **a** must now contains 17568 observations of 3 variables. Let's check it:

```r
str(a)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
The **date** column is of *character* data type. As we will have to deal with it as a date, let's convert it to the *Date* data type and check the result: 

```r
a$date <- as.Date(a$date,"%Y-%m-%d")
str(a)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
It is better now. We have 3 variables:

*  **steps** : An integer value with the number of steps recorded in a 5 minutes interval.
*  **date** : The date on which the measurement was taken.
*  **interval** : An integer representing a identifier for the 5-minute interval in which the measurement was taken.

## What is mean total number of steps taken per day?
We will now calculate the mean total number of steps per day. There is two steps to achieve this calculation:

1. Calculate the total number of steps for each day
2. Calculate the mean and median of these totals

Let's start by calculating the total number of steps for each day and have a look at the resulting data-frame :

```r
ts <- aggregate(steps ~ date, data = a,  sum)
str(ts)
```

```
## 'data.frame':	53 obs. of  2 variables:
##  $ date : Date, format: "2012-10-02" "2012-10-03" ...
##  $ steps: int  126 11352 12116 13294 15420 11015 12811 9900 10304 17382 ...
```
Before calculating the mean and median, let's have a look of how the total number of steps per day are distributed:  

```r
library(ggplot2)
ggplot() + geom_histogram(data = ts,aes(steps), bins = 11, fill = "red", color = "red", alpha = 0.5) +
           xlab("Number of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
  
The majority of the the total numbers of steps recorded for a given day seems to be around 10000. Let calculate the mean and the median to confirm that:

```r
mean(ts$steps)
```

```
## [1] 10766.19
```

```r
median(ts$step)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
Let's have a look of how the steps are distributed during the day by calculating the average number of steps per 5-minutes intervals and displaying it on a chart:

```r
ms <- aggregate(steps ~ interval, data = a, mean)
qplot(interval , steps, data = ms, geom = "line", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

## Imputing missing values
There are some missing values in our data-set. This may have an impact on how we should interpret the data. So let's check them. How many NA's do we have:

```r
sum(is.na(a$steps))
```

```
## [1] 2304
```
Let's find out a little more about these NA's. Are they spread in all the data? Are whole days missing ? Are specific intervals being more impacted than others?
Let's calculate the proportion of NA's for each day (as Boolean value are treated as 0 and 1 in calculation, the mean function will give us the proportion of NA's) and have a look of what we have:

```r
na.d <- aggregate(is.na(steps) ~ date, data = a, mean)
summary(na.d)
```

```
##       date             is.na(steps)   
##  Min.   :2012-10-01   Min.   :0.0000  
##  1st Qu.:2012-10-16   1st Qu.:0.0000  
##  Median :2012-10-31   Median :0.0000  
##  Mean   :2012-10-31   Mean   :0.1311  
##  3rd Qu.:2012-11-15   3rd Qu.:0.0000  
##  Max.   :2012-11-30   Max.   :1.0000
```
So the minimum (Min.) is 0, meaning there are days without any missing values and the maximum (Max.) is 1 meaning that some days are full of NA's. Let's count the number of days that are neither full of NA's (1) nor without any NA's (0):

```r
nrow(subset(na.d, `is.na(steps)` != 0 & `is.na(steps)` != 1 ))
```

```
## [1] 0
```
Good news, there are no partial data for any dates. Meaning that only full days of data are missing.
Lets replace these missing data by the mean of the other days for the corresponding interval. We have already calculated the mean value per interval in the data frame **ms** but we will have to round the value to an integer as the other steps values:

```r
af <- merge(a, ms, by = "interval") # Add a column with the mean for the corresponding interval
af$steps.x <- ifelse(is.na(af$steps.x),as.integer(round(af$steps.y)), # replace NA's by the mean
                                       af$steps.x
                     )
af <- af[,c(2,3,1)] # drop the mean columns and reoder as in the original dataframe
colnames(af)[1] <- "steps" #rename the columan as in the original dataframe
```
Let's check if we still have missing values:

```r
sum(is.na(af$steps))
```

```
## [1] 0
```
Good. Now let's compare the total steps per days before and after filling in the missing value

```r
tsf <- aggregate(steps ~ date, data = af,  sum)
ggplot() + geom_histogram(data = ts ,aes(steps), bins = 11, color = "red", alpha = 0.5, fill = "red") +
           geom_histogram(data = tsf,aes(steps), bins = 11, color = "green", alpha = 0.5) +
           xlab("Number of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

By overlaying the two histograms (original in red and, the one where NA has been filled with border in green and fill in gray), we can see that the number of days where the number of steps are all identical but for one bin. The central bin is now higher. 
Let's compare the means and medians:

```r
matrix(c(mean(ts$steps),   mean(tsf$steps),
         median(ts$steps), median(tsf$steps)),
       nrow = 2, 
       dimnames = list(c("Original (ts)", "Na Replaced (tsf)") ,
                       c("mean","median")
                       )
       )
```

```
##                       mean median
## Original (ts)     10766.19  10765
## Na Replaced (tsf) 10765.64  10762
```

The means and medians are praticaly identical.

## Are there differences in activity patterns between weekdays and weekends?
First we need to add a variable to differentiate week days from weekend days then calculate the average number of steps per interval for the week days and weekend days separately:

```r
af$wd <- as.factor(ifelse(weekdays(af$date) %in% c("Saturday","Sunday"),"weekend","weekday"))
mswd <- aggregate(steps ~ interval + wd, data = af, mean)
```

Finally we compare the average number of steps during the day for the week days and weekend days :

```r
qplot(interval, steps, data = mswd, facets = "wd ~.", geom = "line", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

Looks like we start moving later during weekend that weekdays...
