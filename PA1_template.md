
# Reproducible Research: Peer Assessment 1

### Loading and preprocessing the data

The first thing we need to do is load data into R and clean it up (if necessary)


```r
library(lattice)
dat <- read.csv("activity.csv",sep=",",na.strings = "NA")
dat$date <- as.Date(dat$date, "%Y-%m-%d")
```

### What is mean total number of steps taken per day?

First, Let's see what a data looks like

```r
totalSteps <- aggregate(steps ~ date, data = dat, FUN=sum, na.rm=T)
plot(totalSteps$date, totalSteps$steps, main = "Histogram of Steps Taken per day", 
     xlab = "October to November 2012", ylab = "Frequency", type = "h", 
     lwd = 4, col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)

Mean

```r
mean(totalSteps$steps)
```

```
## [1] 10766.19
```

Median

```r
median(totalSteps$steps)
```

```
## [1] 10765
```

### What is the average daily activity pattern?

First, create subset data mean from variable steps taken in 5-minute interval and then plot the data, so we can see its pattern


```r
avgSteps <- aggregate(steps ~ interval, data = dat, FUN=mean, na.rm=T)
plot(avgSteps$interval, avgSteps$steps, type = "l", xlab = "Time Intervals (5 mnt)", 
     ylab = "Average Number of Steps Taken", main = "Average Steps Taken each 5 minute Intervals", 
     col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)

The Peak Data 

```r
avgSteps[which.max(avgSteps$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

### Imputing Missing Value


```r
MissingValues <- sum(is.na(dat))
MissingValues
```

```
## [1] 2304
```

from above we can see there are days or interval which have missing values. The presence of missing values sometimes can lead us into bias in some calculation or summaries of data, so we need to fill it. I am gonna fill it with average of variable appears.


```r
NaData <- which(is.na(dat))
for(i in NaData){
        dat$steps[i] <- avgSteps[avgSteps$interval==dat$interval[i],2]
}
```

```r
sum(is.na(dat))
```

```
## [1] 0
```
All of the missing values are filled in 

Now, using the filled data set, i am gonna make histogram of the total number of steps taken each day to see if there are changes and calculate the mean and median.


```r
totalSteps <- aggregate(steps ~ date, data = dat, FUN=sum, na.rm=T)

plot(totalSteps$date, totalSteps$steps, main = "Histogram of Steps Taken per day", 
     xlab = "October to November 2012", ylab = "Frequency", type = "h", 
     lwd = 4, col = "red")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)

```r
mean(totalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(totalSteps$steps)
```

```
## [1] 10766.19
```
Now we have same median and mean. The reason is because the missing values are filled in with mean of its variable,so taking the mean will give the same answer.  

### Are there differences in activity patterns between weekdays and weekends?

First, let's make variable factor (Weekdays and weekand) then make panel plot containing plots of average number of steps taken on weekdays and weekends.


```r
days <- weekdays(dat$date)
weekd <- c("Monday","Thursday","Tuesday","Wednesday","Friday")
dat$days <- ifelse(days %in% weekd,"Weekdays","Weekend")

avgStepsDay <- aggregate(steps ~ interval + days, data = dat, mean)

xyplot(steps ~ interval | days, avgStepsDay, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)


I don't know why my plot doesn't appear properly. I tried many times and still got the same. But when i run this code beyond Rmarkdown it is work. You can see my plot here..

![](plot4.png)
