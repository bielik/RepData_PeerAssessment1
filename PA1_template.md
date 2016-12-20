# PA1
##Loading and preprocessing the data

```r
setwd("C:/Users/marti/OneDrive/03_Data/01_Courses/Data Science Specialization/Reproducible research/Week 2/repdata%2Fdata%2Factivity")
amd <- read.csv("activity.csv")
amd$date <- as.Date(amd$date, "%Y-%m-%d")
```

##What is mean total number of steps taken per day?

```r
dTotal <- tapply(amd$steps, amd$date, sum, na.rm = TRUE)
par(mfrow = c(1,1), mar = c(3,3,3,3))
hist(dTotal)
```

![](PA1_template_files/figure-html/total number of steps per day-1.png)<!-- -->

```r
dMean <- mean(dTotal)
dMedian <- median(dTotal)
```

Mean of total number of steps per day is 9354.2295082 and meadeian of total number of steps per day is 10395.

##What is the average daily activity pattern?

```r
iMean <- tapply(amd$steps, amd$interval, mean, na.rm = TRUE)
plot(names(iMean), iMean, type = "l")
```

![](PA1_template_files/figure-html/daily activity pattern-1.png)<!-- -->

```r
maxInterval <- names(iMean)[which.max(iMean)]
```

The interval with maximum average steps across all the days is 835

##Imputing missing values

```r
naCount <- sum(as.vector(apply(amd, 1, function(x) length(which(is.na(x))) != 0)))
amdExtended <- amd 
for (i in seq_len(nrow(amd))) {
        if(is.na(amd$steps[i])){
                
                amdExtended$steps[i] <- mean(subset(amd, amd$interval == amd$interval[i])$steps, na.rm = TRUE)
        }
}

dTotalExtended <- tapply(amd$steps, amd$date, sum, na.rm = TRUE)
hist(dTotalExtended)
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
dMeanExtended <- mean(dTotalExtended)
dMedianExtended <- median(dTotalExtended)
dMeanDiff <- dMean - dMeanExtended
dMedianDiff <- dMedian - dMedianExtended
```

Total number of observations with missing values is 2304.
Missing values were inserted based on the mean of given interval across all days.
the new mean of total number of steps per day is 9354.2295082 and meadeian of total number of steps per day is 10395.
The replacement of missing values changed the total mean by 0 and the tolal median by 0.

##Are there differences in activity patterns between weekdays and weekends?

```r
wd <- weekdays(amd$date) %in% c("Sunday", "Saturday")
amdExtended$weekday <- wdPattern <- factor(wd, labels = c("week", "weekend"))
amdExtendedSplit <- split(amdExtended, amdExtended$weekday)
res <- lapply(amdExtendedSplit, function(x) tapply(x[,1], x[,3], mean))
par(mfrow = c(1,2), mar = c(4,4,4,4))
plot(names(res[[1]]), res[[1]], main = "Week", xlab = "interval", ylab = "Number of steps", type = "l")
plot(names(res[[2]]), res[[2]], main = "Weekend", xlab = "interval", ylab = "Number of steps", type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

