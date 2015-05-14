---
title: "Reproducible Research: Peer Assessment 1"
output: 
html_document:
keep_md: true
---



## Loading and preprocessing the data
###Load the data (i.e. read.csv())

```{r echo=TRUE, cache=TRUE}
unzip("activity.zip")
fullData <- read.csv("activity.csv")
```

### Process/transform the data (if necessary) into a format suitable for your analysis
```{r echo=TRUE, cache=TRUE}
totalStepsPerDay <- aggregate(steps ~ date, data = fullData, sum, na.rm = TRUE)
```

```{r, cache=TRUE}
  if(!("ggplot2" %in% rownames(installed.packages()))){
    install.packages("ggplot2")  
  }
  if(!("lubridate" %in% rownames(installed.packages()))){
    install.packages("lubridate")  
  }
```


## What is mean total number of steps taken per day?

### Calculate the total number of steps taken per day
```{r echo=TRUE, cache=TRUE}
totalStepsPerdaySum <- sum(totalStepsPerDay$steps)
```

### If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
```{r echo=TRUE, cache=TRUE}
hist(totalStepsPerDay$steps)
```

### Calculate and report the mean and median of the total number of steps taken per day
```{r echo=TRUE, cache=TRUE}
totalStepsPerdayMean <- mean(totalStepsPerDay$steps)
totalStepsPerdayMedian <- median(totalStepsPerDay$steps)

cat(paste0("the mean and median of the total number of steps taken per day is: ",totalStepsPerdayMean, " and ", totalStepsPerdayMedian))
```



## What is the average daily activity pattern?

### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r echo=TRUE, cache=TRUE}
stepsMeanPerInterval <- tapply(fullData$steps, fullData$interval, mean, na.rm = T)
plot(stepsMeanPerInterval, type = "l")

```

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r echo=TRUE, cache=TRUE}
seq(along = stepsMeanPerInterval)[stepsMeanPerInterval == max(stepsMeanPerInterval)]

```







## Imputing missing values

### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs) 

```{r echo=TRUE, cache=TRUE}
missing <- is.na(fullData$steps)
table(missing)

```

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

### Create a new dataset that is equal to the original dataset but with the missing data filled in

```{r echo=TRUE, cache=TRUE}
fullDataWitoutNAs <- fullData
fullDataWitoutNAs[is.na(fullData)] = as.integer(runif(1, 5, 7))

```

### Create a new dataset that is equal to the original dataset but with the missing data filled in
```{r echo=TRUE, cache=TRUE}
avgActivityDailyMedian <- aggregate(interval ~ date + steps, data = fullData, median)

```


### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```{r echo=TRUE, cache=TRUE}
avgActivityDailyMean <- aggregate(interval ~ date + steps, data = fullDataWitoutNAs, mean)
hist(avgActivityDailyMedian$steps)
hist(avgActivityDailyMean$steps)
```


### Do these values differ from the estimates from the first part of the assignment? 
Reply: It seems not make much difference.


### What is the impact of imputing missing data on the estimates of the total daily number of steps?
Reply: I didn't figure out.


### Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
```{r echo=TRUE, cache=TRUE}
weekDaysFactor <- factor("weekday","weekend")
```

### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:

```{r echo=TRUE}
library(lubridate)
subdata <- subset(fullData)
subdata$date <- as.Date(subdata$date)
subdata$V4<- weekdays(subdata$date)
colnames(subdata)[4] <- "week"
subdata[5]<- ifelse(subdata$week=="Sunday" | subdata$week=="Saturday","Weekend","Weekday")
colnames(subdata)[5] <- "wday"

library(lattice)
xyplot(steps~interval|subdata$wday, data = subdata, type = "l", layout = c(1, 2), ylab = "Average Number of Steps")

```

## Are there differences in activity patterns between weekdays and weekends?
Reply: Yes. Seems that during the weekdays the activities starts more intensity, as well as, during the weekend we can see less intensity during all graphic.
