Reproducible Research - Peer Assessment 1
============================================

###Rahul Tomar

###January 18, 2015

##Load the Activity monitoring data

```{r echo=TRUE}
data <- read.csv("activity.csv")
```

##Process/transform the dataset by removing the missing data

```{r echo=TRUE}
rawValues <- complete.cases(data)
nMissingValues <- length(rawValues[rawValues==FALSE])
nCompleteValues <- length(rawValues[rawValues==TRUE])
title <- "Activity Monitoring Data"
barplot(table(rawValues), main=title, xaxt='n', col=c("red", "blue"))
legend("topleft",legend=c("Missing Data", "Complete Data"), fill=c("red", "blue"))
text(0.7,0,labels=nMissingValues,pos=3)
text(1.9,0,labels=nCompleteValues,pos=3)
```

## Q1 - What is mean total number of steps taken per day?

###*Make a histogram of the total number of steps taken each day*
```{r echo=TRUE}
totalSteps <- aggregate(steps ~ date, data=data, sum, na.rm=TRUE)
hist(totalSteps$steps, main="Total steps per day", xlab="day", col="red")
```

###*Mean total number of steps*

```{r echo=TRUE}
mean(totalSteps$steps)
```

###*Median total number of steps*

```{r echo=TRUE}
median(totalSteps$steps)
```


## Q2 - What is the average daily activity pattern?

###*Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)*

```{r echo=TRUE}
timeSeries <- tapply(data$steps, data$interval, mean, na.rm=TRUE)
plot(row.names(timeSeries), timeSeries, type="l", xlab="5-minute interval", ylab="Averaged across all days", main="Average Daily Activity Pattern", col="red")
```

###*Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*

```{r echo=TRUE}
maxInterval <- which.max(timeSeries)
names(maxInterval)
```


##Q3 - Imputing missing values

###*Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)*

```{r echo=TRUE}
nMissingValues
```

###*Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.*

```{r echo=TRUE}
averageSteps <- aggregate(steps ~ interval, data=data, FUN=mean)
fillNA <- numeric()
for (i in 1:nrow(data)){
    obs <- data[i,]
    if(is.na(obs$steps)){
        steps <- subset(averageSteps, interval==obs$interval)$steps
    }else {
        steps <- obs$steps
    }
    fillNA <- c(fillNA, steps)
}
```

###*Create a new dataset that is equal to the original dataset but with the missing data filled in.*

```{r echo=TRUE}
newData <- data
newData$steps <- fillNA
```

###*Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?*

```{r echo=TRUE}
newTotalSteps <- aggregate(steps ~ date, data=newData, sum, na.rm=TRUE)
hist(newTotalSteps$steps, main="Total steps per day", xlab="day", col="red")
```

###*Mean total number of steps*

```{r echo=TRUE}
mean(newTotalSteps$steps)
```

###*Median total number of steps*

```{r echo=TRUE}
median(newTotalSteps$steps)
```

###After replacing the mean is the same but the median is a little bit different

##Q4 - Are there differences in activity patterns between weekdays and weekends?

###*Create a new factor variable in the dataset with two levels "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.*

```{r echo=TRUE}
newData$date <- as.Date(strptime(newData$date, format="%Y-%m-%d"))
newData$day <- weekdays(newData$date)
for(i in 1:nrow(newData)){
    if(newData[i,]$day %in% c("Saturday", "Sunday")){
        newData[i,]$day <- "Weekend"
    }
    else{
        newData[i,]$day <- "Weekday"
    }
}
```

###*Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.*

```{r echo=TRUE} 
stepsPerDay <- aggregate(newData$steps ~ newData$interval + newData$day, newData, mean)
names(stepsPerDay) <- c("interval", "day", "steps")
par(mfrow=c(1,1))  
with(stepsPerDay, plot(steps ~ interval, type="n", main="Weekday vs. Weekend Avg."))  
with(stepsPerDay[stepsPerDay$day == "Weekday",], lines(steps ~ interval, type="l", col="chocolate"))  
with(stepsPerDay[stepsPerDay$day == "Weekend",], lines(steps ~ interval, type="l", col="16" ))  
legend("topright", lty=c(1,1), col = c("chocolate", "16"), legend = c("Weekday", "Weekend"), seg.len=3)
```
