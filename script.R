# set working dir
setwd("~/Documents/DataScience/5. Reproducible Research/ReproducibleResearch-Assignment1")

# load library
library(ggplot2)

### Loading and preprocessing the data
xo <- read.csv("./data/activity.csv")
x <- na.omit(xo)

### What is mean total number of steps taken per day?

sdsum  <- aggregate(x$steps,list(x$date), sum)
ggplot(sdsum, aes(x=x)) + 
    geom_histogram(fill = "tomato", breaks=seq(0,20000,by=1000)) + 
    ylim(0,18) +
    labs(title = "Histogram of Total Number of Steps Taken Each Day", x = "Interval", y = "Total number of steps")

mean(sdsum$x)
median(sdsum$x)

### What is the average daily activity pattern?

intmean <- aggregate(x$steps,list(x$interval), mean)
ggplot(intmean, aes(x=Group.1, y=x)) + 
    geom_line(col="steelblue") +
    labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")


intmean[intmean$x == max(intmean$x), ]

### Inputing missing values
sum(is.na(xo))

fillna <- function(steps,interval) {
    if (is.na(steps)) { intmean[intmean$Group.1 == interval, 2] }
    else { steps }
}
xofill <- xo
xofill$steps <- mapply(fillna, steps=xo$steps, interval=xo$interval)

xofillmean <- aggregate(xofill$steps,list(xofill$date), sum)
ggplot(xofillmean, aes(x=x)) + 
    geom_histogram(fill = "steelblue", alpha=0.9, breaks=seq(0,20000,by=1000), width=0.5) + 
    geom_freqpoly(data=sdsum,aes(x=x),color="tomato",alpha=0.6, lwd=1, breaks=seq(0,20000,by=1000)) +
    ylim(0,18) +
    labs(title = "Histogram of Total Number of Steps Taken Each Day (no missing data)", x = "Date", y = "Total number of steps")

mean(xofill$steps)
mean(x$steps)

median(xofill$steps)
median(x$steps)

### Are there differences in activity patterns between weekdays and weekends?

x$day <- weekdays(as.Date(x$date))
x$weekday <- ifelse(x$day == "Saturday" | x$day == "Sunday","Weekend","Weekday")

alt <- aggregate(x$steps,list(x$interval, x$weekday), mean)
weekmean <- aggregate(alt$x,list(alt$Group.2),mean)
ggplot(alt, aes(x=Group.1,y=x)) +
    geom_line(aes(col=alt$Group.2)) + 
    facet_wrap(~Group.2, nrow=2) +
    labs(title = "Total Number of Steps per 5-second interval", x = "Time Interval", y = "Total number of steps") +
    theme(legend.position="none")