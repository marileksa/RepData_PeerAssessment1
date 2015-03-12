---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Loading and preprocessing the data


```r
#cleanup the environment, set working directory to where the data file resides
rm(list=ls(all=TRUE)) 
setwd("~/RepData_PeerAssessment1/")

#load libraries
library(knitr)
library(ggplot2)
library(lubridate)
library(scales)
library(grid)

# load data and check
fulldf <- read.csv("activity.csv", stringsAsFactors = FALSE)
str(fulldf)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
# remove 'steps == NA' from the set and check
# note: if you do 'aggregate(...., na.rm=TRUE)' same result will be achieved: 53 observations to plot
df <- subset(fulldf, !is.na(steps))
str(df)
```

```
## 'data.frame':	15264 obs. of  3 variables:
##  $ steps   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : chr  "2012-10-02" "2012-10-02" "2012-10-02" "2012-10-02" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
#group by date, calculate totals, set column names and check 
library(xtable)
result <- aggregate(steps ~ date , data=df, FUN=sum)
colnames(result) <- c("date", "total_steps")
str(result)
```

```
## 'data.frame':	53 obs. of  2 variables:
##  $ date       : chr  "2012-10-02" "2012-10-03" "2012-10-04" "2012-10-05" ...
##  $ total_steps: int  126 11352 12116 13294 15420 11015 12811 9900 10304 17382 ...
```

```r
xt <- xtable(summary(result), type = html)
print(xt)
```

```
## % latex table generated in R 3.1.2 by xtable 1.7-4 package
## % Thu Mar 12 16:46:44 2015
## \begin{table}[ht]
## \centering
## \begin{tabular}{rll}
##   \hline
##  &     date &  total\_steps \\ 
##   \hline
## 1 & Length:53          & Min.   :   41   \\ 
##   2 & Class :character   & 1st Qu.: 8841   \\ 
##   3 & Mode  :character   & Median :10765   \\ 
##   4 &  & Mean   :10766   \\ 
##   5 &  & 3rd Qu.:13294   \\ 
##   6 &  & Max.   :21194   \\ 
##    \hline
## \end{tabular}
## \end{table}
```

## What is mean total number of steps taken per day?



```r
# report the mean and median total number of steps taken per day
mean_steps <- as.integer(round(mean(result$total_steps)))
mean_steps
```

```
## [1] 10766
```

```r
median_steps <- as.integer(median(result$total_steps))
median_steps
```

```
## [1] 10765
```

generate histogram

```r
#note: since mean and median too close there is only one line visible on the plot

m <- ggplot(result, aes(x=total_steps))
m + geom_histogram(binwidth=1500, aes(fill = ..count..)) +
        geom_vline(aes(xintercept=median_steps, colour='black')) +
        geom_vline(aes(xintercept=mean_steps, colour='blue')) +     
        scale_fill_gradient("count", low = "green", high = "red") +
        labs(x = "total steps per day") + labs(y = "count") + labs(title = "2 months of data ( excluding NA values )")
```

![plot of chunk histogram1](figure/histogram1-1.png) 

## What is the average daily activity pattern?


```r
#group by interval, calculate average steps and check
result <- aggregate(steps ~ interval , data=df, FUN=mean)
colnames(result) <- c("interval", "mean_steps")
str(result)
```

```
## 'data.frame':	288 obs. of  2 variables:
##  $ interval  : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ mean_steps: num  1.717 0.3396 0.1321 0.1509 0.0755 ...
```

```r
#set to sequence of consecutive integers to plot correctly
#knowing that interval is sorted
result$seq_interval <- 1:288
colnames(result) <- c("interval", "mean_steps", "seq_interval")


#scale claculation for x-axis: 288 intervals / 24 hours = 12 intervals in one hour
#if we want to plot "3:00","8:00","13:00","18:00","23:00" then
#the corresponding ints will be 36,96,156,216,276 respectively

#finding 5-minute interval that contains the maximum number of steps
result[result$mean_steps == max(result$mean_steps),]
```

```
##     interval mean_steps seq_interval
## 104      835   206.1698          104
```

generate a plot using the base system

```r
with(result, {
        par(mar=c(5,5,4,2))
        plot(seq_interval, mean_steps, xlab = "", ylab = "", type = "l", axes=FALSE, main="Average steps per interval")
        axis(side=1, at = c(36,96,156,216,276), labels = c("3:00","8:00","13:00","18:00","23:00"))
        axis(side=2, at = c(0,50,100,150,200), las=2)
        box()
        mtext("average steps",side=2,line=4)
        mtext("5-min intervals",side=1,line=3)
        
        text(156, 206, "max steps: 206.17 at 8:35", col="red")
})
```

![plot of chunk plot1](figure/plot1-1.png) 


## Imputing missing values

```r
# EX: to select missing values for interval=0 we could use:
fulldf[fulldf$interval == 0 & is.na(fulldf$steps),]
```

```
##       steps       date interval
## 1        NA 2012-10-01        0
## 2017     NA 2012-10-08        0
## 8929     NA 2012-11-01        0
## 9793     NA 2012-11-04        0
## 11233    NA 2012-11-09        0
## 11521    NA 2012-11-10        0
## 12673    NA 2012-11-14        0
## 17281    NA 2012-11-30        0
```

I have decided to replace missing values in steps with the mean for that 5-minute interval:
logic will loop through interval and set fulldf$steps with NA to the mean and check the data

```r
for (i in result$interval ) {
        index <- is.na(fulldf$steps) & fulldf$interval == i
        fulldf$steps[index] <- result[result$interval == i,2]        
}        
all(colSums(is.na(fulldf)) == 0)
```

```
## [1] TRUE
```


group the initial ( enhanced ) data set by date, calculate total steps, set column names and check

```r
enhanced_result <- aggregate(steps ~ date , data=fulldf, FUN=sum)
colnames(enhanced_result) <- c("date", "total_steps")
str(enhanced_result)
```

```
## 'data.frame':	61 obs. of  2 variables:
##  $ date       : chr  "2012-10-01" "2012-10-02" "2012-10-03" "2012-10-04" ...
##  $ total_steps: num  10766 126 11352 12116 13294 ...
```

generate histogram

```r
m <- ggplot(enhanced_result, aes(x=total_steps))
m + geom_histogram(binwidth=1500, aes(fill = ..count..)) +
        scale_fill_gradient("count", low = "green", high = "red") +
        labs(x = "total steps per day") + 
        labs(y = "count") + 
        labs(title = "2 months of data ( NA replaced by mean interval )")
```

![plot of chunk another_histogram](figure/another_histogram-1.png) 

## Are there differences in activity patterns between weekdays and weekends?



```r
# clenup variables
rm(index)
rm(result)

# generate index and set new factor() variable: day_type
index <- weekdays(ymd(fulldf$date)) == "Sunday" | weekdays(ymd(fulldf$date)) == "Saturday"
fulldf$day_type[index] <- "weekend"   
fulldf$day_type[!index] <- "weekday"

fulldf$day_type <- as.factor(fulldf$day_type)

# group by daty_type and interval, calculate the average
result <- aggregate(steps ~ day_type * interval, data=fulldf, FUN=mean)
colnames(result) <- c("day_type", "interval", "mean_steps")
result$mean_steps <- round(result$mean_steps, digits=2)

#since the interval is sorted we could create a repeating sequence and set $seq_interval
result$seq_interval <- rep(seq(1:288), each=2)      
head(result)
```

```
##   day_type interval mean_steps seq_interval
## 1  weekday        0       2.25            1
## 2  weekend        0       0.21            1
## 3  weekday        5       0.45            2
## 4  weekend        5       0.04            2
## 5  weekday       10       0.17            3
## 6  weekend       10       0.02            3
```

```r
# generate subsets for setting color on separate panels
wday <- subset(result, day_type == "weekday")
wend <- subset(result, day_type == "weekend")
```

generate a plot using the ggplot2 system 

```r
p <- ggplot(result, aes(x=seq_interval, y=mean_steps))
p + facet_wrap(~ day_type, nrow = 2, ncol = 1) +
        geom_line(data = wday, colour = "red", size = 1) +
        geom_line(data = wend, colour = "blue", size = 1) +

        theme(panel.background = element_rect(fill = 'gray'), 
              plot.margin=unit(c(4,4,6,4),"mm"),
              strip.text.x = element_text(size=12, face="bold")) +     

        labs(x = "5-min intervals") + labs(y = "average steps") + labs(title = "Average steps per interval") +       
        scale_x_continuous(breaks=c(36,96,156,216,276), labels = c("3:00","8:00","13:00","18:00","23:00"))
```

![plot of chunk another_plot](figure/another_plot-1.png) 
