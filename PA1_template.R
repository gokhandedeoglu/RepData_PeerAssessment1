## 1.Code for reading in the dataset and/or processing the data
library("data.table")
library(ggplot2)

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'), method = "curl")
unzip("repdata%2Fdata%2Factivity.zip",exdir = "data")

activityDT <- data.table::fread(input = "data/activity.csv")

## 2.Histogram of the total number of steps taken each day
totalSteps <- activityDT[, c(lapply(.SD, sum, na.rm = FALSE)), .SDcols = c("steps"), by = .(date)] 

head(totalSteps, 10)

ggplot(totalSteps, aes(x = steps)) +
  geom_histogram(fill = "brown", binwidth = 1000) +
  labs(title = "Daily Steps", x = "Steps", y = "Frequency")
dev.copy(png,'01-dailySteps.png')
dev.off()
## 3.Mean and median number of steps taken each day
totalSteps[, .(meanSteps = mean(steps, na.rm = TRUE), medianSteps = median(steps, na.rm = TRUE))]

head(totalSteps)

## The average daily activity
intervalDT <- activityDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)] 

ggplot(intervalDT, aes(x = interval , y = steps)) + geom_line(color="blue", size=1) + labs(title = "Avg. Daily Steps", x = "Interval", y = "Avg. Steps per day")
dev.copy(png,'02-avgDailySteps.png')
dev.off()

intervalDT[steps == max(steps), .(max_interval = interval)]

## 6.Code to describe and show a strategy for imputing missing data
activityDT[is.na(steps), .N ]

activityDT[is.na(steps), "steps"] <- activityDT[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]


## 7.Histogram of the total number of steps taken each day after missing values are imputed
data.table::fwrite(x = activityDT, file = "data/tidyData.csv", quote = FALSE)

totalSteps <- activityDT[, c(lapply(.SD, sum)), .SDcols = c("steps"), by = .(date)] 

totalSteps[, .(meanSteps = mean(steps), medianSteps = median(steps))]

ggplot(totalSteps, aes(x = steps)) + geom_histogram(fill = "blue", binwidth = 1000) + labs(title = "Daily Steps", x = "Steps", y = "Frequency")
dev.copy(png,'03-correctedDailySteps.png')
dev.off()

## 8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
activityDT <- data.table::fread(input = "data/activity.csv")
activityDT[, date := as.POSIXct(date, format = "%Y-%m-%d")]
activityDT[, `Day of Week`:= weekdays(x = date)]
activityDT[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `Day of Week`), "weekday or weekend"] <- "weekday"
activityDT[grepl(pattern = "Saturday|Sunday", x = `Day of Week`), "weekday or weekend"] <- "weekend"
activityDT[, `weekday or weekend` := as.factor(`weekday or weekend`)]
head(activityDT, 10)

activityDT[is.na(steps), "steps"] <- activityDT[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
intervalDT <- activityDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `weekday or weekend`)] 

ggplot(intervalDT , aes(x = interval , y = steps, color=`weekday or weekend`)) + geom_line() + labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps") + facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2)
dev.copy(png,'04-avgDailyStepsByWeekType.png')
dev.off()