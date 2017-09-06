---
title: "Reproducible Research - Project 1"
author: "Prasath"
date: "September 6, 2017"
---

## Introduction

This project provides analysis of activity monitoring devices using [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) dataset.

The variables included in this dataset are:
1. steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
2. date: The date on which the measurement was taken in YYYY-MM-DD format
3. interval: Identifier for the 5-minute interval in which measurement was taken

Analysis includes following sections

1. Loading and preprocessing the data
2. What is mean total number of steps taken per day?
3. What is the average daily activity pattern?
4. Imputing missing values
5. Are there differences in activity patterns between weekdays and weekends?

###Loading and preprocessing the data
This section provides datails on importing dataset and preprocessing the data.

Unzip data to obtain a csv file.
```r
unzip("activity.zip",exdir = "data")
```
Reading the data into activity data frame and show some summary statistics

Using "readr" package import downloaded activity dataset.
```{r, echo=TRUE}
library(readr)
activity <- read_csv("data/activity.csv")
head(activity)
```

Validate all the datatype
```{r, echo=TRUE}
str(activity)
```

###What is mean total number of steps taken per day?
For this section, we are ignoring the missing values in the dataset
1. Calculating the total number of steps taken per day
```{r, echo=TRUE}
library(dplyr)
stepstakenperday <- activity %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print
```
2. Histogram of the total number of steps taken each day
```{r, echo=TRUE}
library(ggplot2)
ggplot(stepstakenperday, aes(x = steps)) +
  geom_histogram(fill = "blue", binwidth = 1000) +
  labs(title = "Histogram of the total number of steps taken each day", x = "Steps per day", y = "Frequency")
```
3. Calculating the mean and median of the total number of steps taken per day
```{r, echo=TRUE}
mean_steps_per_day <- mean(stepstakenperday$steps, na.rm = TRUE)
median_steps_per_day <- median(stepstakenperday$steps, na.rm = TRUE)
paste("Mean steps per day:", mean_steps_per_day)
paste("median steps per day:" , median_steps_per_day)
```

###What is the average daily activity pattern?

Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r, echo= TRUE}
averagedailyactivitypattern <- activity %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = sum(steps)) %>%
  print

ggplot(averagedailyactivitypattern, aes(x=interval, y=steps)) +
  geom_line(color = "blue")+
  labs(title = "Average daily activity pattern", x = "5 minutes intervals", y = "Steps")
```
Display 5-minute interval, on average across all the days in the dataset, containing the maximum number of steps.
```{r, echo= TRUE}
IntervalWithMaxSteps <- filter(averagedailyactivitypattern,averagedailyactivitypattern$steps == max(averagedailyactivitypattern$steps) ) 
paste("Interval with Maximum number of steps:", IntervalWithMaxSteps$interval)
```

###Imputing missing values
There are `r sum(is.na(activity$steps)) ` missing values in dataset.
Creating new dataset and using "mean for that 5-minute interval" to update the missing values.

Create dataset without nulls and get mean steps for each interval
```{r, echo= TRUE}
activity_data_without_na <- filter(activity,is.na(activity$steps) == FALSE)
average_steps_per_interval <- activity_data_without_na %>%
  group_by(interval) %>%
  summarize(steps = mean(steps)) 
```
Merge original dataset "activity" with dataset "interval with mean steps"
```{r, echo= TRUE}
mergedactivty <- merge(activity,average_steps_per_interval, by ="interval")
```
Repleace NA with mean steps
```{r, echo= TRUE}
mergedactivty <- mergedactivty %>%
  mutate(steps = ifelse(is.na(steps.x),steps.y,steps.x))  %>%
  select(steps,date,interval)
head(mergedactivty)
```
Mergedactivity dataset contains  `r sum(is.na(mergedactivty$steps)) ` missing values.

histogram of the total number of steps taken each day.
```{r, echo= TRUE}

stepstakenperday2 <- mergedactivty %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print

library(ggplot2)
ggplot(stepstakenperday2, aes(x = steps)) +
  geom_histogram(fill = "blue", binwidth = 1000) +
  labs(title = "Histogram of the total number of steps taken each day", x = "Steps per day", y = "Frequency")
```

Calculate and report the mean and median total number of steps taken per day.
```{r, echo= TRUE}
mean_steps_per_day2 <- mean(stepstakenperday2$steps, na.rm = TRUE)
median_steps_per_day2 <- median(stepstakenperday2$steps, na.rm = TRUE)
paste("Original Mean steps per day:", mean_steps_per_day)
paste("Original median steps per day:" , median_steps_per_day)
paste("Mean steps per day:", mean_steps_per_day2)
paste("median steps per day:" , median_steps_per_day2)
```

###Are there differences in activity patterns between weekdays and weekends?

To check the difference, first, lets add column to the dataset with week day flag
```{r, echo= TRUE}
library(lubridate)
mergedactivty <- mergedactivty %>%
  mutate(weekdayflag = ifelse(wday(date) %in% c(1,7),'Weekends','Weekdays'))  
```

Plot the daily activity pattern with week day flag
```{r, echo= TRUE}
averagedailyactivitypattern <- mergedactivty %>%
  filter(!is.na(steps)) %>%
  group_by(interval,weekdayflag) %>%
  summarize(steps = mean(steps)) %>%
  print

ggplot(averagedailyactivitypattern, aes(x=interval, y=steps, color = weekdayflag)) +
  geom_line()
```