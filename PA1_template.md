---
title: "Course Project 1"
author: "Lionel Ngoubou"
date: "August 29, 2019"
output: html_document
keep_md : true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, cache = FALSE)
```

PART 1 : LOADING AND PREPROCESSING THE DATA
-------------------------------------------

We sart by checking our workspace, working directory and loading the necessary package
```{r check the environment}
ls()
getwd()
library(tidyverse)
```

We then download the data and unzip it
```{r load data}
if(!file.exists("data")) {
  dir.create("data")
}

fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileurl, destfile = "data//amd.zip")
unzip("data//amd.zip", exdir = "data")
dir("data")
```

Before reading the data, we perform a rough calculation to see if it fits in the computer memory.
The data has 3 variables and 17568 observations. We use the following formula
(ncol* nrow * 8) / 2^20
 
```{r memory calculation}
(3*17568*8) / 2^20
```
It takes roughly 400K of memory


Question 1 : Load the data

```{r data loading}
activity <- read.csv("data//activity.csv")
str(activity) # check the data structure
summary(activity) # view some useful statistics
head(activity) # preview the data
```

Question 2 : Process/transform the data (if necessary) into a format suitable for your analysis

```{r data processing}
activity$date <- as.Date(as.character(activity$date)) #convert the "date" variable in date format
str(activity); summary(activity); head(activity)
```
The data has correctly been processed.



PART 2 : WHAT IS MEAN TOTAL NUMBER OF STEPS TAKEN PER DAY?
----------------------------------------------------------
For this part of the assignment, you can ignore the missing values in the dataset.

Question 1 : Calculate the total number of steps taken per day

```{r steps total}
total_steps_day <- activity %>%
  group_by(date) %>%
  summarise(total = sum(steps))

str(total_steps_day); head(total_steps_day)
```


Question 2 : Make a histogram of the total number of steps taken each day.

```{r total steps histogram}
ggplot(total_steps_day, aes(total)) + geom_histogram(fill = "red", bins = 50) +
  ggtitle("Total Number of Steps per Day")
```


Question 3 :  Calculate and report the mean and median of the total number of steps taken per day

I got two possibilities. I can either use the summary function which display six
useful statistics, including the mean and the median, or I can use the mean
and median fucntions.

```{r mean and median of total number of steps}
summary(total_steps_day)
mean(total_steps_day$total, na.rm = TRUE)
median(total_steps_day$total, na.rm = TRUE)
```

Since the mean and the median are almost equals, we can conclude that the distribution is symmetric.



PART 3 : WHAT IS THE AVERAGE DAILY ACTIVITY PATTERN?
----------------------------------------------------

Question 1 :  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all days (y-axis)

```{r group the data by interval}
five_minutes <- aggregate(steps ~ interval, data = activity, FUN = mean, na.rm = TRUE)
```

```{r time series plot of the 5-minute interval and the average number of steps}
ggplot(five_minutes, aes(interval, steps)) + geom_line(color = "blue") +
  ggtitle("Average Number of Steps")
```


Question 2 : Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?

```{r maximum number of steps}
max_steps <- five_minutes %>%
  arrange(desc(steps))
max_steps[1,1]
```


PART 4 : IMPUTING MISSING VALUES
--------------------------------

Question 1 : Calculate and report the total number of missing values in the dataset

I can have this information with the summary function or by using the combination of the functions sum and is.na
It's preferable to use the latter cause we can have thousands of variables in some cases.

```{r total of missing values}
missing_values <- sum(is.na(activity))
missing_values
```


Question 2 : Devise a strategy for filling in all of the missing values in the dataset. 

The strategy does not need to be sophisticated.
For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```{r filling in missing values}
avg_interval <- tapply(activity$steps, activity$interval, mean, na.rm=TRUE, simplify = TRUE)

```


Question 3 : Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r new dataset without missing values}
activity_clean <- na.omit(activity)
str(activity_clean); summary(activity_clean); View(activity_clean)
```


Question 4 : Make a histogram of the total number of steps taken each day and Calculate and report the mean
and median total number of steps taken per day. Do these values differ from the estimates
from the first part of the assignment?
What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r total number of steps}
total_steps_day_clean <- activity_clean %>%
  group_by(date) %>%
  summarise(total = sum(steps))
```


```{r histogram of the total number of steps}
ggplot(total_steps_day_clean, aes(total)) + geom_histogram(color = "black") +
  ggtitle("Total Number of Steps (without missing values)")
```

These values do not differ from the estiamtes of the first part of the assignment.
Imputing missing values has no impact on the estimates of the total dayly number of steps.



PART 5 : ARE THERE DIFFERENCES IN ACTIVITY PATTERNS BETWEEN WEEKDAYS AND WEEKENDS?
--------------------------------------------------------------------------------

For this part the weekdays() function may be of some help here. 
Use the dataset with the filled-in missing values for this part.

Question 1 : Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
indicating whether a given date is a weekday or weekend day.

```{r list of names of days}
days <- weekdays(activity_clean$date) #list of the names of the days
activity_clean <- cbind(activity_clean, days) # add the list as a column in the dataframe
str(activity_clean)
```

Create a new variable indicating wether a date is a weekday or weekend.
My computer system is in french, hence the use of french days.

```{r create the new factor variable}
activity_clean <- activity_clean %>%
  mutate(day_type = ifelse(days == "samedi" | days == "dimanche", "weekend",
                           "weekday"))

activity_clean$days <- NULL # delete the days column
str(activity_clean)
```


Question 2 : Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
See the README file in the GitHub repository to see an example of what this plot should look like 
using simulated data.
```{r group data by interval}
fiveminutes2<- aggregate(steps ~ interval, data = activity_clean, FUN = mean, na.rm = TRUE)
head(fiveminutes2)
```


```{r panel plot of the time series}
ggplot(activity_clean, aes(x =interval , y=steps, color=day_type)) +
  geom_line() +
  ggtitle(" Average Number of Steps across Weekdays and Weekends") +
facet_wrap(~ day_type, ncol = 1, nrow=2)
```







