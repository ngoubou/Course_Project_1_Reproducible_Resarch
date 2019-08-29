# PART 1 : LOADING AND PREPROCESSING THE DATA

# check the workspace and load the packages
ls(); rm(list = ls()); library(tidyverse)

# download the data

if(!file.exists("data")) {
  dir.create("data")
}

fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileurl, destfile = "data//amd.zip")
unzip("data//amd.zip", exdir = "data")
dir("data")


# the data has 3 variables and 17568 observations, normally it fits 
# but check it anyway with a rough calculation
(ncol*nrow*8) / 2^20
(3*17568*8) / 2^20 # it takes roughly 400K of memory

# Q1. load the data

activity <- read.csv("data//activity.csv")

str(activity) # check the dataframe

# Q2. Process/transform the data (if necessary) into a format suitable for your analysis

# convert the "date" variable in date format

activity$date <- as.Date(as.character(activity$date))
str(activity); summary(activity); View(activity); head(activity)



# PART 2 : WHAT IS MEAN TOTAL NUMBER OF STEPS TAKEN PER DAY?

# For this part of the assignment, you can ignore the missing values in the dataset.

# Q1. Calculate the total number of steps taken per day

total_steps_day <- activity %>%
  group_by(date) %>%
  summarise(total = sum(steps))

View(total_steps_day)

# Q2. Make a histogram of the total number of steps taken each day.

ggplot(total_steps_day, aes(total)) + geom_histogram(fill = "red", bins = 50)


# If you do not understand the difference between a histogram and a barplot,
# research the difference between them. 

# Q3. Calculate and report the mean and median of the total number of steps taken per day

# I got two possibilities. I can either use the summary function which display six
# useful statistics, including the mean and the median, or I can use the mean
# and median fucntions

summary(total_steps_day)
mean(total_steps_day$total, na.rm = TRUE); median(total_steps_day$total, na.rm = TRUE)
# answer : the mean is 100766.19 and the median is 10765
# since the mean and the median are almost equals, we can conclude that 
# the distribution is symmetric



# PART 3 : WHAT IS THE AVERAGE DAILY ACTIVITY PATTERN?

# Q1. Make a time series plot (i.e. type = "l") of the 5-minute interval 
# (x-axis) and the average number of steps taken, averaged across all days (y-axis)

five_minutes <- aggregate(steps ~ interval, data = activity, FUN = mean, na.rm = TRUE)

tt <- activity %>%
  group_by(interval, date) %>%
  summarise(avg = mean(steps, na.rm = TRUE))
  

# the following function produces the same result
aggregate(activity, by = list(activity$interval), FUN = mean, na.rm = TRUE)

# time series plot
ggplot(five_minutes, aes(interval, steps)) + geom_line(color = "blue")


# Q2. Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?

maxsteps <- five_minutes$interval[which.max(five_minutes$steps)]
maxsteps

max_steps <- five_minutes %>%
  arrange(desc(steps))
max_steps[1,1]

# the answer is: 835 interval



# PART 4 : IMPUTING MISSING VALUES

# Note that there are a number of days/intervals where there are missing values 
# (coded as NA). 
# The presence of missing days may introduce bias into some calculations or summaries of the data.

# Q1. Calculate and report the total number of missing values in the dataset

# I can have this information with the summary function or by using the combination
# of the functions sum and is.na
# It's preferable to use the latter cause we can have thousands of variables in some cases

summary(activity)
missing_values <- sum(is.na(activity))
missing_values
# the answer is 2304


# Q2. Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated.
# For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

# from what I understand, it's asking us to remove all the missing values in the dataset
# but it seems like the question below asks me the same thing. So I don't understand this question
avg_interval <- tapply(activity$steps, activity$interval, mean, na.rm=TRUE, simplify = TRUE)

# Q3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

activity_clean <- na.omit(activity)
str(activity_clean); summary(activity_clean); View(activity_clean)

# Q4. Make a histogram of the total number of steps taken each day and Calculate and report the mean
# and median total number of steps taken per day. Do these values differ from the estimates
# from the first part of the assignment?
# What is the impact of imputing missing data on the estimates of the total daily number of steps?


total_steps_day_clean <- activity_clean %>%
  group_by(date) %>%
  summarise(total = sum(steps))

# the following function produces the same result
ts <- aggregate(steps ~ date, data = activity_clean, FUN = sum, na.rm = TRUE)

str(total_steps_day_clean); summary(total_steps_day_clean)

ggplot(total_steps_day_clean, aes(total)) + geom_histogram(color = "black")

# these values do not differ from the estiamtes of the first part of the assignment.
# imputing missing values has no impact on the estimates of the total dayly number of steps.



# PART 5 : ARE THERE DIFFERENCES IN ACTIVITY PATTERNS BETWEEN WEEKDAYS AND WEEKENDS?

# For this part the weekdays() function may be of some help here. 
# Use the dataset with the filled-in missing values for this part.

# Q1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
# indicating whether a given date is a weekday or weekend day.

days <- weekdays(activity_clean$date)

activity_clean <- cbind(activity_clean, days)
str(activity_clean)

# my computer system is in french, hence the use of french days 

activity_clean <- activity_clean %>%
  mutate(day_type = ifelse(days == "samedi" | days == "dimanche", "weekend",
                           "weekday"))

activity_clean$days <- NULL
str(activity_clean)


# Q2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
# See the README file in the GitHub repository to see an example of what this plot should look like 
# using simulated data.

fiveminutes2<- aggregate(steps ~ interval, data = activity_clean, FUN = mean, na.rm = TRUE)
head(fiveminutes2)

ggplot(activity_clean, aes(x =interval , y=steps, color=day_type)) +
  geom_line() +
facet_wrap(~ day_type, ncol = 1, nrow=2)
