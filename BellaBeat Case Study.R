# How can a wellness company play it smart?

# Business Task: To analyze FitBit fitness tracker data in order to have proper insight on how consumers are using the FitBit applications and also to discover outlying trends in order to help with its marketing startegy. 

# First I will add the appropriate packets. 

install.packages("tidyverse")
library(tidyverse)
install.packages("here")
library(here)
install.packages("skimr")
library(skimr)
install.packages("janitor")
library(janitor)
install.packages("lubridate")
library(lubridate)

# Now I will look through the data

skim_without_charts(sleepDay_merged)
skim_without_charts(WeightLogInfo_merged)
skim_without_charts(DailyActivity_merged)

# To help with furthering my analyis, I will add more columns.

DailyActivity_merged %>%
  mutate(day_of_the_week = wday(mdy(ActivityDate), label = TRUE, abbr = FALSE),
         overall_active_hours = (VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes) / 60,
         sedentary_hours = SedentaryMinutes/60,
         date = mdy(ActivityDate))

DailyActivity_merged <- DailyActivity_merged %>%
  mutate(day_of_the_week = wday(mdy(ActivityDate), label = TRUE, abbr = FALSE),
         overall_active_hours = (VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes) / 60,
         sedentary_hours = SedentaryMinutes/60,
         date = mdy(ActivityDate))

glimpse(DailyActivity_merged)

sleepDay_merged %>%
  mutate(day_of_the_week = wday(mdy_hms(SleepDay), label = TRUE, abbr = FALSE),
         sum_hours_asleep = TotalMinutesAsleep/60,
         sum_hours_in_bed = TotalTimeInBed/60,
         date = mdy_hms(SleepDay))

sleepDay_merged <- sleepDay_merged %>%
  mutate(day_of_the_week = wday(mdy_hms(SleepDay), label = TRUE, abbr = FALSE),
         sum_hours_asleep = TotalMinutesAsleep/60,
         sum_hours_in_bed = TotalTimeInBed/60,
         date = mdy_hms(SleepDay))
glimpse(sleepDay_merged)

WeightLogInfo_merged %>%
  mutate(date = mdy_hms(Date), day_of_the_week = wday(mdy_hms(Date), label = TRUE, abbr = FALSE))

WeightLogInfo_merged <- WeightLogInfo_merged %>%
  mutate(date = mdy_hms(Date), day_of_the_week = wday(mdy_hms(Date), label = TRUE, abbr = FALSE))

glimpse(WeightLogInfo_merged)

# Lets analyize the data,
# With the added columns, I will create charts in order to see how the steps, active minutes, and calories burned against each day of the week.
# This will help me find out what days are the most active. 

ggplot(data = DailyActivity_merged) +
  geom_col(mapping = aes(x = day_of_the_week, y = TotalSteps), fill = "green") +
  labs(title = "Total Steps Taken Everyday")

ggplot(data = DailyActivity_merged) +
  geom_col(mapping = aes(x = day_of_the_week, y = VeryActiveMinutes), fill = "orange") +
  labs(title = "Very Active Minutes Everyday")

ggplot(data = DailyActivity_merged) +
  geom_col(mapping = aes(x = day_of_the_week, y = Calories), fill = "blue") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
  labs(title = "Calories Burned Everyday")

# Now I will see the correlation between the calories burned against the number of stpes taken.

ggplot(data = DailyActivity_merged) +
  geom_point(mapping = aes(x = TotalSteps, y = Calories), color = "red") +
  geom_smooth(mapping = aes(x = TotalSteps, y = Calories)) +
  labs(title = "Steps Taken To Calories Burned")

# Looking at the relationship, it is evident and self explanitory that wth the more steps you take the more calories are burned.
# But one things that is interesting is how most of the total steps are aroudn gthe 1000 range but starts to stagnate downarda with the few that reach 2000 steps plus.

ggplot(data = DailyActivity_merged) +
  geom_jitter(mapping = aes(x = TotalSteps, y = SedentaryMinutes), color = "darkorchid3") +
  geom_smooth(mapping = aes(x = TotalSteps, y = SedentaryMinutes))

# First, sedentary minutes would be the time that people lay down or arent moving as much,
# In this case we see a negative correlation, with the more steps taken we see more rest time in addition to it increasing overt time.

# Now I will look for a correlation between the calories intake to the active minutes spent using smart devices.

types_of_daily_activity <- DailyActivity_merged %>%
  pivot_longer(cols = ends_with("ActiveMinutes"),
               names_to = "ActiveType",
               values_to = "ActiveMinutes")

ggplot(data = types_of_daily_activity) +
  geom_point(mapping = aes(x = Calories, y = ActiveMinutes, color = ActiveType)) +
  geom_smooth(mapping = aes(x = Calories, y = ActiveMinutes), color = "deeppink2")

# What i observed in this relationship is that lightly active minute that range from 100- to 400 on a smart device have about 2k to 3k in calory intake.
# But what is interesting to see is how both failty active minutes and and very active monutes both share a very low monites but a massice 2k to 3k calory intake, less then double of the lightly active minutes spent.

# Now lets find out certain sleep patterns, let us analyze the sleep data.

ggplot(data = sleepDay_merged) +
  geom_col(mapping = aes(x = day_of_the_week, y = sum_hours_asleep, fill = day_of_the_week))

# Taking the sleep data, we will relate it back to daily activities. 

sleep_activity <- merge(DailyActivity_merged, sleepDay_merged,
                        by = c('Id', by.y = 'date'))

ggplot(data = sleep_activity) +
  geom_jitter(mapping = aes(x = sum_hours_asleep, y = TotalSteps), color = "green1") +
  geom_smooth(mapping = aes(x = sum_hours_asleep, y = TotalSteps))

#

weight_activity <- merge(DailyActivity_merged, WeightLogInfo_merged,
                         by = c('Id', by.y = 'date'))
ggplot(data = weight_activity) +
  geom_violin(mapping = aes(x = VeryActiveMinutes, y = WeightPounds), fill = "lightslateblue")
