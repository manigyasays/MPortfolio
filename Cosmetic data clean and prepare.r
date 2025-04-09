# data Manipulation library
library(dplyr)
# time Manipulation library
library(lubridate)

# Read the CSV file
cosmetics <- read.csv("cosmetics.csv")

#  Convert 'event_time' to a Date-Time format
cosmetics$event_time <- ymd_hms(cosmetics$event_time)

# Remove duplicates based on all columns except the 'event_time' column
cosmetics_clean <- cosmetics %>%
  arrange(event_time) %>%
  distinct(across(-event_time), .keep_all = TRUE)

# separate the 'event_time' column into date and time components
cosmetics_clean <- cosmetics_clean %>%
  mutate(
    # Weekday or Weekend
    day_type = ifelse(wday(event_time) %in% c(1, 7), "Weekend", "Weekday"),
    
    # Day of the week 
    day_of_week = weekdays(event_time),
    
    # Part of the day (Morning, Afternoon, Evening, Night)
    time_of_day = case_when(
      hour(event_time) >= 5 & hour(event_time) < 12 ~ "Morning",
      hour(event_time) >= 12 & hour(event_time) < 17 ~ "Afternoon",
      hour(event_time) >= 17 & hour(event_time) < 21 ~ "Evening",
      TRUE ~ "Night"
    )
  )


write.csv(cosmetics_clean, "cosmetics_cleaned.csv", row.names = FALSE)


head(cosmetics_clean)