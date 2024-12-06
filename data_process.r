# Downloading and connecting packages
install.packages("tidyverse")
install.packages("lubridate")
library(tidyverse)
library(lubridate)
library(dplyr)

# Specify where the folder with files is located
setwd("D:/Data Analytics/Project/202312-202411_Cyclistic")

# Uploading files and merging them
files <- list.files(pattern = "*.csv")
data_list <- lapply(files, read_csv)
combined_data <- bind_rows(data_list)

# Save the file to the specified directory
write_csv(combined_data, "D:/Data Analytics/Project/combined_data.csv")

# Information about rows, columns and their data types
glimpse(combined_data)

# Remove duplicates
cleaned_data <- combined_data %>% distinct()

# Remove rows with missing values
cleaned_data <- cleaned_data %>% drop_na()

# time stamp processing
cleaned_data <- cleaned_data %>%
  mutate(start_time = ymd_hms(started_at), end_time = ymd_hms(ended_at))

# There were problems during conversion. There are strings in the data that could not be converted correctly to date and time format

problematic_rows_start <- cleaned_data %>% filter(is.na(ymd_hms(started_at))) 
problematic_rows_end <- cleaned_data %>% filter(is.na(ymd_hms(ended_at)))

print("Problematic start times:")
print(problematic_rows_start)
print("Problematic end times:")
print(problematic_rows_end)

# It was decided to remove them since their number is minimal and they should not have a global impact on the final result

# Remove rows with incorrect dates
cleaned_data <- cleaned_data %>% filter(!is.na(started_at) & !is.na(ended_at))
cleaned_data <- cleaned_data %>%
  filter(!is.na(ymd_hms(started_at)) & !is.na(ymd_hms(ended_at)))

# Re-converting the remaining data
cleaned_data <- cleaned_data %>%
  mutate(start_time = ymd_hms(started_at), end_time = ymd_hms(ended_at))


# Check if there are trips with negative duration and if there are, we delete them
cleaned_data <- cleaned_data %>% 
  filter(difftime(end_time, start_time, units = "mins") > 0)

# Make sure that the latitude and longitude are in the correct ranges
cleaned_data <- cleaned_data %>%
  filter(start_lat >= -90 &
         start_lat <= 90 &
         start_lng >= -180 &
         start_lng <= 180 &
         end_lat >= -90 &
         end_lat <= 90 &
         end_lng >= -180 &
         end_lng <= 180)

# Calculate the duration of the trips
cleaned_data <- cleaned_data %>%
   mutate(trip_duration = as.numeric(difftime(end_time, start_time, units = "mins")))

# Leave only valid trips that are longer than 1 minute and shorter than 1 day
cleaned_data <- cleaned_data %>% 
   filter(trip_duration >=1, trip_duration <= (24*60))


# Counts the number of missing values ​​in each column
colSums(is.na(cleaned_data))

# Remove unnecessary columns
cleaned_data <- cleaned_data %>% select(-started_at, -ended_at)

# Information about rows, columns and their data types
glimpse(cleaned_data)

# Save the file to the specified directory
write_csv(cleaned_data, "D:/Data Analytics/Project/cleaned_data.csv")