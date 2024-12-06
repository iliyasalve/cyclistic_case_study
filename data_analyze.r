# Downloading and connecting packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("scales")
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)

# Change local
Sys.setlocale("LC_TIME", "C")

# read data
data <- read_csv("D:/Data Analytics/Project/cleaned_data.csv")


# Part 1: Comparing the number of rides per month

# Add a column with the month
new_data <- data %>% 
  mutate(month = month(start_time, label = TRUE, abbr = FALSE))

# Obtain a table that indicates the number of rides for each month and user type (member/casual), sorted by month
monthly_rides <- new_data %>%
  group_by(month, member_casual) %>%
  summarise(count = n()) %>%
  arrange(month)

# Show result
print(monthly_rides)

# Create a schedule
ggplot(monthly_rides, aes(x = month, y = count, fill = member_casual)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(title = "Number of Rides per Month by User Type",
        x = "Month", y = "Number of Rides") +
   scale_y_continuous(labels = label_number(scale = 0.001, suffix = "k")) +
   scale_fill_manual(values = c("member" = "#00c3ff", "casual" = "#ff0090"), name = "Member Type") +
   theme_minimal()

# Save a schedule
ggsave("D:/Data Analytics/Project/monthly_rides_plot.png", width = 12, height = 6, units = "in")


# Part 2: Comparing bike usage on days of the week

# Add a column with the month
new_data <- new_data %>% mutate(day_of_week = wday(start_time, label = TRUE, abbr = FALSE))

# Obtain a table that indicates the number of rides for each day of the week and user type (member/casual), sorted by day of the week
weekly_rides <- new_data %>%
  group_by(day_of_week, member_casual) %>%
  summarise(count = n()) %>%
  arrange(day_of_week)

# Show result
print(weekly_rides)

# Create a schedule
ggplot(weekly_rides, aes(x = day_of_week, y = count, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Rides per Day of the Week by User Type", x = "Day of the Week", y = "Number of Rides") +
  scale_y_continuous(labels = label_number(scale = 0.001, suffix = "k")) +
  scale_fill_manual(values = c("member" = "#00c3ff", "casual" = "#ff0090"), name = "Member Type") +
  theme_minimal()

# Save a schedule
ggsave("D:/Data Analytics/Project/weekly_rides_plot.png", width = 12, height = 6, units = "in")


# Part 3: Comparing bike usage at different times of day
new_data <- new_data %>% mutate(hour = format(start_time, "%H"))

# Obtain a table that indicates the number of trips for each hour and each user type
hourly_rides <- new_data %>% 
  group_by(member_casual, hour) %>% 
  summarise(count = n())%>%
  arrange(hour)

# Show result
print(hourly_rides)

# Create a schedule
ggplot(hourly_rides, aes(x = hour, y = count, fill = member_casual)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Bike Usage by Hour of the Day", x = "Hour", y = "Number of Rides") + 
  scale_y_continuous(labels = label_number(scale = 0.001, suffix = "k")) + 
  scale_fill_manual(values = c("member" = "#00c3ff", "casual" = "#ff0090"), name = "User Type") + 
  theme_minimal()

# Save a schedule
ggsave("D:/Data Analytics/Project/hour_rides_plot.png", width = 12, height = 6, units = "in")


# Part 4: Average and Median trip length

# Group the data and calculate the average trip length and median
trip_duration_analysis <- new_data %>% 
  group_by(member_casual) %>% 
  summarise(avg_duration = mean(trip_duration, na.rm = TRUE), median_duration = median(trip_duration, na.rm = TRUE))

# Show result
print(trip_duration_analysis)

# Create a schedule
ggplot(trip_duration_analysis, aes(x = member_casual, y = avg_duration, fill = member_casual)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_hline(aes(yintercept = median_duration, color = member_casual), linetype = "dashed", size = 1) + 
  annotate("text", x = 1, y = trip_duration_analysis$median_duration[1], label = "Median", vjust = -0.5, color = "#0033cc") + 
  annotate("text", x = 2, y = trip_duration_analysis$median_duration[2], label = "Median", vjust = -0.5, color = "#cc0033") + 
  labs(title = "Average and Median Trip Duration by User Type", x = "User Type", y = "Duration (minutes)") + 
  scale_fill_manual(values = c("member" = "#00c3ff", "casual" = "#ff0090"), name = "User Type") + 
  scale_color_manual(values = c("member" = "#0033cc", "casual" = "#cc0033"), name = "User Type") + 
  theme_minimal() + 
  guides(color = guide_legend("Median Duration (dashed)"), fill = guide_legend("Average Duration (bars)"))

# Save a schedule
ggsave("D:/Data Analytics/Project/average_and_median_trip_plot.png", width = 12, height = 6, units = "in")


# Part 5: Top 5 starting and ending stations

# Obtain a table that will list the number of trips for each starting station and user type "member/casual", sorted by descending number of records
popular_start_stations_by_group <- new_data %>% 
  group_by(start_station_name, member_casual) %>% 
  summarise(count = n(), .groups = 'drop') %>% 
  arrange(desc(count))

# Save top 5
top_start_stations <- popular_start_stations_by_group %>% 
   group_by(member_casual) %>% 
   top_n(5, wt = count) %>% 
   ungroup()

# Create a schedule
ggplot(top_start_stations, aes(x = reorder(start_station_name, count), y = count, fill = member_casual)) + 
  geom_bar(stat = "identity", position = "dodge") + coord_flip() + 
  labs(title = "Top 5 Popular Start Stations by User Type", x = "Start Station", y = "Number of Rides") + 
  scale_fill_manual(values = c("member" = "#00c3ff", "casual" = "#ff0090"), name = "User Type") + 
  theme_minimal() +
  facet_wrap(~member_casual, scales = "free_y")

# Save a schedule
ggsave("D:/Data Analytics/Project/top_5_start_stations.png", width = 12, height = 6, units = "in")


# Obtain a table that will list the number of trips for each ending station and user type "member/casual", sorted by descending number of records
popular_end_stations_by_group <- new_data %>% 
  group_by(end_station_name, member_casual) %>% 
  summarise(count = n(), .groups = 'drop') %>% 
  arrange(desc(count))

# Save top 5
top_end_stations <- popular_end_stations_by_group %>% 
   group_by(member_casual) %>% 
   top_n(5, wt = count) %>% 
   ungroup()

# Create a schedule
ggplot(top_end_stations, aes(x = reorder(end_station_name, count), y = count, fill = member_casual)) + 
  geom_bar(stat = "identity", position = "dodge") + coord_flip() + 
  labs(title = "Top 5 Popular End Stations by User Type", x = "End Station", y = "Number of Rides") + 
  scale_fill_manual(values = c("member" = "#00c3ff", "casual" = "#ff0090"), name = "User Type") + 
  theme_minimal() +
  facet_wrap(~member_casual, scales = "free_y")

# Save a schedule
ggsave("D:/Data Analytics/Project/top_5_end_stations.png", width = 12, height = 6, units = "in")


# Part 6: Analyzing the rate of return to the starting station

# Analyze the rate of return to the starting station by user type
return_analysis <- new_data %>% group_by(member_casual, same_start_end) %>% summarise(count = n(), .groups = 'drop') %>% mutate(percentage = count / sum(count) * 100)

# Create a function for schedule
create_pie_chart <- function(new_data, member_casual) { 
    ggplot(new_data, aes(x = " ", y = percentage, fill = same_start_end)) + 
       geom_bar(width = 1, stat = "identity") + coord_polar("y") + 
       labs(title = paste("Return Rate for", member_casual), fill = "Returned to Start") + 
       scale_fill_manual(values = c("TRUE" = "#00c3ff", "FALSE" = "#ff0090"), labels = c("No", "Yes")) + 
       theme_minimal() + 
       theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
           axis.text.x = element_blank(), axis.text.y = element_blank(), 
           panel.grid = element_blank(), axis.ticks = element_blank(), 
           panel.background = element_rect(fill = "transparent", color = NA), plot.background = element_rect(fill = "transparent", color = NA))}

# Create and merge schedule
pie_chart_member <- create_pie_chart(return_analysis %>% filter(member_casual == "member"), "Members")
pie_chart_casual <- create_pie_chart(return_analysis %>% filter(member_casual == "casual"), "Casual Riders")
combined_plot <- grid.arrange(pie_chart_member, pie_chart_casual, ncol = 2)

# Save a schedule
ggsave(filename = "D:/Data Analytics/Project/return_rate_pie_charts.png", plot = combined_plot, width = 12, height = 6, units = "in", bg = "transparent")


# Save the file to the specified directory
write_csv(new_data, "D:/Data Analytics/Project/analyze_data.csv")