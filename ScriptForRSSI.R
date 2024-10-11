# ONLY RUN THIS IF YOU DONT HAVE THESE PACKAGES INSTALLED
# install.packages("plotly")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("car")
# install.packages("babynames")
# install.packages("gapminder")
# install.packages("lubridate")

library(plotly)
library(dplyr)
library(carData)
library(gapminder)
library(babynames)
library(lubridate)


# set the name of the file here
dataset_name_single <- "rfid_2024-10-09_1-21-41[1].csv"

# Read the data
data_single <- read.csv(paste0("/Users/Yarra/Downloads/", dataset_name_single))

# Extract milliseconds from the timestamp and convert to numeric
data_single$milliseconds <- as.numeric(sub(".*\\.(\\d+)", "\\1", data_single$timestamp))

# Parse the timestamp including milliseconds
data_single$timestamp <- ymd_hms(sub("\\.\\d+", "", data_single$timestamp), tz = "UTC") + 
  milliseconds(data_single$milliseconds)

# Calculate elapsed time in milliseconds from the first timestamp
data_single <- data_single %>%
  mutate(elapsed_time_ms = as.numeric(difftime(timestamp, min(timestamp), units = "secs")) * 1000)

# Filter out rows where 'count' is greater than 0 (if needed)
# Assuming there is a 'count' column in the data
data_single <- data_single %>%
  filter(is.na(count) | count == 0)

# use this if you want to delete the first row
# data_single = data_single[-1,]

# use this if you want to only grab the first n rows
# data_single <- head(data_single, 24)

# Create the scatter plot with a line connecting the points
# * this is using ggplot with the x axis being the time and y being the rssi
# * then it uses geom point to add one dot per data point
# * geom line will make a line between every two points
# * labs is just the label for the plot
# * theme minimal is one of the ways ggplot will format the plot. 
scatter_plot <- ggplot(data_single, aes(x=elapsed_time_ms, y=rssi)) + 
  geom_point(size=3, color="#DC6423") + 
  geom_line(color="#239BDC", lineend="round") +
  labs(x = "Elapsed Time (ms)", y = "RSSI", title = "Graph of RSSI values over time for one tag", 
       subtitle = paste("Dataset:", dataset_name_single)) +
  theme_minimal() 

# Display the plot
scatter_plot

# Interactive plot display
ggplotly(scatter_plot)

#------------------------------------ data_bar_graph & relative frequancy

# set the name of the file here
dataset_name_bar_graph <- "multitag.csv"

# Read the data
data_bar_graph <- read.csv(paste0("/Users/Yarra/Downloads/", dataset_name_bar_graph))

# Extract milliseconds from the timestamp and convert to numeric
data_bar_graph$milliseconds <- as.numeric(sub(".*\\.(\\d+)", "\\1", data_bar_graph$timestamp))

# Parse the timestamp including milliseconds
data_bar_graph$timestamp <- ymd_hms(sub("\\.\\d+", "", data_bar_graph$timestamp), tz = "UTC") + 
  milliseconds(data_bar_graph$milliseconds)

# Calculate elapsed time in milliseconds from the first timestamp
data_bar_graph <- data_bar_graph %>%
  mutate(elapsed_time_ms = as.numeric(difftime(timestamp, min(timestamp), units = "secs")) * 1000)

# Filter out rows where 'count' is greater than 0 (if needed)
# Assuming there is a 'count' column in the data
data_bar_graph <- data_bar_graph %>%
  filter(is.na(count) | count == 0)

mutated_data <- data_bar_graph %>%
  mutate(time_interval = floor(elapsed_time_ms / 500) * 500) %>%
  group_by(epc, time_interval) %>%
  summarise(count_occurances = n(), .groups = 'drop')

mutated_data <- mutated_data %>%
  filter(epc != "")

df_grouped <- mutated_data %>%
  group_by(time_interval) %>%  
  mutate(total_count_per_interval = sum(count_occurances)) %>% 
  ungroup() %>%
  mutate(relative_frequency = count_occurances / total_count_per_interval)

relative_frequancy <- ggplot(df_grouped, aes(x = time_interval, y = relative_frequency, fill = factor(epc))) +
  geom_bar(stat = "identity", alpha = 0.5, position = 'dodge') + 
  scale_fill_discrete(name = "epc") + 
  scale_y_continuous(labels = scales::percent) + 
  labs(
    x = "Elapsed Time Interval (ms)", 
    y = "Relative Frequency (%)", 
    title = "Graph of relative reading count frequancy over time", 
    subtitle = paste("Dataset:", dataset_name_bar_graph)
  ) +  
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(df_grouped$time_interval), max(df_grouped$time_interval), by = 500)) 

# this shows the subtitle 
relative_frequancy

# this won't have subtitle but is interactive
ggplotly(relative_frequancy)

bar <- ggplot(mutated_data, aes(x = time_interval, y = count_occurances, fill = factor(epc))) +
  geom_bar(stat = "identity", alpha = 0.5, position = 'dodge') +  # Bar plot with side-by-side bars for each EPC
  scale_fill_discrete(name = "epc") + 
  labs(
    x = "Elapsed Time Interval (ms)", 
    y = "Raw Occurrence Count", 
    title = "Graph of reading counts over time for multiple tags", 
    subtitle = paste("Dataset:", dataset_name_bar_graph)
  ) + 
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(mutated_data$time_interval), max(mutated_data$time_interval), by = 500))

# this shows the subtitle 
bar

# this won't have subtitle but is interactive
ggplotly(bar)

# ------------------------------------------------------------- multi_line_graph

# set the name of the file here
dataset_name_multi <- "multitag.csv"

# Read the data
data_multi <- read.csv(paste0("/Users/Yarra/Downloads/", dataset_name_multi))

# Extract milliseconds from the timestamp and convert to numeric
data_multi$milliseconds <- as.numeric(sub(".*\\.(\\d+)", "\\1", data_multi$timestamp))

# Parse the timestamp including milliseconds
data_multi$timestamp <- ymd_hms(sub("\\.\\d+", "", data_multi$timestamp), tz = "UTC") + 
  milliseconds(data_multi$milliseconds)

# Calculate elapsed time in milliseconds from the first timestamp
data_multi <- data_multi %>%
  mutate(elapsed_time_ms = as.numeric(difftime(timestamp, min(timestamp), units = "secs")) * 1000)

# Filter out rows where 'count' is greater than 0 (if needed)
data_multi <- data_multi %>%
  filter(is.na(count) | count == 0)

# use this if you want to delete the first row
# data_multi = data_multi[-1,]

# use this if you want to only grab the first n rows
# data_multi <- head(data_multi, 24)

# Create the scatter plot with a line connecting the points
multi_line_graph <- ggplot(data_multi, aes(x=elapsed_time_ms, y=rssi)) + 
  geom_point(aes(colour = factor(epc)), size=3) + 
  geom_line(aes(colour = factor(epc)), lineend="round") +
  labs(
    x = "Elapsed Time (ms)", 
    y = "RSSI", 
    title = "Graph of RSSI values over time for multiple tags", 
    subtitle = paste("Dataset:", dataset_name_multi)
  ) +
  theme_minimal() 

# normal graph with subtitle
multi_line_graph

# Convert to interactive plotly graph
ggplotly(multi_line_graph)

