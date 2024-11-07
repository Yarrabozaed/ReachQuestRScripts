# ONLY RUN THIS IF YOU DONT HAVE THESE PACKAGES INSTALLED
# install.packages("plotly")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("car")
# install.packages("babynames")
# install.packages("gapminder")
# install.packages("lubridate")
# install.packages("signal")
# install.packages("zoo")

library(plotly)
library(dplyr)
library(carData)
library(gapminder)
library(babynames)
library(lubridate)


# set the name of the file here
dataset_name_single <- "12.4-rfid_2024-10-22_0-04-10.csv"

dataset_name_bar_graph <- dataset_name_single

dataset_name_multi <- dataset_name_single

dataset_name_multi_timestamp <- dataset_name_single

dataset_rolling_avg <- dataset_name_single

# Read the data
data_single <- read.csv(paste0("/Users/Yarra/Downloads/experiment_data/", dataset_name_single))

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
  labs(x = "Elapsed Time (ms)", y = "RSSI", 
       title = paste("Graph of RSSI values over time for one tag. Data: ", dataset_name_multi)) +
  theme_minimal() + 
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # White background
    plot.background = element_rect(fill = "lightgray", color = NA)  # Light gray background for the entire plot
  )

# Display the plot
scatter_plot

# ggsave(
#  filename = "rssi_over_time_for_one_tag.png",  
#  plot = scatter_plot,                        
#  width = 10,                           
#  height = 7,
#  dpi = 300 
#)

# Interactive plot display
ggplotly(scatter_plot)

#------------------------------------ data_bar_graph & relative frequancy

# set the name of the file here
# dataset_name_bar_graph <- "multitag.csv"

# Read the data
data_bar_graph <- read.csv(paste0("/Users/Yarra/Downloads/experiment_data/", dataset_name_bar_graph))

zzzzz <- data_bar_graph

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
  mutate(time_interval = floor(elapsed_time_ms / 100) * 100) %>%
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
    title = paste("Graph of relative reading count frequancy over time. Data: ", dataset_name_multi)
  ) +  
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(df_grouped$time_interval), max(df_grouped$time_interval), by = 100)) + 
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # White background
    plot.background = element_rect(fill = "lightgray", color = NA)  # Light gray background for the entire plot
  )

# this shows the subtitle 
relative_frequancy

ggsave(
  filename = "relative_frequancy_bar_graph.png",  
  plot = relative_frequancy,                        
  width = 10,                           
  height = 7,
  dpi = 300 
)

# this won't have subtitle but is interactive
ggplotly(relative_frequancy)

bar <- ggplot(mutated_data, aes(x = time_interval, y = count_occurances, fill = factor(epc))) +
  geom_bar(stat = "identity", alpha = 0.5, position = 'dodge') +  # Bar plot with side-by-side bars for each EPC
  scale_fill_discrete(name = "epc") + 
  labs(
    x = "Elapsed Time Interval (ms)", 
    y = "Raw Occurrence Count", 
    title = paste("Graph of reading counts over time for multiple tags. Data: ", dataset_name_multi)
  ) + 
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(mutated_data$time_interval), max(mutated_data$time_interval), by = 100)) + 
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # White background
    plot.background = element_rect(fill = "lightgray", color = NA),  # Light gray background for the entire plot
    axis.text.x = element_text(angle = 90, vjust = 0.5)  # Make x-axis labels vertical
  ) 

bar


ggsave(
  filename = "occurance_count_bar_graph.png",  
  plot = bar,                        
  width = 10,                           
  height = 7,
  dpi = 300 
)


# this won't have subtitle but is interactive
ggplotly(bar)

# ------------------------------------------------------------- multi_line_graph

# set the name of the file here
# dataset_name_multi <- "multitag.csv"

# Read the data
data_multi <- read.csv(paste0("/Users/Yarra/Downloads/experiment_data/", dataset_name_multi))

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
    title = paste("Graph of RSSI values over time for multiple tags. Data: ", dataset_name_multi)) +
  theme_minimal()  + 
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # White background
    plot.background = element_rect(fill = "lightgray", color = NA)  # Light gray background for the entire plot
  )

# normal graph with subtitle
multi_line_graph

ggsave(
  filename = "line_graph_RSSI_over_time_for_all_tags.png",  
  plot = multi_line_graph,                        
  width = 10,                           
  height = 7,
  dpi = 300 
)


# Convert to interactive plotly graph
ggplotly(multi_line_graph)


# ------------------------------------------------------------- multi_line_graph_timestamp_x_axis


# set the name of the file here
# dataset_name_multi <- "multitag.csv"

# Read the data
data_multi <- read.csv(paste0("/Users/Yarra/Downloads/experiment_data/", dataset_name_multi_timestamp))

# Filter out rows where 'count' is greater than 0 (if needed)
data_multi_ts <- data_multi %>%
  filter(is.na(count) | count == 0)

# use this if you want to delete the first row
# data_multi = data_multi[-1,]

# use this if you want to only grab the first n rows
# data_multi <- head(data_multi, 24)

# Plot the data, using scale_x_datetime and proper formatting for milliseconds
multi_line_graph_ts <- ggplot(data_multi_ts, aes(x=timestamp, y=rssi)) +
  geom_point(aes(colour = factor(epc)), size=3) +
  geom_line(aes(colour = factor(epc)), lineend="round") +
  labs(
    x = "Timestamp",
    y = "RSSI",
    title = paste("Graph of RSSI values over time for multiple tags with timestamps. Data: ", dataset_name_multi)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # White background
    plot.background = element_rect(fill = "lightgray", color = NA)  # Light gray background for the entire plot
  )

# Display the plot
print(multi_line_graph_ts)

# normal graph with subtitle
multi_line_graph_ts

ggsave(
  filename = "timestamp_by_RSSI_for_all_tags.png",
  plot = multi_line_graph_ts,
  width = 10,
  height = 7,
  dpi = 300
)

# Convert to interactive plotly graph
ggplotly(multi_line_graph_ts)


# ------------------------------------------------------------- rolling averages plot

library(zoo)

dataset_rolling_avg <- "12.5-rfid_2024-10-22_0-04-38.csv"

# Read the data
data_rolling_avg_graph <- read.csv(paste0("/Users/Yarra/Downloads/experiment_data/", dataset_rolling_avg))

# Extract milliseconds from the timestamp and convert to numeric
data_rolling_avg_graph$milliseconds <- as.numeric(sub(".*\\.(\\d+)", "\\1", data_rolling_avg_graph$timestamp))

# Parse the timestamp including milliseconds
data_rolling_avg_graph$timestamp <- ymd_hms(sub("\\.\\d+", "", data_rolling_avg_graph$timestamp), tz = "UTC") + 
  milliseconds(data_rolling_avg_graph$milliseconds)

# Calculate elapsed time in milliseconds from the first timestamp
data_rolling_avg_graph <- data_rolling_avg_graph %>%
  mutate(elapsed_time_ms = as.numeric(difftime(timestamp, min(timestamp), units = "secs")) * 1000)

# Filter out rows where 'count' is greater than 0 (if needed)
data_rolling_avg_graph <- data_rolling_avg_graph %>%
  filter(is.na(count) | count == 0)

data <- data_rolling_avg_graph

# Ensure that the RSSI and elapsed_time_ms columns are numeric
data$rssi <- as.numeric(data$rssi)
data$elapsed_time_ms <- as.numeric(data$elapsed_time_ms)

# Calculate rolling average for each EPC group
data <- data %>%
  group_by(epc) %>%
  arrange(elapsed_time_ms) %>%
  mutate(rolling_rssi = rollmean(rssi, k = 4, fill = NA)) # You can change 'k' for a different window size

# Plot RSSI rolling average over elapsed time per EPC
graph_RA <- ggplot(data, aes(x = elapsed_time_ms, y = rolling_rssi, color = epc)) +
  geom_line() +
  labs(title = paste("Rolling Average of RSSI Over Time by EPC. Data: ", dataset_rolling_avg),
       x = "Elapsed Time (ms)",
       y = "RSSI (Rolling Average)") +
  scale_x_continuous(breaks = seq(0, max(data$elapsed_time_ms, na.rm = TRUE), by = 500)) + # Increase x-axis ticks
  scale_y_continuous(breaks = seq(min(data$rssi, na.rm = TRUE), max(data$rssi, na.rm = TRUE), by = 5)) + # Increase y-axis ticks
  theme_minimal()

ggplotly(graph_RA)

# ------------------------------------------------------------- savitzky

library(signal)

# Define the dataset file path
dataset_rolling_avg <- "12.5-rfid_2024-10-22_0-04-38.csv"

# Read the data
data_rolling_avg_graph <- read.csv(paste0("/Users/Yarra/Downloads/experiment_data/", dataset_rolling_avg))

# Extract milliseconds from the timestamp and convert to numeric
data_rolling_avg_graph$milliseconds <- as.numeric(sub(".*\\.(\\d+)", "\\1", data_rolling_avg_graph$timestamp))

# Parse the timestamp including milliseconds
data_rolling_avg_graph$timestamp <- ymd_hms(sub("\\.\\d+", "", data_rolling_avg_graph$timestamp), tz = "UTC") + 
  milliseconds(data_rolling_avg_graph$milliseconds)

# Calculate elapsed time in milliseconds from the first timestamp
data_rolling_avg_graph <- data_rolling_avg_graph %>%
  mutate(elapsed_time_ms = as.numeric(difftime(timestamp, min(timestamp), units = "secs")) * 1000)

# Filter out rows where 'count' is greater than 0 (if needed)
data_rolling_avg_graph <- data_rolling_avg_graph %>%
  filter(is.na(count) | count == 0)

# Ensure that the RSSI and elapsed_time_ms columns are numeric
data_rolling_avg_graph$rssi <- as.numeric(data_rolling_avg_graph$rssi)
data_rolling_avg_graph$elapsed_time_ms <- as.numeric(data_rolling_avg_graph$elapsed_time_ms)

# Parameters for the Savitzky-Golay filter
window_size <- 5  # Use an odd window size for compatibility with the filter
poly_order <- 2   # Polynomial order for Savitzky-Golay

# Create a function to apply the Savitzky-Golay filter
apply_savgol_filter <- function(rssi_values) {
  if (length(rssi_values) < window_size) {
    return(rep(NA, length(rssi_values)))
  }
  sg_filter <- sgolayfilt(rssi_values, p = poly_order, n = window_size)
  return(sg_filter)
}

# Group by EPC and apply the Savitzky-Golay filter
data_smoothed <- data_rolling_avg_graph %>%
  group_by(epc) %>%
  arrange(elapsed_time_ms) %>%
  mutate(smoothed_rssi = apply_savgol_filter(rssi))

# Plot the smoothed RSSI over elapsed time for each EPC
graph_savgol <- ggplot(data_smoothed, aes(x = elapsed_time_ms, y = smoothed_rssi, color = epc)) +
  geom_line() +
  labs(title = paste("Savitzky-Golay Smoothed RSSI Over Time by EPC. Data: ", dataset_rolling_avg),
       x = "Elapsed Time (ms)",
       y = "RSSI (Smoothed)") +
  scale_x_continuous(breaks = seq(0, max(data_smoothed$elapsed_time_ms, na.rm = TRUE), by = 500)) + # Increase x-axis ticks
  scale_y_continuous(breaks = seq(min(data_smoothed$rssi, na.rm = TRUE), max(data_smoothed$rssi, na.rm = TRUE), by = 5)) + # Increase y-axis ticks
  theme_minimal()

# Convert to interactive plot
ggplotly(graph_savgol)

# ------------------------------------------------------------- EMA

# Define the dataset file path
dataset_rolling_avg <- "12.4-rfid_2024-10-22_0-04-10.csv"

# Read the data
data_rolling_avg_graph <- read.csv(paste0("/Users/Yarra/Downloads/experiment_data/", dataset_rolling_avg))

# Extract milliseconds from the timestamp and convert to numeric
data_rolling_avg_graph$milliseconds <- as.numeric(sub(".*\\.(\\d+)", "\\1", data_rolling_avg_graph$timestamp))

# Parse the timestamp including milliseconds
data_rolling_avg_graph$timestamp <- ymd_hms(sub("\\.\\d+", "", data_rolling_avg_graph$timestamp), tz = "UTC") + 
  milliseconds(data_rolling_avg_graph$milliseconds)

# Calculate elapsed time in milliseconds from the first timestamp
data_rolling_avg_graph <- data_rolling_avg_graph %>%
  mutate(elapsed_time_ms = as.numeric(difftime(timestamp, min(timestamp), units = "secs")) * 1000)

# Filter out rows where 'count' is greater than 0 (if needed)
data_rolling_avg_graph <- data_rolling_avg_graph %>%
  filter(is.na(count) | count == 0)

# Ensure that the RSSI and elapsed_time_ms columns are numeric
data_rolling_avg_graph$rssi <- as.numeric(data_rolling_avg_graph$rssi)
data_rolling_avg_graph$elapsed_time_ms <- as.numeric(data_rolling_avg_graph$elapsed_time_ms)

# Define the smoothing function for Exponential Moving Average (EMA)
apply_ema_filter <- function(rssi_values, alpha = 0.1) {
  if (length(rssi_values) < 2) {
    return(rep(NA, length(rssi_values)))
  }
  ema <- rep(NA, length(rssi_values))
  ema[1] <- rssi_values[1]
  for (i in 2:length(rssi_values)) {
    ema[i] <- alpha * rssi_values[i] + (1 - alpha) * ema[i - 1]
  }
  return(ema)
}

# Define the smoothing factor alpha
alpha <- 0.1  # Adjust this value (0 < alpha < 1) to control the smoothing effect

# Group by EPC and apply the EMA filter
data_smoothed <- data_rolling_avg_graph %>%
  group_by(epc) %>%
  arrange(elapsed_time_ms) %>%
  mutate(smoothed_rssi = apply_ema_filter(rssi, alpha))

# Plot the EMA-smoothed RSSI over elapsed time for each EPC
graph_ema <- ggplot(data_smoothed, aes(x = elapsed_time_ms, y = smoothed_rssi, color = epc)) +
  geom_line() +
  labs(title = paste("Exponential Moving Average Smoothed RSSI Over Time by EPC. Data: ", dataset_rolling_avg),
       x = "Elapsed Time (ms)",
       y = "RSSI (Smoothed with EMA)") +
  scale_x_continuous(breaks = seq(0, max(data_smoothed$elapsed_time_ms, na.rm = TRUE), by = 500)) + # Increase x-axis ticks
  scale_y_continuous(breaks = seq(min(data_smoothed$rssi, na.rm = TRUE), max(data_smoothed$rssi, na.rm = TRUE), by = 5)) + # Increase y-axis ticks
  theme_minimal()

# Convert to interactive plot
ggplotly(graph_ema)
