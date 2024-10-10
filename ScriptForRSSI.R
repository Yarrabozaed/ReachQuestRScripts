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

# Read the data
data_single <- read.csv("/Users/Yarra/Downloads/rfid_2024-10-09_1-21-41[1].csv")

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
  filter(count <= 0)

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
  geom_point(size=3, color="tomato2") + 
  geom_line(color="royalblue3", lineend="round") +
  labs(x = "Elapsed Time (ms)", y = "RSSI") +
  theme_minimal()

# Display the plot
scatter_plot
