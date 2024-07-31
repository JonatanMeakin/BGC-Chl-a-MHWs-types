
#Climatology

library("dplyr")
library("lubridate")
library("ncdf4")
library("ggplot2")

# Clear workspace
rm(list = ls())

# Define the file name and open the NetCDF file
filename <- 'IMOS_aggregation_20240726T035640Z.nc'
nc <- nc_open('IMOS_aggregation_20240726T035640Z.nc')

# Extract chlorophyll data using the correct variable ID
chl <- ncvar_get(nc, varid = 'chl_oc3')  # Adjust 'chl_oc3' if necessary based on file contents

# Get the number of time observations
num_obs <- dim(chl)[3]  # Assuming time is the 3rd dimension

# Extract and process date information with correct units
time_units <- ncatt_get(nc, "time", "units")$value
time_origin <- sub("days since ", "", time_units)
date_vals <- nc$dim$time$vals
date_fin <- as.Date(date_vals, origin = time_origin)

# Create a list to store data for each observation
data_list <- vector("list", num_obs)

# Loop through each observation and store chlorophyll data in a data frame
for (i in 1:num_obs) {
  data <- data.frame(chlorophyll = chl[, , i])
  data_list[[i]] <- data
}

# Function to calculate the mean chlorophyll concentration
spatav_exp3 <- function(df) {
  col_means <- sapply(df, function(x) mean(x, na.rm = TRUE))
  point_mean <- mean(col_means)
  return(point_mean)
}

# Apply the function to each data frame and store results
results <- data.frame(chlorophyll_mean = sapply(data_list, spatav_exp3))

# Interpolate missing values if necessary
interp <- function(x) {
  na_indices <- which(is.na(x))
  for (i in na_indices) {
    left_val <- ifelse(i > 1, x[i - 1], NA)
    right_val <- ifelse(i < length(x), x[i + 1], NA)
    x[i] <- mean(c(left_val, right_val), na.rm = TRUE)
  }
  return(x)
}

# Check the length of results and perform interpolation
chl_interp <- data.frame(chlorophyll_interp = interp(results$chlorophyll_mean))

# Ensure date_fin and chl_interp have the same length
if (length(date_fin) == nrow(chl_interp)) {
  chla <- data.frame(t = date_fin, Chla = chl_interp$chlorophyll_interp)
} else {
  stop("Length mismatch between dates and chlorophyll data.")
}

# Calculate the day of the year for each date
chla$day_of_year <- yday(chla$t)

# Calculate daily climatology, excluding day 366 to avoid issues with non-leap years
daily_climatology <- chla %>%
  filter(day_of_year <= 365) %>%
  group_by(day_of_year) %>%
  summarise(Chla_climatology = mean(Chla, na.rm = TRUE))

# Add dates (mm-dd format) corresponding to day of the year
daily_climatology <- daily_climatology %>%
  mutate(date = as.Date(day_of_year - 1, origin = "1970-01-01"))

# Remove "Jan 01" at the end of the year
daily_climatology <- daily_climatology %>%
  filter(date < as.Date("1971-01-01"))  # Exclude duplicate Jan 01

# Plot the daily climatology with actual dates on x-axis
ggplot(daily_climatology) +
  geom_line(aes(x = date, y = Chla_climatology), color = "blue", linewidth = 1, linetype = "solid", alpha = 0.7) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +
  labs(title = "Daily Climatology of Chlorophyll-a Concentration",
       x = "Date",
       y = "Chlorophyll-a (mg/m^3)") +
  theme_minimal()


#-------------------------------------------------------------------------------

#November 2015

# Load necessary libraries
library(dplyr)
library(lubridate)
library(ncdf4)
library(ggplot2)

# Clear workspace
rm(list = ls())

# Define the file name and open the NetCDF file
filename <- 'IMOS_aggregation_20240726T035640Z.nc'
nc <- nc_open('IMOS_aggregation_20240726T035640Z.nc')

# Extract chlorophyll data using the correct variable ID
chl <- ncvar_get(nc, varid = 'chl_oc3')

# Get the number of time observations
num_obs <- dim(chl)[3]  # Assuming time is the 3rd dimension

# Extract and process date information with correct units
time_units <- ncatt_get(nc, "time", "units")$value
time_origin <- sub("days since ", "", time_units)
date_vals <- nc$dim$time$vals
date_fin <- as.Date(date_vals, origin = time_origin)

# Create a list to store data for each observation
data_list <- vector("list", num_obs)

# Loop through each observation and store chlorophyll data in a data frame
for (i in 1:num_obs) {
  data <- data.frame(chlorophyll = chl[, , i])
  data_list[[i]] <- data
}

# Function to calculate the mean chlorophyll concentration
spatav_exp3 <- function(df) {
  col_means <- sapply(df, function(x) mean(x, na.rm = TRUE))
  point_mean <- mean(col_means)
  return(point_mean)
}

# Apply the function to each data frame and store results
results <- data.frame(chlorophyll_mean = sapply(data_list, spatav_exp3))

# Interpolate missing values if necessary
interp <- function(x) {
  na_indices <- which(is.na(x))
  for (i in na_indices) {
    left_val <- ifelse(i > 1, x[i - 1], NA)
    right_val <- ifelse(i < length(x), x[i + 1], NA)
    x[i] <- mean(c(left_val, right_val), na.rm = TRUE)
  }
  return(x)
}

# Check the length of results and perform interpolation
chl_interp <- data.frame(chlorophyll_interp = interp(results$chlorophyll_mean))

# Ensure date_fin and chl_interp have the same length
if (length(date_fin) == nrow(chl_interp)) {
  chla <- data.frame(t = date_fin, Chla = chl_interp$chlorophyll_interp)
} else {
  stop("Length mismatch between dates and chlorophyll data.")
}

# Calculate the day of the year for each date
chla$day_of_year <- yday(chla$t)

# Calculate daily climatology, excluding day 366 to avoid issues with non-leap years
daily_climatology <- chla %>%
  filter(day_of_year <= 365) %>%
  group_by(day_of_year) %>%
  summarise(Chla_climatology = mean(Chla, na.rm = TRUE))

# Add dates (mm-dd format) corresponding to day of the year
daily_climatology <- daily_climatology %>%
  mutate(date = format(as.Date(day_of_year - 1, origin = "1970-01-01"), "%m-%d"))

# Arrange the dataframe in date order
daily_climatology <- daily_climatology %>%
  arrange(day_of_year) %>%
  select(day_of_year, date, Chla_climatology)

# Merge the climatology with the original data
merged_data <- merge(chla, daily_climatology, by = "day_of_year")

# Calculate anomalies
merged_data <- merged_data %>%
  mutate(anomaly = Chla - Chla_climatology)

# Calculate the 10th percentile and 90th percentile anomalies for each day across all years
daily_percentiles <- merged_data %>%
  group_by(date) %>%
  summarise(percentile_10 = quantile(anomaly, 0.1, na.rm = TRUE),
            percentile_90 = quantile(anomaly, 0.9, na.rm = TRUE))

# Calculate the mean anomaly for each day across all years
daily_mean_anomaly <- merged_data %>%
  group_by(date) %>%
  summarise(mean_anomaly = mean(anomaly, na.rm = TRUE))

# Merge the percentiles and mean anomaly data with the original data
merged_data <- merge(merged_data, daily_percentiles, by = "date")
merged_data <- merge(merged_data, daily_mean_anomaly, by = "date")

# Determine if the anomaly is in the 10% most negative or positive anomalies
merged_data <- merged_data %>%
  mutate(in_10_percentile_negative = anomaly <= percentile_10,
         in_10_percentile_positive = anomaly >= percentile_90)

# Determine if positive anomalies are more positive than the average anomaly for that day
merged_data <- merged_data %>%
  mutate(more_positive_than_average = anomaly > mean_anomaly)

# Ensure the data is in chronological order
merged_data <- merged_data %>%
  arrange(t)

# Select relevant columns for final output
merged_data <- merged_data %>%
  select(t, date, Chla, Chla_climatology, anomaly, 
         in_10_percentile_negative, in_10_percentile_positive, 
         mean_anomaly, more_positive_than_average)

# Filter the data for the specified time range (1st Nov 2015 to 30th Nov 2015)
plot_data <- merged_data %>%
  filter(t >= as.Date("2015-11-01") & t <= as.Date("2015-11-30"))

# Print the values for the selected days
print(plot_data %>% select(t, in_10_percentile_negative, in_10_percentile_positive, more_positive_than_average))

# Plotting the data with colours indicating positive or negative anomalies
ggplot(plot_data, aes(x = t, y = anomaly)) +
  geom_bar(stat = "identity", aes(fill = anomaly > 0)) +
  scale_fill_manual(values = c("FALSE" = "red3", "TRUE" = "green4"), 
                    labels = c("Negative", "Positive"), 
                    name = "Anomaly") +
  labs(title = "Chl-a Anomalies Nov 2015",
       x = "Time",
       y = "Anomaly (mg/m^3)",
       caption = "Bar plot representing anomalies above and below climatology") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        aspect.ratio = 0.5) +  # Adjust aspect ratio for a narrower plot
  ylim(-1, 2.5)  # Set y-axis limits



#December 2015

# Load necessary libraries
library(dplyr)
library(lubridate)
library(ncdf4)
library(ggplot2)

# Clear workspace
rm(list = ls())

# Define the file name and open the NetCDF file
filename <- 'IMOS_aggregation_20240726T035640Z.nc'
nc <- nc_open('IMOS_aggregation_20240726T035640Z.nc')

# Extract chlorophyll data using the correct variable ID
chl <- ncvar_get(nc, varid = 'chl_oc3')

# Get the number of time observations
num_obs <- dim(chl)[3]  # Assuming time is the 3rd dimension

# Extract and process date information with correct units
time_units <- ncatt_get(nc, "time", "units")$value
time_origin <- sub("days since ", "", time_units)
date_vals <- nc$dim$time$vals
date_fin <- as.Date(date_vals, origin = time_origin)

# Create a list to store data for each observation
data_list <- vector("list", num_obs)

# Loop through each observation and store chlorophyll data in a data frame
for (i in 1:num_obs) {
  data <- data.frame(chlorophyll = chl[, , i])
  data_list[[i]] <- data
}

# Function to calculate the mean chlorophyll concentration
spatav_exp3 <- function(df) {
  col_means <- sapply(df, function(x) mean(x, na.rm = TRUE))
  point_mean <- mean(col_means)
  return(point_mean)
}

# Apply the function to each data frame and store results
results <- data.frame(chlorophyll_mean = sapply(data_list, spatav_exp3))

# Interpolate missing values if necessary
interp <- function(x) {
  na_indices <- which(is.na(x))
  for (i in na_indices) {
    left_val <- ifelse(i > 1, x[i - 1], NA)
    right_val <- ifelse(i < length(x), x[i + 1], NA)
    x[i] <- mean(c(left_val, right_val), na.rm = TRUE)
  }
  return(x)
}

# Check the length of results and perform interpolation
chl_interp <- data.frame(chlorophyll_interp = interp(results$chlorophyll_mean))

# Ensure date_fin and chl_interp have the same length
if (length(date_fin) == nrow(chl_interp)) {
  chla <- data.frame(t = date_fin, Chla = chl_interp$chlorophyll_interp)
} else {
  stop("Length mismatch between dates and chlorophyll data.")
}

# Calculate the day of the year for each date
chla$day_of_year <- yday(chla$t)

# Calculate daily climatology, excluding day 366 to avoid issues with non-leap years
daily_climatology <- chla %>%
  filter(day_of_year <= 365) %>%
  group_by(day_of_year) %>%
  summarise(Chla_climatology = mean(Chla, na.rm = TRUE))

# Add dates (mm-dd format) corresponding to day of the year
daily_climatology <- daily_climatology %>%
  mutate(date = format(as.Date(day_of_year - 1, origin = "1970-01-01"), "%m-%d"))

# Arrange the dataframe in date order
daily_climatology <- daily_climatology %>%
  arrange(day_of_year) %>%
  select(day_of_year, date, Chla_climatology)

# Merge the climatology with the original data
merged_data <- merge(chla, daily_climatology, by = "day_of_year")

# Calculate anomalies
merged_data <- merged_data %>%
  mutate(anomaly = Chla - Chla_climatology)

# Calculate the 10th percentile and 90th percentile anomalies for each day across all years
daily_percentiles <- merged_data %>%
  group_by(date) %>%
  summarise(percentile_10 = quantile(anomaly, 0.1, na.rm = TRUE),
            percentile_90 = quantile(anomaly, 0.9, na.rm = TRUE))

# Calculate the mean anomaly for each day across all years
daily_mean_anomaly <- merged_data %>%
  group_by(date) %>%
  summarise(mean_anomaly = mean(anomaly, na.rm = TRUE))

# Merge the percentiles and mean anomaly data with the original data
merged_data <- merge(merged_data, daily_percentiles, by = "date")
merged_data <- merge(merged_data, daily_mean_anomaly, by = "date")

# Determine if the anomaly is in the 10% most negative or positive anomalies
merged_data <- merged_data %>%
  mutate(in_10_percentile_negative = anomaly <= percentile_10,
         in_10_percentile_positive = anomaly >= percentile_90)

# Determine if anomalies are more negative or more positive than the average anomaly for that day
merged_data <- merged_data %>%
  mutate(more_negative_than_average = anomaly < mean_anomaly,
         more_positive_than_average = anomaly > mean_anomaly)

# Ensure the data is in chronological order
merged_data <- merged_data %>%
  arrange(t)

# Select relevant columns for final output
merged_data <- merged_data %>%
  select(t, date, Chla, Chla_climatology, anomaly, 
         in_10_percentile_negative, in_10_percentile_positive, 
         mean_anomaly, more_negative_than_average, more_positive_than_average)

# Filter the data for the specified time range (1st Dec 2015 to 31st Dec 2015)
plot_data <- merged_data %>%
  filter(t >= as.Date("2015-12-01") & t <= as.Date("2015-12-31"))

# Print the values for the selected days
print(plot_data %>% select(t, in_10_percentile_negative, in_10_percentile_positive, more_negative_than_average, more_positive_than_average))

# Plotting the data with colours indicating positive or negative anomalies
ggplot(plot_data, aes(x = t, y = anomaly)) +
  geom_bar(stat = "identity", aes(fill = anomaly > 0)) +
  scale_fill_manual(values = c("FALSE" = "red3", "TRUE" = "green4"), 
                    labels = c("Negative", "Positive"), 
                    name = "Anomaly") +
  labs(title = "Chl-a Anomalies Dec 2015",
       x = "Time",
       y = "Anomaly (mg/m^3)",
       caption = "Bar plot representing anomalies above and below climatology") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        aspect.ratio = 0.5) +
  ylim(-1, 2.5) # Adjust aspect ratio for a narrower plot


#January 2016

# Load necessary libraries
library(dplyr)
library(lubridate)
library(ncdf4)
library(ggplot2)

# Clear workspace
rm(list = ls())

# Define the file name and open the NetCDF file
filename <- 'IMOS_aggregation_20240726T035640Z.nc'
nc <- nc_open('IMOS_aggregation_20240726T035640Z.nc')

# Extract chlorophyll data using the correct variable ID
chl <- ncvar_get(nc, varid = 'chl_oc3')

# Get the number of time observations
num_obs <- dim(chl)[3]  # Assuming time is the 3rd dimension

# Extract and process date information with correct units
time_units <- ncatt_get(nc, "time", "units")$value
time_origin <- sub("days since ", "", time_units)
date_vals <- nc$dim$time$vals
date_fin <- as.Date(date_vals, origin = time_origin)

# Create a list to store data for each observation
data_list <- vector("list", num_obs)

# Loop through each observation and store chlorophyll data in a data frame
for (i in 1:num_obs) {
  data <- data.frame(chlorophyll = chl[, , i])
  data_list[[i]] <- data
}

# Function to calculate the mean chlorophyll concentration
spatav_exp3 <- function(df) {
  col_means <- sapply(df, function(x) mean(x, na.rm = TRUE))
  point_mean <- mean(col_means)
  return(point_mean)
}

# Apply the function to each data frame and store results
results <- data.frame(chlorophyll_mean = sapply(data_list, spatav_exp3))

# Interpolate missing values if necessary
interp <- function(x) {
  na_indices <- which(is.na(x))
  for (i in na_indices) {
    left_val <- ifelse(i > 1, x[i - 1], NA)
    right_val <- ifelse(i < length(x), x[i + 1], NA)
    x[i] <- mean(c(left_val, right_val), na.rm = TRUE)
  }
  return(x)
}

# Check the length of results and perform interpolation
chl_interp <- data.frame(chlorophyll_interp = interp(results$chlorophyll_mean))

# Ensure date_fin and chl_interp have the same length
if (length(date_fin) == nrow(chl_interp)) {
  chla <- data.frame(t = date_fin, Chla = chl_interp$chlorophyll_interp)
} else {
  stop("Length mismatch between dates and chlorophyll data.")
}

# Calculate the day of the year for each date
chla$day_of_year <- yday(chla$t)

# Calculate daily climatology, excluding day 366 to avoid issues with non-leap years
daily_climatology <- chla %>%
  filter(day_of_year <= 365) %>%
  group_by(day_of_year) %>%
  summarise(Chla_climatology = mean(Chla, na.rm = TRUE))

# Add dates (mm-dd format) corresponding to day of the year
daily_climatology <- daily_climatology %>%
  mutate(date = format(as.Date(day_of_year - 1, origin = "1970-01-01"), "%m-%d"))

# Arrange the dataframe in date order
daily_climatology <- daily_climatology %>%
  arrange(day_of_year) %>%
  select(day_of_year, date, Chla_climatology)

# Merge the climatology with the original data
merged_data <- merge(chla, daily_climatology, by = "day_of_year")

# Calculate anomalies
merged_data <- merged_data %>%
  mutate(anomaly = Chla - Chla_climatology)

# Calculate the 10th percentile and 90th percentile anomalies for each day across all years
daily_percentiles <- merged_data %>%
  group_by(date) %>%
  summarise(percentile_10 = quantile(anomaly, 0.1, na.rm = TRUE),
            percentile_90 = quantile(anomaly, 0.9, na.rm = TRUE))

# Calculate the mean anomaly for each day across all years
daily_mean_anomaly <- merged_data %>%
  group_by(date) %>%
  summarise(mean_anomaly = mean(anomaly, na.rm = TRUE))

# Merge the percentiles and mean anomaly data with the original data
merged_data <- merge(merged_data, daily_percentiles, by = "date")
merged_data <- merge(merged_data, daily_mean_anomaly, by = "date")

# Determine if the anomaly is in the 10% most negative or positive anomalies
merged_data <- merged_data %>%
  mutate(in_10_percentile_negative = anomaly <= percentile_10,
         in_10_percentile_positive = anomaly >= percentile_90)

# Determine if anomalies are more negative or more positive than the average anomaly for that day
merged_data <- merged_data %>%
  mutate(more_negative_than_average = anomaly < mean_anomaly,
         more_positive_than_average = anomaly > mean_anomaly)

# Ensure the data is in chronological order
merged_data <- merged_data %>%
  arrange(t)

# Select relevant columns for final output
merged_data <- merged_data %>%
  select(t, date, Chla, Chla_climatology, anomaly, 
         in_10_percentile_negative, in_10_percentile_positive, 
         mean_anomaly, more_negative_than_average, more_positive_than_average)

# Filter the data for the specified time range (1st Jan 2016 to 31st Jan 2016)
plot_data <- merged_data %>%
  filter(t >= as.Date("2016-01-01") & t <= as.Date("2016-01-31"))

# Print the values for the selected days
print(plot_data %>% select(t, in_10_percentile_negative, in_10_percentile_positive, more_negative_than_average, more_positive_than_average))

# Plotting the data with colours indicating positive or negative anomalies
ggplot(plot_data, aes(x = t, y = anomaly)) +
  geom_bar(stat = "identity", aes(fill = anomaly > 0)) +
  scale_fill_manual(values = c("FALSE" = "red3", "TRUE" = "green4"), 
                    labels = c("Negative", "Positive"), 
                    name = "Anomaly") +
  labs(title = "Chl-a Anomalies Jan 2016",
       x = "Time",
       y = "Anomaly (mg/m^3)",
       caption = "Bar plot representing anomalies above and below climatology") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        aspect.ratio = 0.5) +
  coord_cartesian(ylim = c(-1, 2.5))




#February 2016

# Load necessary libraries
library(dplyr)
library(lubridate)
library(ncdf4)
library(ggplot2)

# Clear workspace
rm(list = ls())

# Define the file name and open the NetCDF file
filename <- 'IMOS_aggregation_20240726T035640Z.nc'
nc <- nc_open('IMOS_aggregation_20240726T035640Z.nc')

# Extract chlorophyll data using the correct variable ID
chl <- ncvar_get(nc, varid = 'chl_oc3')

# Get the number of time observations
num_obs <- dim(chl)[3]  # Assuming time is the 3rd dimension

# Extract and process date information with correct units
time_units <- ncatt_get(nc, "time", "units")$value
time_origin <- sub("days since ", "", time_units)
date_vals <- nc$dim$time$vals
date_fin <- as.Date(date_vals, origin = time_origin)

# Create a list to store data for each observation
data_list <- vector("list", num_obs)

# Loop through each observation and store chlorophyll data in a data frame
for (i in 1:num_obs) {
  data <- data.frame(chlorophyll = chl[, , i])
  data_list[[i]] <- data
}

# Function to calculate the mean chlorophyll concentration
spatav_exp3 <- function(df) {
  col_means <- sapply(df, function(x) mean(x, na.rm = TRUE))
  point_mean <- mean(col_means)
  return(point_mean)
}

# Apply the function to each data frame and store results
results <- data.frame(chlorophyll_mean = sapply(data_list, spatav_exp3))

# Interpolate missing values if necessary
interp <- function(x) {
  na_indices <- which(is.na(x))
  for (i in na_indices) {
    left_val <- ifelse(i > 1, x[i - 1], NA)
    right_val <- ifelse(i < length(x), x[i + 1], NA)
    x[i] <- mean(c(left_val, right_val), na.rm = TRUE)
  }
  return(x)
}

# Check the length of results and perform interpolation
chl_interp <- data.frame(chlorophyll_interp = interp(results$chlorophyll_mean))

# Ensure date_fin and chl_interp have the same length
if (length(date_fin) == nrow(chl_interp)) {
  chla <- data.frame(t = date_fin, Chla = chl_interp$chlorophyll_interp)
} else {
  stop("Length mismatch between dates and chlorophyll data.")
}

# Calculate the day of the year for each date
chla$day_of_year <- yday(chla$t)

# Calculate daily climatology, excluding day 366 to avoid issues with non-leap years
daily_climatology <- chla %>%
  filter(day_of_year <= 365) %>%
  group_by(day_of_year) %>%
  summarise(Chla_climatology = mean(Chla, na.rm = TRUE))

# Add dates (mm-dd format) corresponding to day of the year
daily_climatology <- daily_climatology %>%
  mutate(date = format(as.Date(day_of_year - 1, origin = "1970-01-01"), "%m-%d"))

# Arrange the dataframe in date order
daily_climatology <- daily_climatology %>%
  arrange(day_of_year) %>%
  select(day_of_year, date, Chla_climatology)

# Merge the climatology with the original data
merged_data <- merge(chla, daily_climatology, by = "day_of_year")

# Calculate anomalies
merged_data <- merged_data %>%
  mutate(anomaly = Chla - Chla_climatology)

# Calculate the 10th percentile and 90th percentile anomalies for each day across all years
daily_percentiles <- merged_data %>%
  group_by(date) %>%
  summarise(percentile_10 = quantile(anomaly, 0.1, na.rm = TRUE),
            percentile_90 = quantile(anomaly, 0.9, na.rm = TRUE))

# Calculate the mean anomaly for each day across all years
daily_mean_anomaly <- merged_data %>%
  group_by(date) %>%
  summarise(mean_anomaly = mean(anomaly, na.rm = TRUE))

# Merge the percentiles and mean anomaly data with the original data
merged_data <- merge(merged_data, daily_percentiles, by = "date")
merged_data <- merge(merged_data, daily_mean_anomaly, by = "date")

# Determine if the anomaly is in the 10% most negative or positive anomalies
merged_data <- merged_data %>%
  mutate(in_10_percentile_negative = anomaly <= percentile_10,
         in_10_percentile_positive = anomaly >= percentile_90)

# Determine if anomalies are more negative or more positive than the average anomaly for that day
merged_data <- merged_data %>%
  mutate(more_negative_than_average = anomaly < mean_anomaly,
         more_positive_than_average = anomaly > mean_anomaly)

# Ensure the data is in chronological order
merged_data <- merged_data %>%
  arrange(t)

# Select relevant columns for final output
merged_data <- merged_data %>%
  select(t, date, Chla, Chla_climatology, anomaly, 
         in_10_percentile_negative, in_10_percentile_positive, 
         mean_anomaly, more_negative_than_average, more_positive_than_average)

# Filter the data for the specified time range (1st Feb 2016 to 29th Feb 2016)
plot_data <- merged_data %>%
  filter(t >= as.Date("2016-02-01") & t <= as.Date("2016-02-29"))

# Print the values for the selected days
print(plot_data %>% select(t, in_10_percentile_negative, in_10_percentile_positive, more_negative_than_average, more_positive_than_average))

# Plotting the data with colours indicating positive or negative anomalies
ggplot(plot_data, aes(x = t, y = anomaly)) +
  geom_bar(stat = "identity", aes(fill = anomaly > 0)) +
  scale_fill_manual(values = c("FALSE" = "red3", "TRUE" = "green4"), 
                    labels = c("Negative", "Positive"), 
                    name = "Anomaly") +
  labs(title = "Chl-a Anomalies Feb 2016",
       x = "Time",
       y = "Anomaly (mg/m^3)",
       caption = "Bar plot representing anomalies above and below climatology") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        aspect.ratio = 0.5)+
  ylim(-1, 2.5)# Adjust aspect ratio for a narrower plot


#March 2016

# Load necessary libraries
library(dplyr)
library(lubridate)
library(ncdf4)
library(ggplot2)

# Clear workspace
rm(list = ls())

# Define the file name and open the NetCDF file
filename <- 'IMOS_aggregation_20240726T035640Z.nc'
nc <- nc_open('IMOS_aggregation_20240726T035640Z.nc')

# Extract chlorophyll data using the correct variable ID
chl <- ncvar_get(nc, varid = 'chl_oc3')

# Get the number of time observations
num_obs <- dim(chl)[3]  # Assuming time is the 3rd dimension

# Extract and process date information with correct units
time_units <- ncatt_get(nc, "time", "units")$value
time_origin <- sub("days since ", "", time_units)
date_vals <- nc$dim$time$vals
date_fin <- as.Date(date_vals, origin = time_origin)

# Create a list to store data for each observation
data_list <- vector("list", num_obs)

# Loop through each observation and store chlorophyll data in a data frame
for (i in 1:num_obs) {
  data <- data.frame(chlorophyll = chl[, , i])
  data_list[[i]] <- data
}

# Function to calculate the mean chlorophyll concentration
spatav_exp3 <- function(df) {
  col_means <- sapply(df, function(x) mean(x, na.rm = TRUE))
  point_mean <- mean(col_means)
  return(point_mean)
}

# Apply the function to each data frame and store results
results <- data.frame(chlorophyll_mean = sapply(data_list, spatav_exp3))

# Interpolate missing values if necessary
interp <- function(x) {
  na_indices <- which(is.na(x))
  for (i in na_indices) {
    left_val <- ifelse(i > 1, x[i - 1], NA)
    right_val <- ifelse(i < length(x), x[i + 1], NA)
    x[i] <- mean(c(left_val, right_val), na.rm = TRUE)
  }
  return(x)
}

# Check the length of results and perform interpolation
chl_interp <- data.frame(chlorophyll_interp = interp(results$chlorophyll_mean))

# Ensure date_fin and chl_interp have the same length
if (length(date_fin) == nrow(chl_interp)) {
  chla <- data.frame(t = date_fin, Chla = chl_interp$chlorophyll_interp)
} else {
  stop("Length mismatch between dates and chlorophyll data.")
}

# Calculate the day of the year for each date
chla$day_of_year <- yday(chla$t)

# Calculate daily climatology, excluding day 366 to avoid issues with non-leap years
daily_climatology <- chla %>%
  filter(day_of_year <= 365) %>%
  group_by(day_of_year) %>%
  summarise(Chla_climatology = mean(Chla, na.rm = TRUE))

# Add dates (mm-dd format) corresponding to day of the year
daily_climatology <- daily_climatology %>%
  mutate(date = format(as.Date(day_of_year - 1, origin = "1970-01-01"), "%m-%d"))

# Arrange the dataframe in date order
daily_climatology <- daily_climatology %>%
  arrange(day_of_year) %>%
  select(day_of_year, date, Chla_climatology)

# Merge the climatology with the original data
merged_data <- merge(chla, daily_climatology, by = "day_of_year")

# Calculate anomalies
merged_data <- merged_data %>%
  mutate(anomaly = Chla - Chla_climatology)

# Calculate the 10th percentile and 90th percentile anomalies for each day across all years
daily_percentiles <- merged_data %>%
  group_by(date) %>%
  summarise(percentile_10 = quantile(anomaly, 0.1, na.rm = TRUE),
            percentile_90 = quantile(anomaly, 0.9, na.rm = TRUE))

# Calculate the mean anomaly for each day across all years
daily_mean_anomaly <- merged_data %>%
  group_by(date) %>%
  summarise(mean_anomaly = mean(anomaly, na.rm = TRUE))

# Merge the percentiles and mean anomaly data with the original data
merged_data <- merge(merged_data, daily_percentiles, by = "date")
merged_data <- merge(merged_data, daily_mean_anomaly, by = "date")

# Determine if the anomaly is in the 10% most negative or positive anomalies
merged_data <- merged_data %>%
  mutate(in_10_percentile_negative = anomaly <= percentile_10,
         in_10_percentile_positive = anomaly >= percentile_90)

# Determine if anomalies are more negative or more positive than the average anomaly for that day
merged_data <- merged_data %>%
  mutate(more_negative_than_average = anomaly < mean_anomaly,
         more_positive_than_average = anomaly > mean_anomaly)

# Ensure the data is in chronological order
merged_data <- merged_data %>%
  arrange(t)

# Select relevant columns for final output
merged_data <- merged_data %>%
  select(t, date, Chla, Chla_climatology, anomaly, 
         in_10_percentile_negative, in_10_percentile_positive, 
         mean_anomaly, more_negative_than_average, more_positive_than_average)

# Filter the data for the specified time range (1st March 2016 to 31st March 2016)
plot_data <- merged_data %>%
  filter(t >= as.Date("2016-03-01") & t <= as.Date("2016-03-31"))

# Print the values for the selected days
print(plot_data %>% select(t, in_10_percentile_negative, in_10_percentile_positive, more_negative_than_average, more_positive_than_average))

# Plotting the data with colours indicating positive or negative anomalies
ggplot(plot_data, aes(x = t, y = anomaly)) +
  geom_bar(stat = "identity", aes(fill = anomaly > 0)) +
  scale_fill_manual(values = c("FALSE" = "red3", "TRUE" = "green4"), 
                    labels = c("Negative", "Positive"), 
                    name = "Anomaly") +
  labs(title = "Chl-a Anomalies March 2016",
       x = "Time",
       y = "Anomaly (mg/m^3)",
       caption = "Bar plot representing anomalies above and below climatology") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        aspect.ratio = 0.5)+
  ylim(-1, 2.5)# Adjust aspect ratio for a narrower plot


#April 2016

# Load necessary libraries
library(dplyr)
library(lubridate)
library(ncdf4)
library(ggplot2)

# Clear workspace
rm(list = ls())

# Define the file name and open the NetCDF file
filename <- 'IMOS_aggregation_20240726T035640Z.nc'
nc <- nc_open('IMOS_aggregation_20240726T035640Z.nc')

# Extract chlorophyll data using the correct variable ID
chl <- ncvar_get(nc, varid = 'chl_oc3')

# Get the number of time observations
num_obs <- dim(chl)[3]  # Assuming time is the 3rd dimension

# Extract and process date information with correct units
time_units <- ncatt_get(nc, "time", "units")$value
time_origin <- sub("days since ", "", time_units)
date_vals <- nc$dim$time$vals
date_fin <- as.Date(date_vals, origin = time_origin)

# Create a list to store data for each observation
data_list <- vector("list", num_obs)

# Loop through each observation and store chlorophyll data in a data frame
for (i in 1:num_obs) {
  data <- data.frame(chlorophyll = chl[, , i])
  data_list[[i]] <- data
}

# Function to calculate the mean chlorophyll concentration
spatav_exp3 <- function(df) {
  col_means <- sapply(df, function(x) mean(x, na.rm = TRUE))
  point_mean <- mean(col_means)
  return(point_mean)
}

# Apply the function to each data frame and store results
results <- data.frame(chlorophyll_mean = sapply(data_list, spatav_exp3))

# Interpolate missing values if necessary
interp <- function(x) {
  na_indices <- which(is.na(x))
  for (i in na_indices) {
    left_val <- ifelse(i > 1, x[i - 1], NA)
    right_val <- ifelse(i < length(x), x[i + 1], NA)
    x[i] <- mean(c(left_val, right_val), na.rm = TRUE)
  }
  return(x)
}

# Check the length of results and perform interpolation
chl_interp <- data.frame(chlorophyll_interp = interp(results$chlorophyll_mean))

# Ensure date_fin and chl_interp have the same length
if (length(date_fin) == nrow(chl_interp)) {
  chla <- data.frame(t = date_fin, Chla = chl_interp$chlorophyll_interp)
} else {
  stop("Length mismatch between dates and chlorophyll data.")
}

# Calculate the day of the year for each date
chla$day_of_year <- yday(chla$t)

# Calculate daily climatology, excluding day 366 to avoid issues with non-leap years
daily_climatology <- chla %>%
  filter(day_of_year <= 365) %>%
  group_by(day_of_year) %>%
  summarise(Chla_climatology = mean(Chla, na.rm = TRUE))

# Add dates (mm-dd format) corresponding to day of the year
daily_climatology <- daily_climatology %>%
  mutate(date = format(as.Date(day_of_year - 1, origin = "1970-01-01"), "%m-%d"))

# Arrange the dataframe in date order
daily_climatology <- daily_climatology %>%
  arrange(day_of_year) %>%
  select(day_of_year, date, Chla_climatology)

# Merge the climatology with the original data
merged_data <- merge(chla, daily_climatology, by = "day_of_year")

# Calculate anomalies
merged_data <- merged_data %>%
  mutate(anomaly = Chla - Chla_climatology)

# Calculate the 10th percentile and 90th percentile anomalies for each day across all years
daily_percentiles <- merged_data %>%
  group_by(date) %>%
  summarise(percentile_10 = quantile(anomaly, 0.1, na.rm = TRUE),
            percentile_90 = quantile(anomaly, 0.9, na.rm = TRUE))

# Calculate the mean anomaly for each day across all years
daily_mean_anomaly <- merged_data %>%
  group_by(date) %>%
  summarise(mean_anomaly = mean(anomaly, na.rm = TRUE))

# Merge the percentiles and mean anomaly data with the original data
merged_data <- merge(merged_data, daily_percentiles, by = "date")
merged_data <- merge(merged_data, daily_mean_anomaly, by = "date")

# Determine if the anomaly is in the 10% most negative or positive anomalies
merged_data <- merged_data %>%
  mutate(in_10_percentile_negative = anomaly <= percentile_10,
         in_10_percentile_positive = anomaly >= percentile_90)

# Determine if anomalies are more negative or more positive than the average anomaly for that day
merged_data <- merged_data %>%
  mutate(more_negative_than_average = anomaly < mean_anomaly,
         more_positive_than_average = anomaly > mean_anomaly)

# Ensure the data is in chronological order
merged_data <- merged_data %>%
  arrange(t)

# Select relevant columns for final output
merged_data <- merged_data %>%
  select(t, date, Chla, Chla_climatology, anomaly, 
         in_10_percentile_negative, in_10_percentile_positive, 
         mean_anomaly, more_negative_than_average, more_positive_than_average)

# Filter the data for the specified time range (1st April 2016 to 30th April 2016)
plot_data <- merged_data %>%
  filter(t >= as.Date("2016-04-01") & t <= as.Date("2016-04-30"))

# Print the values for the selected days
print(plot_data %>% select(t, in_10_percentile_negative, in_10_percentile_positive, more_negative_than_average, more_positive_than_average))

# Plotting the data with colours indicating positive or negative anomalies
ggplot(plot_data, aes(x = t, y = anomaly)) +
  geom_bar(stat = "identity", aes(fill = anomaly > 0)) +
  scale_fill_manual(values = c("FALSE" = "red3", "TRUE" = "green4"), 
                    labels = c("Negative", "Positive"), 
                    name = "Anomaly") +
  labs(title = "Chl-a Anomalies April 2016",
       x = "Time",
       y = "Anomaly (mg/m^3)",
       caption = "Bar plot representing anomalies above and below climatology") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        aspect.ratio = 0.5)+
  ylim(-1, 2.5)# Adjust aspect ratio for a narrower plot


#May 2016

# Load necessary libraries
library(dplyr)
library(lubridate)
library(ncdf4)
library(ggplot2)

# Clear workspace
rm(list = ls())

# Define the file name and open the NetCDF file
filename <- 'IMOS_aggregation_20240726T035640Z.nc'
nc <- nc_open('IMOS_aggregation_20240726T035640Z.nc')

# Extract chlorophyll data using the correct variable ID
chl <- ncvar_get(nc, varid = 'chl_oc3')

# Get the number of time observations
num_obs <- dim(chl)[3]  # Assuming time is the 3rd dimension

# Extract and process date information with correct units
time_units <- ncatt_get(nc, "time", "units")$value
time_origin <- sub("days since ", "", time_units)
date_vals <- nc$dim$time$vals
date_fin <- as.Date(date_vals, origin = time_origin)

# Create a list to store data for each observation
data_list <- vector("list", num_obs)

# Loop through each observation and store chlorophyll data in a data frame
for (i in 1:num_obs) {
  data <- data.frame(chlorophyll = chl[, , i])
  data_list[[i]] <- data
}

# Function to calculate the mean chlorophyll concentration
spatav_exp3 <- function(df) {
  col_means <- sapply(df, function(x) mean(x, na.rm = TRUE))
  point_mean <- mean(col_means)
  return(point_mean)
}

# Apply the function to each data frame and store results
results <- data.frame(chlorophyll_mean = sapply(data_list, spatav_exp3))

# Interpolate missing values if necessary
interp <- function(x) {
  na_indices <- which(is.na(x))
  for (i in na_indices) {
    left_val <- ifelse(i > 1, x[i - 1], NA)
    right_val <- ifelse(i < length(x), x[i + 1], NA)
    x[i] <- mean(c(left_val, right_val), na.rm = TRUE)
  }
  return(x)
}

# Check the length of results and perform interpolation
chl_interp <- data.frame(chlorophyll_interp = interp(results$chlorophyll_mean))

# Ensure date_fin and chl_interp have the same length
if (length(date_fin) == nrow(chl_interp)) {
  chla <- data.frame(t = date_fin, Chla = chl_interp$chlorophyll_interp)
} else {
  stop("Length mismatch between dates and chlorophyll data.")
}

# Calculate the day of the year for each date
chla$day_of_year <- yday(chla$t)

# Calculate daily climatology, excluding day 366 to avoid issues with non-leap years
daily_climatology <- chla %>%
  filter(day_of_year <= 365) %>%
  group_by(day_of_year) %>%
  summarise(Chla_climatology = mean(Chla, na.rm = TRUE))

# Add dates (mm-dd format) corresponding to day of the year
daily_climatology <- daily_climatology %>%
  mutate(date = format(as.Date(day_of_year - 1, origin = "1970-01-01"), "%m-%d"))

# Arrange the dataframe in date order
daily_climatology <- daily_climatology %>%
  arrange(day_of_year) %>%
  select(day_of_year, date, Chla_climatology)

# Merge the climatology with the original data
merged_data <- merge(chla, daily_climatology, by = "day_of_year")

# Calculate anomalies
merged_data <- merged_data %>%
  mutate(anomaly = Chla - Chla_climatology)

# Calculate the 10th percentile and 90th percentile anomalies for each day across all years
daily_percentiles <- merged_data %>%
  group_by(date) %>%
  summarise(percentile_10 = quantile(anomaly, 0.1, na.rm = TRUE),
            percentile_90 = quantile(anomaly, 0.9, na.rm = TRUE))

# Calculate the mean anomaly for each day across all years
daily_mean_anomaly <- merged_data %>%
  group_by(date) %>%
  summarise(mean_anomaly = mean(anomaly, na.rm = TRUE))

# Merge the percentiles and mean anomaly data with the original data
merged_data <- merge(merged_data, daily_percentiles, by = "date")
merged_data <- merge(merged_data, daily_mean_anomaly, by = "date")

# Determine if the anomaly is in the 10% most negative or positive anomalies
merged_data <- merged_data %>%
  mutate(in_10_percentile_negative = anomaly <= percentile_10,
         in_10_percentile_positive = anomaly >= percentile_90)

# Determine if anomalies are more negative or more positive than the average anomaly for that day
merged_data <- merged_data %>%
  mutate(more_negative_than_average = anomaly < mean_anomaly,
         more_positive_than_average = anomaly > mean_anomaly)

# Ensure the data is in chronological order
merged_data <- merged_data %>%
  arrange(t)

# Select relevant columns for final output
merged_data <- merged_data %>%
  select(t, date, Chla, Chla_climatology, anomaly, 
         in_10_percentile_negative, in_10_percentile_positive, 
         mean_anomaly, more_negative_than_average, more_positive_than_average)

# Filter the data for the specified time range (1st May 2016 to 31st May 2016)
plot_data <- merged_data %>%
  filter(t >= as.Date("2016-05-01") & t <= as.Date("2016-05-31"))

# Print the values for the selected days
print(plot_data %>% select(t, in_10_percentile_negative, in_10_percentile_positive, more_negative_than_average, more_positive_than_average))

# Plotting the data with colours indicating positive or negative anomalies
ggplot(plot_data, aes(x = t, y = anomaly)) +
  geom_bar(stat = "identity", aes(fill = anomaly > 0)) +
  scale_fill_manual(values = c("FALSE" = "red3", "TRUE" = "green4"), 
                    labels = c("Negative", "Positive"), 
                    name = "Anomaly") +
  labs(title = "Chl-a Anomalies May 2016",
       x = "Time",
       y = "Anomaly (mg/m^3)",
       caption = "Bar plot representing anomalies above and below climatology") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        aspect.ratio = 0.5)+
  ylim(-1, 2.5)# Adjust aspect ratio for a narrower plot
