
#Climatology

# Load necessary libraries
library("dplyr")
library("lubridate")
library("ncdf4")
library("ggplot2")

# Clear workspace
rm(list = ls())

# Define the file name and open the NetCDF file
filename <- 'IMOS_aggregation_20240726T034923Z.nc'
nc <- nc_open('IMOS_aggregation_20240726T034923Z.nc')

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


#------------------------------------------------------------------------

#May 2019 Anoms

# Load necessary libraries
library(dplyr)
library(lubridate)
library(ncdf4)
library(ggplot2)

# Clear workspace
rm(list = ls())

# Define the file name and open the NetCDF file
filename <- 'IMOS_aggregation_20240726T034923Z.nc'
nc <- nc_open('IMOS_aggregation_20240726T034923Z.nc')

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

# Calculate the 10th percentile anomaly for each day across all years
daily_percentile_10 <- merged_data %>%
  group_by(date) %>%
  summarise(percentile_10 = quantile(anomaly, 0.1, na.rm = TRUE))

# Calculate the mean anomaly for each day across all years
daily_mean_anomaly <- merged_data %>%
  group_by(date) %>%
  summarise(mean_anomaly = mean(anomaly, na.rm = TRUE))

# Merge the 10th percentile and mean anomaly data with the original data
merged_data <- merge(merged_data, daily_percentile_10, by = "date")
merged_data <- merge(merged_data, daily_mean_anomaly, by = "date")

# Determine if the anomaly is in the 10% most negative anomalies (10th percentile)
merged_data <- merged_data %>%
  mutate(in_10_percentile = anomaly <= percentile_10)

# Ensure the data is in chronological order
merged_data <- merged_data %>%
  arrange(t)

# Select relevant columns for final output
merged_data <- merged_data %>%
  select(t, date, Chla, Chla_climatology, anomaly, in_10_percentile, mean_anomaly)

# Filter the data for the year 2019 and specific dates
plot_data <- merged_data %>%
  filter(t >= as.Date("2019-05-21") & t <= as.Date("2019-05-25"))

# Determine if anomalies are more negative than the average anomaly for that day
plot_data <- plot_data %>%
  mutate(more_negative_than_average = anomaly < mean_anomaly)

# Print the 'in_10_percentile' and 'more_negative_than_average' values for the selected days
print(plot_data %>% select(t, in_10_percentile, more_negative_than_average))

# Plotting the data with colors indicating positive or negative anomalies
ggplot(plot_data, aes(x = t, y = anomaly)) +
  geom_bar(stat = "identity", aes(fill = anomaly > 0)) +
  scale_fill_manual(values = c("FALSE" = "red3", "TRUE" = "green4"), 
                    labels = c("Negative", "Positive"), 
                    name = "Anomaly") +
  labs(title = "Chl-a Anomalies May 2019",
       x = "Time",
       y = "Anomaly (mg/m^3)",
       caption = "Bar plot representing anomalies above and below climatology") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        aspect.ratio = 0.5) # Adjust aspect ratio for a narrower plot



#April 2014 Anoms

# Load necessary libraries
library(dplyr)
library(lubridate)
library(ncdf4)
library(ggplot2)

# Clear workspace
rm(list = ls())

# Define the file name and open the NetCDF file
filename <- 'IMOS_aggregation_20240726T034923Z.nc'
nc <- nc_open('IMOS_aggregation_20240726T034923Z.nc')

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

# Calculate the 10th percentile anomaly for each day across all years
daily_percentile_10 <- merged_data %>%
  group_by(date) %>%
  summarise(percentile_10 = quantile(anomaly, 0.1, na.rm = TRUE))

# Calculate the mean anomaly for each day across all years
daily_mean_anomaly <- merged_data %>%
  group_by(date) %>%
  summarise(mean_anomaly = mean(anomaly, na.rm = TRUE))

# Merge the 10th percentile and mean anomaly data with the original data
merged_data <- merge(merged_data, daily_percentile_10, by = "date")
merged_data <- merge(merged_data, daily_mean_anomaly, by = "date")

# Determine if the anomaly is in the 10% most negative anomalies (10th percentile)
merged_data <- merged_data %>%
  mutate(in_10_percentile = anomaly <= percentile_10)

# Ensure the data is in chronological order
merged_data <- merged_data %>%
  arrange(t)

# Select relevant columns for final output
merged_data <- merged_data %>%
  select(t, date, Chla, Chla_climatology, anomaly, in_10_percentile, mean_anomaly)

# Filter the data for the desired period (April 26th, 2014 to April 30th, 2014)
plot_data <- merged_data %>%
  filter(t >= as.Date("2014-04-26") & t <= as.Date("2014-04-30"))

# Determine if anomalies are more negative than the average anomaly for that day
plot_data <- plot_data %>%
  mutate(more_negative_than_average = anomaly < mean_anomaly)

# Print the 'in_10_percentile' and 'more_negative_than_average' values for the selected days
print(plot_data %>% select(t, in_10_percentile, more_negative_than_average))

# Plotting the data with colors indicating positive or negative anomalies
ggplot(plot_data, aes(x = t, y = anomaly)) +
  geom_bar(stat = "identity", aes(fill = anomaly > 0)) +
  scale_fill_manual(values = c("FALSE" = "red3", "TRUE" = "green4"), 
                    labels = c("Negative", "Positive"), 
                    name = "Anomaly") +
  labs(title = "Chl-a Anomalies April 2014",
       x = "Time",
       y = "Anomaly (mg/m^3)",
       caption = "Bar plot representing anomalies above and below climatology") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        aspect.ratio = 0.5) # Adjust aspect ratio for a narrower plot



#October 2015 Anoms

# Load necessary libraries
library(dplyr)
library(lubridate)
library(ncdf4)
library(ggplot2)

# Clear workspace
rm(list = ls())

# Define the file name and open the NetCDF file
filename <- 'IMOS_aggregation_20240726T034923Z.nc'
nc <- nc_open('IMOS_aggregation_20240726T034923Z.nc')

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

# Calculate the 10th percentile anomaly for each day across all years
daily_percentile_10 <- merged_data %>%
  group_by(date) %>%
  summarise(percentile_10 = quantile(anomaly, 0.1, na.rm = TRUE))

# Calculate the mean anomaly for each day across all years
daily_mean_anomaly <- merged_data %>%
  group_by(date) %>%
  summarise(mean_anomaly = mean(anomaly, na.rm = TRUE))

# Merge the 10th percentile and mean anomaly data with the original data
merged_data <- merge(merged_data, daily_percentile_10, by = "date")
merged_data <- merge(merged_data, daily_mean_anomaly, by = "date")

# Determine if the anomaly is in the 10% most negative anomalies (10th percentile)
merged_data <- merged_data %>%
  mutate(in_10_percentile = anomaly <= percentile_10)

# Ensure the data is in chronological order
merged_data <- merged_data %>%
  arrange(t)

# Select relevant columns for final output
merged_data <- merged_data %>%
  select(t, date, Chla, Chla_climatology, anomaly, in_10_percentile, mean_anomaly)

# Filter the data for the desired period (October 11th, 2015 to October 16th, 2015)
plot_data <- merged_data %>%
  filter(t >= as.Date("2015-10-11") & t <= as.Date("2015-10-16"))

# Determine if anomalies are more negative than the average anomaly for that day
plot_data <- plot_data %>%
  mutate(more_negative_than_average = anomaly < mean_anomaly)

# Print the 'in_10_percentile' and 'more_negative_than_average' values for the selected days
print(plot_data %>% select(t, in_10_percentile, more_negative_than_average))

# Plotting the data with colors indicating positive or negative anomalies
ggplot(plot_data, aes(x = t, y = anomaly)) +
  geom_bar(stat = "identity", aes(fill = anomaly > 0)) +
  scale_fill_manual(values = c("FALSE" = "red3", "TRUE" = "green4"), 
                    labels = c("Negative", "Positive"), 
                    name = "Anomaly") +
  labs(title = "Chl-a Anomalies October 2015",
       x = "Time",
       y = "Anomaly (mg/m^3)",
       caption = "Bar plot representing anomalies above and below climatology") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        aspect.ratio = 0.5) # Adjust aspect ratio for a narrower plot



#September 2016 Anoms

# Load necessary libraries
library(dplyr)
library(lubridate)
library(ncdf4)
library(ggplot2)

# Clear workspace
rm(list = ls())

# Define the file name and open the NetCDF file
filename <- 'IMOS_aggregation_20240726T034923Z.nc'
nc <- nc_open('IMOS_aggregation_20240726T034923Z.nc')

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

# Calculate the 10th percentile anomaly for each day across all years
daily_percentile_10 <- merged_data %>%
  group_by(date) %>%
  summarise(percentile_10 = quantile(anomaly, 0.1, na.rm = TRUE))

# Calculate the mean anomaly for each day across all years
daily_mean_anomaly <- merged_data %>%
  group_by(date) %>%
  summarise(mean_anomaly = mean(anomaly, na.rm = TRUE))

# Merge the 10th percentile and mean anomaly data with the original data
merged_data <- merge(merged_data, daily_percentile_10, by = "date")
merged_data <- merge(merged_data, daily_mean_anomaly, by = "date")

# Determine if the anomaly is in the 10% most negative anomalies (10th percentile)
merged_data <- merged_data %>%
  mutate(in_10_percentile = anomaly <= percentile_10)

# Ensure the data is in chronological order
merged_data <- merged_data %>%
  arrange(t)

# Select relevant columns for final output
merged_data <- merged_data %>%
  select(t, date, Chla, Chla_climatology, anomaly, in_10_percentile, mean_anomaly)

# Filter the data for the desired period (September 25th, 2016 to September 29th, 2016)
plot_data <- merged_data %>%
  filter(t >= as.Date("2016-09-25") & t <= as.Date("2016-09-29"))

# Determine if anomalies are more negative than the average anomaly for that day
plot_data <- plot_data %>%
  mutate(more_negative_than_average = anomaly < mean_anomaly)

# Print the 'in_10_percentile' and 'more_negative_than_average' values for the selected days
print(plot_data %>% select(t, in_10_percentile, more_negative_than_average))

# Plotting the data with colors indicating positive or negative anomalies
ggplot(plot_data, aes(x = t, y = anomaly)) +
  geom_bar(stat = "identity", aes(fill = anomaly > 0)) +
  scale_fill_manual(values = c("FALSE" = "red3", "TRUE" = "green4"), 
                    labels = c("Negative", "Positive"), 
                    name = "Anomaly") +
  labs(title = "Chl-a Anomalies September 2016",
       x = "Time",
       y = "Anomaly (mg/m^3)",
       caption = "Bar plot representing anomalies above and below climatology") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        aspect.ratio = 0.5) # Adjust aspect ratio for a narrower plot



#April 2013 Anoms

# Load necessary libraries
library(dplyr)
library(lubridate)
library(ncdf4)
library(ggplot2)

# Clear workspace
rm(list = ls())

# Define the file name and open the NetCDF file
filename <- 'IMOS_aggregation_20240726T034923Z.nc'
nc <- nc_open(filename)

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

# Calculate the 10th and 90th percentile anomaly for each day across all years
daily_percentile_10 <- merged_data %>%
  group_by(date) %>%
  summarise(percentile_10 = quantile(anomaly, 0.1, na.rm = TRUE),
            percentile_90 = quantile(anomaly, 0.9, na.rm = TRUE))

# Calculate the mean anomaly for each day across all years
daily_mean_anomaly <- merged_data %>%
  group_by(date) %>%
  summarise(mean_anomaly = mean(anomaly, na.rm = TRUE))

# Merge the percentile and mean anomaly data with the original data
merged_data <- merge(merged_data, daily_percentile_10, by = "date")
merged_data <- merge(merged_data, daily_mean_anomaly, by = "date")

# Determine if the anomaly is in the 10% most negative anomalies (10th percentile)
# and 10% most positive anomalies (90th percentile)
merged_data <- merged_data %>%
  mutate(in_10_percentile_negative = anomaly <= percentile_10,
         in_10_percentile_positive = anomaly >= percentile_90,
         more_negative_than_average = anomaly < mean_anomaly,
         more_positive_than_average = anomaly > mean_anomaly)

# Ensure the data is in chronological order
merged_data <- merged_data %>%
  arrange(t)

# Select relevant columns for final output
merged_data <- merged_data %>%
  select(t, date, Chla, Chla_climatology, anomaly, in_10_percentile_negative, 
         in_10_percentile_positive, mean_anomaly, more_negative_than_average, more_positive_than_average)

# Filter the data for the desired period (April 20th, 2013 to April 25th, 2013)
plot_data <- merged_data %>%
  filter(t >= as.Date("2013-04-20") & t <= as.Date("2013-04-25"))

# Print the 'in_10_percentile_negative', 'in_10_percentile_positive', 
# 'more_negative_than_average', and 'more_positive_than_average' values for the selected days
print(plot_data %>% select(t, in_10_percentile_negative, in_10_percentile_positive, 
                           more_negative_than_average, more_positive_than_average))

# Plotting the data with colors indicating positive or negative anomalies
ggplot(plot_data, aes(x = t, y = anomaly)) +
  geom_bar(stat = "identity", aes(fill = anomaly > 0)) +
  scale_fill_manual(values = c("FALSE" = "red3", "TRUE" = "green4"), 
                    labels = c("Negative", "Positive"), 
                    name = "Anomaly") +
  labs(title = "Chl-a Anomalies April 2013",
       x = "Time",
       y = "Anomaly (mg/m^3)",
       caption = "Bar plot representing anomalies above and below climatology") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        aspect.ratio = 0.5) # Adjust aspect ratio for a narrower plot





#October 2014 Anoms

# Load necessary libraries
library(dplyr)
library(lubridate)
library(ncdf4)
library(ggplot2)

# Clear workspace
rm(list = ls())

# Define the file name and open the NetCDF file
filename <- 'IMOS_aggregation_20240726T034923Z.nc'
nc <- nc_open('IMOS_aggregation_20240726T034923Z.nc')

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

# Calculate the 10th and 90th percentile anomaly for each day across all years
daily_percentile_10 <- merged_data %>%
  group_by(date) %>%
  summarise(percentile_10 = quantile(anomaly, 0.1, na.rm = TRUE),
            percentile_90 = quantile(anomaly, 0.9, na.rm = TRUE))

# Calculate the mean anomaly for each day across all years
daily_mean_anomaly <- merged_data %>%
  group_by(date) %>%
  summarise(mean_anomaly = mean(anomaly, na.rm = TRUE))

# Merge the percentile and mean anomaly data with the original data
merged_data <- merge(merged_data, daily_percentile_10, by = "date")
merged_data <- merge(merged_data, daily_mean_anomaly, by = "date")

# Determine if the anomaly is in the 10% most negative anomalies (10th percentile)
# and 10% most positive anomalies (90th percentile)
merged_data <- merged_data %>%
  mutate(in_10_percentile_negative = anomaly <= percentile_10,
         in_10_percentile_positive = anomaly >= percentile_90,
         more_negative_than_average = anomaly < mean_anomaly,
         more_positive_than_average = anomaly > mean_anomaly)

# Ensure the data is in chronological order
merged_data <- merged_data %>%
  arrange(t)

# Select relevant columns for final output
merged_data <- merged_data %>%
  select(t, date, Chla, Chla_climatology, anomaly, in_10_percentile_negative, 
         in_10_percentile_positive, mean_anomaly, more_negative_than_average, more_positive_than_average)

# Filter the data for the desired period (October 20th, 2014 to October 24th, 2014)
plot_data <- merged_data %>%
  filter(t >= as.Date("2014-10-20") & t <= as.Date("2014-10-24"))

# Print the 'in_10_percentile_negative', 'in_10_percentile_positive', 
# 'more_negative_than_average', and 'more_positive_than_average' values for the selected days
print(plot_data %>% select(t, in_10_percentile_negative, in_10_percentile_positive, 
                           more_negative_than_average, more_positive_than_average))

# Plotting the data with colors indicating positive or negative anomalies
ggplot(plot_data, aes(x = t, y = anomaly)) +
  geom_bar(stat = "identity", aes(fill = anomaly > 0)) +
  scale_fill_manual(values = c("FALSE" = "red3", "TRUE" = "green4"), 
                    labels = c("Negative", "Positive"), 
                    name = "Anomaly") +
  labs(title = "Chl-a Anomalies October 2014",
       x = "Time",
       y = "Anomaly (mg/m^3)",
       caption = "Bar plot representing anomalies above and below climatology") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        aspect.ratio = 0.5) # Adjust aspect ratio for a narrower plot
