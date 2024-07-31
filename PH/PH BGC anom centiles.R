
# NITRATE

#Apr 28 2014
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the CSV file
nitrate_data <- read_csv("BCG_PH_2009_2023.csv")

# Convert the Time column to Date type
nitrate_data$Time <- ymd(nitrate_data$Time)

# Extract the month from the Time column
nitrate_data$Month <- month(nitrate_data$Time, label = TRUE, abbr = TRUE)

# Calculate the monthly climatology for Nitrate at each Depth
monthly_climatology <- nitrate_data %>%
  group_by(Depth, Month) %>%
  summarise(Average_Nitrate = mean(Nitrate, na.rm = TRUE)) %>%
  ungroup()

# Join the climatology with the original data to calculate anomalies
nitrate_anomalies <- nitrate_data %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Nitrate_Anomaly = Nitrate - Average_Nitrate) %>%
  select(-Average_Nitrate)

# Get unique depths from the data
depths <- unique(nitrate_anomalies$Depth)

# Initialize a data frame to store results
results <- data.frame(Depth = numeric(), Is_In_10th_Percentile_Negative = logical(), Is_In_10th_Percentile_Positive = logical())

# Loop over each depth
for (depth in depths) {
  # Filter data for anomalies in April at the current depth
  april_anomalies_at_depth <- nitrate_anomalies %>%
    filter(Depth == depth, Month == "Apr") %>%
    select(Time, Nitrate_Anomaly)
  
  # Find the anomaly value on April 28th, 2014 at the current depth
  anomaly_april_28_2014 <- april_anomalies_at_depth %>%
    filter(Time == ymd("2014-04-28")) %>%
    pull(Nitrate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for April at the current depth
  percentile_10_negative <- quantile(april_anomalies_at_depth$Nitrate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for April at the current depth
  percentile_90_positive <- quantile(april_anomalies_at_depth$Nitrate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on April 28th, 2014 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_april_28_2014 <= percentile_10_negative
  
  # Check if the anomaly on April 28th, 2014 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_april_28_2014 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)


#May 23 2019
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the CSV file
nitrate_data <- read_csv("BCG_PH_2009_2023.csv")

# Convert the Time column to Date type
nitrate_data$Time <- ymd(nitrate_data$Time)

# Extract the month from the Time column
nitrate_data$Month <- month(nitrate_data$Time, label = TRUE, abbr = TRUE)

# Calculate the monthly climatology for Nitrate at each Depth
monthly_climatology <- nitrate_data %>%
  group_by(Depth, Month) %>%
  summarise(Average_Nitrate = mean(Nitrate, na.rm = TRUE)) %>%
  ungroup()

# Join the climatology with the original data to calculate anomalies
nitrate_anomalies <- nitrate_data %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Nitrate_Anomaly = Nitrate - Average_Nitrate) %>%
  select(-Average_Nitrate)

# Get unique depths from the data
depths <- unique(nitrate_anomalies$Depth)

# Initialize a data frame to store results
results <- data.frame(Depth = numeric(), Is_In_10th_Percentile_Negative = logical(), Is_In_10th_Percentile_Positive = logical())

# Loop over each depth
for (depth in depths) {
  # Filter data for anomalies in May at the current depth
  may_anomalies_at_depth <- nitrate_anomalies %>%
    filter(Depth == depth, Month == "May") %>%
    select(Time, Nitrate_Anomaly)
  
  # Find the anomaly value on May 23rd, 2019 at the current depth
  anomaly_may_23_2019 <- may_anomalies_at_depth %>%
    filter(Time == ymd("2019-05-23")) %>%
    pull(Nitrate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for May at the current depth
  percentile_10_negative <- quantile(may_anomalies_at_depth$Nitrate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for May at the current depth
  percentile_90_positive <- quantile(may_anomalies_at_depth$Nitrate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on May 23rd, 2019 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_may_23_2019 <= percentile_10_negative
  
  # Check if the anomaly on May 23rd, 2019 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_may_23_2019 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)


#Oct 14 2015
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the CSV file
nitrate_data <- read_csv("BCG_PH_2009_2023.csv")

# Convert the Time column to Date type
nitrate_data$Time <- ymd(nitrate_data$Time)

# Extract the month from the Time column
nitrate_data$Month <- month(nitrate_data$Time, label = TRUE, abbr = TRUE)

# Calculate the monthly climatology for Nitrate at each Depth
monthly_climatology <- nitrate_data %>%
  group_by(Depth, Month) %>%
  summarise(Average_Nitrate = mean(Nitrate, na.rm = TRUE)) %>%
  ungroup()

# Join the climatology with the original data to calculate anomalies
nitrate_anomalies <- nitrate_data %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Nitrate_Anomaly = Nitrate - Average_Nitrate) %>%
  select(-Average_Nitrate)

# Get unique depths from the data
depths <- unique(nitrate_anomalies$Depth)

# Initialize a data frame to store results
results <- data.frame(Depth = numeric(), Is_In_10th_Percentile_Negative = logical(), Is_In_10th_Percentile_Positive = logical())

# Loop over each depth
for (depth in depths) {
  # Filter data for anomalies in October at the current depth
  october_anomalies_at_depth <- nitrate_anomalies %>%
    filter(Depth == depth, Month == "Oct") %>%
    select(Time, Nitrate_Anomaly)
  
  # Find the anomaly value on October 14th, 2015 at the current depth
  anomaly_october_14_2015 <- october_anomalies_at_depth %>%
    filter(Time == ymd("2015-10-14")) %>%
    pull(Nitrate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for October at the current depth
  percentile_10_negative <- quantile(october_anomalies_at_depth$Nitrate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for October at the current depth
  percentile_90_positive <- quantile(october_anomalies_at_depth$Nitrate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on October 14th, 2015 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_october_14_2015 <= percentile_10_negative
  
  # Check if the anomaly on October 14th, 2015 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_october_14_2015 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)


#Sept 27 2016
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the CSV file
nitrate_data <- read_csv("BCG_PH_2009_2023.csv")

# Convert the Time column to Date type
nitrate_data$Time <- ymd(nitrate_data$Time)

# Extract the month from the Time column
nitrate_data$Month <- month(nitrate_data$Time, label = TRUE, abbr = TRUE)

# Calculate the monthly climatology for Nitrate at each Depth
monthly_climatology <- nitrate_data %>%
  group_by(Depth, Month) %>%
  summarise(Average_Nitrate = mean(Nitrate, na.rm = TRUE)) %>%
  ungroup()

# Join the climatology with the original data to calculate anomalies
nitrate_anomalies <- nitrate_data %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Nitrate_Anomaly = Nitrate - Average_Nitrate) %>%
  select(-Average_Nitrate)

# Get unique depths from the data
depths <- unique(nitrate_anomalies$Depth)

# Initialize a data frame to store results
results <- data.frame(Depth = numeric(), Is_In_10th_Percentile_Negative = logical(), Is_In_10th_Percentile_Positive = logical())

# Loop over each depth
for (depth in depths) {
  # Filter data for anomalies in September at the current depth
  september_anomalies_at_depth <- nitrate_anomalies %>%
    filter(Depth == depth, Month == "Sep") %>%
    select(Time, Nitrate_Anomaly)
  
  # Find the anomaly value on September 27th, 2016 at the current depth
  anomaly_september_27_2016 <- september_anomalies_at_depth %>%
    filter(Time == ymd("2016-09-27")) %>%
    pull(Nitrate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for September at the current depth
  percentile_10_negative <- quantile(september_anomalies_at_depth$Nitrate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for September at the current depth
  percentile_90_positive <- quantile(september_anomalies_at_depth$Nitrate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on September 27th, 2016 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_september_27_2016 <= percentile_10_negative
  
  # Check if the anomaly on September 27th, 2016 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_september_27_2016 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)


#Apr 23 2013
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the CSV file
nitrate_data <- read_csv("BCG_PH_2009_2023.csv")

# Convert the Time column to Date type
nitrate_data$Time <- ymd(nitrate_data$Time)

# Extract the month from the Time column
nitrate_data$Month <- month(nitrate_data$Time, label = TRUE, abbr = TRUE)

# Calculate the monthly climatology for Nitrate at each Depth
monthly_climatology <- nitrate_data %>%
  group_by(Depth, Month) %>%
  summarise(Average_Nitrate = mean(Nitrate, na.rm = TRUE)) %>%
  ungroup()

# Join the climatology with the original data to calculate anomalies
nitrate_anomalies <- nitrate_data %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Nitrate_Anomaly = Nitrate - Average_Nitrate) %>%
  select(-Average_Nitrate)

# Get unique depths from the data
depths <- unique(nitrate_anomalies$Depth)

# Initialize a data frame to store results
results <- data.frame(Depth = numeric(), Is_In_10th_Percentile_Negative = logical(), Is_In_10th_Percentile_Positive = logical())

# Loop over each depth
for (depth in depths) {
  # Filter data for anomalies in April at the current depth
  april_anomalies_at_depth <- nitrate_anomalies %>%
    filter(Depth == depth, Month == "Apr") %>%
    select(Time, Nitrate_Anomaly)
  
  # Find the anomaly value on April 23rd, 2013 at the current depth
  anomaly_april_23_2013 <- april_anomalies_at_depth %>%
    filter(Time == ymd("2013-04-23")) %>%
    pull(Nitrate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for April at the current depth
  percentile_10_negative <- quantile(april_anomalies_at_depth$Nitrate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for April at the current depth
  percentile_90_positive <- quantile(april_anomalies_at_depth$Nitrate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on April 23rd, 2013 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_april_23_2013 <= percentile_10_negative
  
  # Check if the anomaly on April 23rd, 2013 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_april_23_2013 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)


#Oct 22 2014
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the CSV file
nitrate_data <- read_csv("BCG_PH_2009_2023.csv")

# Convert the Time column to Date type
nitrate_data$Time <- ymd(nitrate_data$Time)

# Extract the month from the Time column
nitrate_data$Month <- month(nitrate_data$Time, label = TRUE, abbr = TRUE)

# Calculate the monthly climatology for Nitrate at each Depth
monthly_climatology <- nitrate_data %>%
  group_by(Depth, Month) %>%
  summarise(Average_Nitrate = mean(Nitrate, na.rm = TRUE)) %>%
  ungroup()

# Join the climatology with the original data to calculate anomalies
nitrate_anomalies <- nitrate_data %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Nitrate_Anomaly = Nitrate - Average_Nitrate) %>%
  select(-Average_Nitrate)

# Get unique depths from the data
depths <- unique(nitrate_anomalies$Depth)

# Initialize a data frame to store results
results <- data.frame(Depth = numeric(), Is_In_10th_Percentile_Negative = logical(), Is_In_10th_Percentile_Positive = logical())

# Loop over each depth
for (depth in depths) {
  # Filter data for anomalies in October at the current depth
  october_anomalies_at_depth <- nitrate_anomalies %>%
    filter(Depth == depth, Month == "Oct") %>%
    select(Time, Nitrate_Anomaly)
  
  # Find the anomaly value on October 22nd, 2014 at the current depth
  anomaly_october_22_2014 <- october_anomalies_at_depth %>%
    filter(Time == ymd("2014-10-22")) %>%
    pull(Nitrate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for October at the current depth
  percentile_10_negative <- quantile(october_anomalies_at_depth$Nitrate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for October at the current depth
  percentile_90_positive <- quantile(october_anomalies_at_depth$Nitrate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on October 22nd, 2014 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_october_22_2014 <= percentile_10_negative
  
  # Check if the anomaly on October 22nd, 2014 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_october_22_2014 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)


#-------------------------------------------------------------------------------

#Phosphate

#April 28 2014
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the CSV file
phosphate_data <- read_csv("BCG_PH_2009_2023.csv")

# Convert the Time column to Date type
phosphate_data$Time <- ymd(phosphate_data$Time)

# Extract the month from the Time column
phosphate_data$Month <- month(phosphate_data$Time, label = TRUE, abbr = TRUE)

# Calculate the monthly climatology for Phosphate at each Depth
monthly_climatology <- phosphate_data %>%
  group_by(Depth, Month) %>%
  summarise(Average_Phosphate = mean(Phosphate, na.rm = TRUE)) %>%
  ungroup()

# Join the climatology with the original data to calculate anomalies
phosphate_anomalies <- phosphate_data %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Phosphate_Anomaly = Phosphate - Average_Phosphate) %>%
  select(-Average_Phosphate)

# Get unique depths from the data
depths <- unique(phosphate_anomalies$Depth)

# Initialize a data frame to store results
results <- data.frame(Depth = numeric(), Is_In_10th_Percentile_Negative = logical(), Is_In_10th_Percentile_Positive = logical())

# Loop over each depth
for (depth in depths) {
  # Filter data for anomalies in April at the current depth
  april_anomalies_at_depth <- phosphate_anomalies %>%
    filter(Depth == depth, Month == "Apr") %>%
    select(Time, Phosphate_Anomaly)
  
  # Find the anomaly value on April 28th, 2014 at the current depth
  anomaly_april_28_2014 <- april_anomalies_at_depth %>%
    filter(Time == ymd("2014-04-28")) %>%
    pull(Phosphate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for April at the current depth
  percentile_10_negative <- quantile(april_anomalies_at_depth$Phosphate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for April at the current depth
  percentile_90_positive <- quantile(april_anomalies_at_depth$Phosphate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on April 28th, 2014 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_april_28_2014 <= percentile_10_negative
  
  # Check if the anomaly on April 28th, 2014 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_april_28_2014 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)



#May 23 2019
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the CSV file
phosphate_data <- read_csv("BCG_PH_2009_2023.csv")

# Convert the Time column to Date type
phosphate_data$Time <- ymd(phosphate_data$Time)

# Extract the month from the Time column
phosphate_data$Month <- month(phosphate_data$Time, label = TRUE, abbr = TRUE)

# Calculate the monthly climatology for Phosphate at each Depth
monthly_climatology <- phosphate_data %>%
  group_by(Depth, Month) %>%
  summarise(Average_Phosphate = mean(Phosphate, na.rm = TRUE)) %>%
  ungroup()

# Join the climatology with the original data to calculate anomalies
phosphate_anomalies <- phosphate_data %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Phosphate_Anomaly = Phosphate - Average_Phosphate) %>%
  select(-Average_Phosphate)

# Get unique depths from the data
depths <- unique(phosphate_anomalies$Depth)

# Initialize a data frame to store results
results <- data.frame(Depth = numeric(), Is_In_10th_Percentile_Negative = logical(), Is_In_10th_Percentile_Positive = logical())

# Loop over each depth
for (depth in depths) {
  # Filter data for anomalies in May at the current depth
  may_anomalies_at_depth <- phosphate_anomalies %>%
    filter(Depth == depth, Month == "May") %>%
    select(Time, Phosphate_Anomaly)
  
  # Find the anomaly value on May 23, 2019 at the current depth
  anomaly_may_23_2019 <- may_anomalies_at_depth %>%
    filter(Time == ymd("2019-05-23")) %>%
    pull(Phosphate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for May at the current depth
  percentile_10_negative <- quantile(may_anomalies_at_depth$Phosphate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for May at the current depth
  percentile_90_positive <- quantile(may_anomalies_at_depth$Phosphate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on May 23, 2019 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_may_23_2019 <= percentile_10_negative
  
  # Check if the anomaly on May 23, 2019 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_may_23_2019 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)



#October 14 2015
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the CSV file
phosphate_data <- read_csv("BCG_PH_2009_2023.csv")

# Convert the Time column to Date type
phosphate_data$Time <- ymd(phosphate_data$Time)

# Extract the month from the Time column
phosphate_data$Month <- month(phosphate_data$Time, label = TRUE, abbr = TRUE)

# Calculate the monthly climatology for Phosphate at each Depth
monthly_climatology <- phosphate_data %>%
  group_by(Depth, Month) %>%
  summarise(Average_Phosphate = mean(Phosphate, na.rm = TRUE)) %>%
  ungroup()

# Join the climatology with the original data to calculate anomalies
phosphate_anomalies <- phosphate_data %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Phosphate_Anomaly = Phosphate - Average_Phosphate) %>%
  select(-Average_Phosphate)

# Get unique depths from the data
depths <- unique(phosphate_anomalies$Depth)

# Initialize a data frame to store results
results <- data.frame(Depth = numeric(), Is_In_10th_Percentile_Negative = logical(), Is_In_10th_Percentile_Positive = logical())

# Loop over each depth
for (depth in depths) {
  # Filter data for anomalies in October at the current depth
  october_anomalies_at_depth <- phosphate_anomalies %>%
    filter(Depth == depth, Month == "Oct") %>%
    select(Time, Phosphate_Anomaly)
  
  # Find the anomaly value on October 14, 2015 at the current depth
  anomaly_october_14_2015 <- october_anomalies_at_depth %>%
    filter(Time == ymd("2015-10-14")) %>%
    pull(Phosphate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for October at the current depth
  percentile_10_negative <- quantile(october_anomalies_at_depth$Phosphate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for October at the current depth
  percentile_90_positive <- quantile(october_anomalies_at_depth$Phosphate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on October 14, 2015 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_october_14_2015 <= percentile_10_negative
  
  # Check if the anomaly on October 14, 2015 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_october_14_2015 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)



#September 27 2016
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the CSV file
phosphate_data <- read_csv("BCG_PH_2009_2023.csv")

# Convert the Time column to Date type
phosphate_data$Time <- ymd(phosphate_data$Time)

# Extract the month from the Time column
phosphate_data$Month <- month(phosphate_data$Time, label = TRUE, abbr = TRUE)

# Calculate the monthly climatology for Phosphate at each Depth
monthly_climatology <- phosphate_data %>%
  group_by(Depth, Month) %>%
  summarise(Average_Phosphate = mean(Phosphate, na.rm = TRUE)) %>%
  ungroup()

# Join the climatology with the original data to calculate anomalies
phosphate_anomalies <- phosphate_data %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Phosphate_Anomaly = Phosphate - Average_Phosphate) %>%
  select(-Average_Phosphate)

# Get unique depths from the data
depths <- unique(phosphate_anomalies$Depth)

# Initialize a data frame to store results
results <- data.frame(Depth = numeric(), Is_In_10th_Percentile_Negative = logical(), Is_In_10th_Percentile_Positive = logical())

# Loop over each depth
for (depth in depths) {
  # Filter data for anomalies in September at the current depth
  september_anomalies_at_depth <- phosphate_anomalies %>%
    filter(Depth == depth, Month == "Sep") %>%
    select(Time, Phosphate_Anomaly)
  
  # Find the anomaly value on September 27, 2016 at the current depth
  anomaly_september_27_2016 <- september_anomalies_at_depth %>%
    filter(Time == ymd("2016-09-27")) %>%
    pull(Phosphate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for September at the current depth
  percentile_10_negative <- quantile(september_anomalies_at_depth$Phosphate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for September at the current depth
  percentile_90_positive <- quantile(september_anomalies_at_depth$Phosphate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on September 27, 2016 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_september_27_2016 <= percentile_10_negative
  
  # Check if the anomaly on September 27, 2016 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_september_27_2016 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)



#April 23 2013
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the CSV file
phosphate_data <- read_csv("BCG_PH_2009_2023.csv")

# Convert the Time column to Date type
phosphate_data$Time <- ymd(phosphate_data$Time)

# Extract the month from the Time column
phosphate_data$Month <- month(phosphate_data$Time, label = TRUE, abbr = TRUE)

# Calculate the monthly climatology for Phosphate at each Depth
monthly_climatology <- phosphate_data %>%
  group_by(Depth, Month) %>%
  summarise(Average_Phosphate = mean(Phosphate, na.rm = TRUE)) %>%
  ungroup()

# Join the climatology with the original data to calculate anomalies
phosphate_anomalies <- phosphate_data %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Phosphate_Anomaly = Phosphate - Average_Phosphate) %>%
  select(-Average_Phosphate)

# Get unique depths from the data
depths <- unique(phosphate_anomalies$Depth)

# Initialize a data frame to store results
results <- data.frame(Depth = numeric(), Is_In_10th_Percentile_Negative = logical(), Is_In_10th_Percentile_Positive = logical())

# Loop over each depth
for (depth in depths) {
  # Filter data for anomalies in April at the current depth
  april_anomalies_at_depth <- phosphate_anomalies %>%
    filter(Depth == depth, Month == "Apr") %>%
    select(Time, Phosphate_Anomaly)
  
  # Find the anomaly value on April 23, 2013 at the current depth
  anomaly_april_23_2013 <- april_anomalies_at_depth %>%
    filter(Time == ymd("2013-04-23")) %>%
    pull(Phosphate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for April at the current depth
  percentile_10_negative <- quantile(april_anomalies_at_depth$Phosphate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for April at the current depth
  percentile_90_positive <- quantile(april_anomalies_at_depth$Phosphate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on April 23, 2013 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_april_23_2013 <= percentile_10_negative
  
  # Check if the anomaly on April 23, 2013 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_april_23_2013 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)



#October 22 2014
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the CSV file
phosphate_data <- read_csv("BCG_PH_2009_2023.csv")

# Convert the Time column to Date type
phosphate_data$Time <- ymd(phosphate_data$Time)

# Extract the month from the Time column
phosphate_data$Month <- month(phosphate_data$Time, label = TRUE, abbr = TRUE)

# Calculate the monthly climatology for Phosphate at each Depth
monthly_climatology <- phosphate_data %>%
  group_by(Depth, Month) %>%
  summarise(Average_Phosphate = mean(Phosphate, na.rm = TRUE)) %>%
  ungroup()

# Join the climatology with the original data to calculate anomalies
phosphate_anomalies <- phosphate_data %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Phosphate_Anomaly = Phosphate - Average_Phosphate) %>%
  select(-Average_Phosphate)

# Get unique depths from the data
depths <- unique(phosphate_anomalies$Depth)

# Initialize a data frame to store results
results <- data.frame(Depth = numeric(), Is_In_10th_Percentile_Negative = logical(), Is_In_10th_Percentile_Positive = logical())

# Loop over each depth
for (depth in depths) {
  # Filter data for anomalies in October at the current depth
  october_anomalies_at_depth <- phosphate_anomalies %>%
    filter(Depth == depth, Month == "Oct") %>%
    select(Time, Phosphate_Anomaly)
  
  # Find the anomaly value on October 22, 2014 at the current depth
  anomaly_october_22_2014 <- october_anomalies_at_depth %>%
    filter(Time == ymd("2014-10-22")) %>%
    pull(Phosphate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for October at the current depth
  percentile_10_negative <- quantile(october_anomalies_at_depth$Phosphate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for October at the current depth
  percentile_90_positive <- quantile(october_anomalies_at_depth$Phosphate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on October 22, 2014 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_october_22_2014 <= percentile_10_negative
  
  # Check if the anomaly on October 22, 2014 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_october_22_2014 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)


#-------------------------------------------------------------------------------

#Silicate

#April 28 2014
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the CSV file
silicate_data <- read_csv("BCG_PH_2009_2023.csv")

# Convert the Time column to Date type
silicate_data$Time <- ymd(silicate_data$Time)

# Extract the month from the Time column
silicate_data$Month <- month(silicate_data$Time, label = TRUE, abbr = TRUE)

# Calculate the monthly climatology for Silicate at each Depth
monthly_climatology <- silicate_data %>%
  group_by(Depth, Month) %>%
  summarise(Average_Silicate = mean(Silicate, na.rm = TRUE)) %>%
  ungroup()

# Join the climatology with the original data to calculate anomalies
silicate_anomalies <- silicate_data %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Silicate_Anomaly = Silicate - Average_Silicate) %>%
  select(-Average_Silicate)

# Get unique depths from the data
depths <- unique(silicate_anomalies$Depth)

# Initialize a data frame to store results
results <- data.frame(Depth = numeric(), Is_In_10th_Percentile_Negative = logical(), Is_In_10th_Percentile_Positive = logical())

# Loop over each depth
for (depth in depths) {
  # Filter data for anomalies in April at the current depth
  april_anomalies_at_depth <- silicate_anomalies %>%
    filter(Depth == depth, Month == "Apr") %>%
    select(Time, Silicate_Anomaly)
  
  # Find the anomaly value on April 28th, 2014 at the current depth
  anomaly_april_28_2014 <- april_anomalies_at_depth %>%
    filter(Time == ymd("2014-04-28")) %>%
    pull(Silicate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for April at the current depth
  percentile_10_negative <- quantile(april_anomalies_at_depth$Silicate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for April at the current depth
  percentile_90_positive <- quantile(april_anomalies_at_depth$Silicate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on April 28th, 2014 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_april_28_2014 <= percentile_10_negative
  
  # Check if the anomaly on April 28th, 2014 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_april_28_2014 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)



#May 23 2019
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the CSV file
silicate_data <- read_csv("BCG_PH_2009_2023.csv")

# Convert the Time column to Date type
silicate_data$Time <- ymd(silicate_data$Time)

# Extract the month from the Time column
silicate_data$Month <- month(silicate_data$Time, label = TRUE, abbr = TRUE)

# Calculate the monthly climatology for Silicate at each Depth
monthly_climatology <- silicate_data %>%
  group_by(Depth, Month) %>%
  summarise(Average_Silicate = mean(Silicate, na.rm = TRUE)) %>%
  ungroup()

# Join the climatology with the original data to calculate anomalies
silicate_anomalies <- silicate_data %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Silicate_Anomaly = Silicate - Average_Silicate) %>%
  select(-Average_Silicate)

# Get unique depths from the data
depths <- unique(silicate_anomalies$Depth)

# Initialize a data frame to store results
results <- data.frame(Depth = numeric(), Is_In_10th_Percentile_Negative = logical(), Is_In_10th_Percentile_Positive = logical())

# Loop over each depth
for (depth in depths) {
  # Filter data for anomalies in May at the current depth
  may_anomalies_at_depth <- silicate_anomalies %>%
    filter(Depth == depth, Month == "May") %>%
    select(Time, Silicate_Anomaly)
  
  # Find the anomaly value on May 23rd, 2019 at the current depth
  anomaly_may_23_2019 <- may_anomalies_at_depth %>%
    filter(Time == ymd("2019-05-23")) %>%
    pull(Silicate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for May at the current depth
  percentile_10_negative <- quantile(may_anomalies_at_depth$Silicate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for May at the current depth
  percentile_90_positive <- quantile(may_anomalies_at_depth$Silicate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on May 23rd, 2019 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_may_23_2019 <= percentile_10_negative
  
  # Check if the anomaly on May 23rd, 2019 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_may_23_2019 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)



#October 14 2015
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the CSV file
silicate_data <- read_csv("BCG_PH_2009_2023.csv")

# Convert the Time column to Date type
silicate_data$Time <- ymd(silicate_data$Time)

# Extract the month from the Time column
silicate_data$Month <- month(silicate_data$Time, label = TRUE, abbr = TRUE)

# Calculate the monthly climatology for Silicate at each Depth
monthly_climatology <- silicate_data %>%
  group_by(Depth, Month) %>%
  summarise(Average_Silicate = mean(Silicate, na.rm = TRUE)) %>%
  ungroup()

# Join the climatology with the original data to calculate anomalies
silicate_anomalies <- silicate_data %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Silicate_Anomaly = Silicate - Average_Silicate) %>%
  select(-Average_Silicate)

# Get unique depths from the data
depths <- unique(silicate_anomalies$Depth)

# Initialize a data frame to store results
results <- data.frame(Depth = numeric(), Is_In_10th_Percentile_Negative = logical(), Is_In_10th_Percentile_Positive = logical())

# Loop over each depth
for (depth in depths) {
  # Filter data for anomalies in October at the current depth
  october_anomalies_at_depth <- silicate_anomalies %>%
    filter(Depth == depth, Month == "Oct") %>%
    select(Time, Silicate_Anomaly)
  
  # Find the anomaly value on October 14th, 2015 at the current depth
  anomaly_october_14_2015 <- october_anomalies_at_depth %>%
    filter(Time == ymd("2015-10-14")) %>%
    pull(Silicate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for October at the current depth
  percentile_10_negative <- quantile(october_anomalies_at_depth$Silicate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for October at the current depth
  percentile_90_positive <- quantile(october_anomalies_at_depth$Silicate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on October 14th, 2015 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_october_14_2015 <= percentile_10_negative
  
  # Check if the anomaly on October 14th, 2015 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_october_14_2015 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)



#September 27 2016
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the CSV file
silicate_data <- read_csv("BCG_PH_2009_2023.csv")

# Convert the Time column to Date type
silicate_data$Time <- ymd(silicate_data$Time)

# Extract the month from the Time column
silicate_data$Month <- month(silicate_data$Time, label = TRUE, abbr = TRUE)

# Calculate the monthly climatology for Silicate at each Depth
monthly_climatology <- silicate_data %>%
  group_by(Depth, Month) %>%
  summarise(Average_Silicate = mean(Silicate, na.rm = TRUE)) %>%
  ungroup()

# Join the climatology with the original data to calculate anomalies
silicate_anomalies <- silicate_data %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Silicate_Anomaly = Silicate - Average_Silicate) %>%
  select(-Average_Silicate)

# Get unique depths from the data
depths <- unique(silicate_anomalies$Depth)

# Initialize a data frame to store results
results <- data.frame(Depth = numeric(), Is_In_10th_Percentile_Negative = logical(), Is_In_10th_Percentile_Positive = logical())

# Loop over each depth
for (depth in depths) {
  # Filter data for anomalies in September at the current depth
  september_anomalies_at_depth <- silicate_anomalies %>%
    filter(Depth == depth, Month == "Sep") %>%
    select(Time, Silicate_Anomaly)
  
  # Find the anomaly value on September 27th, 2016 at the current depth
  anomaly_september_27_2016 <- september_anomalies_at_depth %>%
    filter(Time == ymd("2016-09-27")) %>%
    pull(Silicate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for September at the current depth
  percentile_10_negative <- quantile(september_anomalies_at_depth$Silicate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for September at the current depth
  percentile_90_positive <- quantile(september_anomalies_at_depth$Silicate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on September 27th, 2016 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_september_27_2016 <= percentile_10_negative
  
  # Check if the anomaly on September 27th, 2016 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_september_27_2016 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)



#April 23 2013
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the CSV file
silicate_data <- read_csv("BCG_PH_2009_2023.csv")

# Convert the Time column to Date type
silicate_data$Time <- ymd(silicate_data$Time)

# Extract the month from the Time column
silicate_data$Month <- month(silicate_data$Time, label = TRUE, abbr = TRUE)

# Calculate the monthly climatology for Silicate at each Depth
monthly_climatology <- silicate_data %>%
  group_by(Depth, Month) %>%
  summarise(Average_Silicate = mean(Silicate, na.rm = TRUE)) %>%
  ungroup()

# Join the climatology with the original data to calculate anomalies
silicate_anomalies <- silicate_data %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Silicate_Anomaly = Silicate - Average_Silicate) %>%
  select(-Average_Silicate)

# Get unique depths from the data
depths <- unique(silicate_anomalies$Depth)

# Initialize a data frame to store results
results <- data.frame(Depth = numeric(), Is_In_10th_Percentile_Negative = logical(), Is_In_10th_Percentile_Positive = logical())

# Loop over each depth
for (depth in depths) {
  # Filter data for anomalies in April at the current depth
  april_anomalies_at_depth <- silicate_anomalies %>%
    filter(Depth == depth, Month == "Apr") %>%
    select(Time, Silicate_Anomaly)
  
  # Find the anomaly value on April 23rd, 2013 at the current depth
  anomaly_april_23_2013 <- april_anomalies_at_depth %>%
    filter(Time == ymd("2013-04-23")) %>%
    pull(Silicate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for April at the current depth
  percentile_10_negative <- quantile(april_anomalies_at_depth$Silicate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for April at the current depth
  percentile_90_positive <- quantile(april_anomalies_at_depth$Silicate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on April 23rd, 2013 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_april_23_2013 <= percentile_10_negative
  
  # Check if the anomaly on April 23rd, 2013 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_april_23_2013 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)



#October 22 2014
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the CSV file
silicate_data <- read_csv("BCG_PH_2009_2023.csv")

# Convert the Time column to Date type
silicate_data$Time <- ymd(silicate_data$Time)

# Extract the month from the Time column
silicate_data$Month <- month(silicate_data$Time, label = TRUE, abbr = TRUE)

# Calculate the monthly climatology for Silicate at each Depth
monthly_climatology <- silicate_data %>%
  group_by(Depth, Month) %>%
  summarise(Average_Silicate = mean(Silicate, na.rm = TRUE)) %>%
  ungroup()

# Join the climatology with the original data to calculate anomalies
silicate_anomalies <- silicate_data %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Silicate_Anomaly = Silicate - Average_Silicate) %>%
  select(-Average_Silicate)

# Get unique depths from the data
depths <- unique(silicate_anomalies$Depth)

# Initialize a data frame to store results
results <- data.frame(Depth = numeric(), Is_In_10th_Percentile_Negative = logical(), Is_In_10th_Percentile_Positive = logical())

# Loop over each depth
for (depth in depths) {
  # Filter data for anomalies in October at the current depth
  october_anomalies_at_depth <- silicate_anomalies %>%
    filter(Depth == depth, Month == "Oct") %>%
    select(Time, Silicate_Anomaly)
  
  # Find the anomaly value on October 22nd, 2014 at the current depth
  anomaly_october_22_2014 <- october_anomalies_at_depth %>%
    filter(Time == ymd("2014-10-22")) %>%
    pull(Silicate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for October at the current depth
  percentile_10_negative <- quantile(october_anomalies_at_depth$Silicate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for October at the current depth
  percentile_90_positive <- quantile(october_anomalies_at_depth$Silicate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on October 22nd, 2014 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_october_22_2014 <= percentile_10_negative
  
  # Check if the anomaly on October 22nd, 2014 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_october_22_2014 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)
