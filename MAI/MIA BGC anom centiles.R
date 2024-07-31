
#Nitrate

#Dec 17 2015
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the new CSV file
nitrate_data <- read_csv("BCG_MIA_2009_2023.csv")

# Convert the Time column to Date type
nitrate_data$Time <- mdy(nitrate_data$Time)

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
  # Filter data for anomalies in December at the current depth
  december_anomalies_at_depth <- nitrate_anomalies %>%
    filter(Depth == depth, Month == "Dec") %>%
    select(Time, Nitrate_Anomaly)
  
  # Find the anomaly value on December 17th, 2015 at the current depth
  anomaly_december_17_2015 <- december_anomalies_at_depth %>%
    filter(Time == ymd("2015-12-17")) %>%
    pull(Nitrate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for December at the current depth
  percentile_10_negative <- quantile(december_anomalies_at_depth$Nitrate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for December at the current depth
  percentile_90_positive <- quantile(december_anomalies_at_depth$Nitrate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on December 17th, 2015 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_december_17_2015 <= percentile_10_negative
  
  # Check if the anomaly on December 17th, 2015 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_december_17_2015 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)



#Feb 10 2016
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the new CSV file
nitrate_data <- read_csv("BCG_MIA_2009_2023.csv")

# Convert the Time column to Date type
nitrate_data$Time <- mdy(nitrate_data$Time)

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
  # Filter data for anomalies in February at the current depth
  february_anomalies_at_depth <- nitrate_anomalies %>%
    filter(Depth == depth, Month == "Feb") %>%
    select(Time, Nitrate_Anomaly)
  
  # Find the anomaly value on February 10th, 2016 at the current depth
  anomaly_february_10_2016 <- february_anomalies_at_depth %>%
    filter(Time == ymd("2016-02-10")) %>%
    pull(Nitrate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for February at the current depth
  percentile_10_negative <- quantile(february_anomalies_at_depth$Nitrate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for February at the current depth
  percentile_90_positive <- quantile(february_anomalies_at_depth$Nitrate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on February 10th, 2016 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_february_10_2016 <= percentile_10_negative
  
  # Check if the anomaly on February 10th, 2016 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_february_10_2016 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)



#Mar 22 2016
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the new CSV file
nitrate_data <- read_csv("BCG_MIA_2009_2023.csv")

# Convert the Time column to Date type
nitrate_data$Time <- mdy(nitrate_data$Time)

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
  # Filter data for anomalies in March at the current depth
  march_anomalies_at_depth <- nitrate_anomalies %>%
    filter(Depth == depth, Month == "Mar") %>%
    select(Time, Nitrate_Anomaly)
  
  # Find the anomaly value on March 22nd, 2016 at the current depth
  anomaly_march_22_2016 <- march_anomalies_at_depth %>%
    filter(Time == ymd("2016-03-22")) %>%
    pull(Nitrate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for March at the current depth
  percentile_10_negative <- quantile(march_anomalies_at_depth$Nitrate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for March at the current depth
  percentile_90_positive <- quantile(march_anomalies_at_depth$Nitrate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on March 22nd, 2016 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_march_22_2016 <= percentile_10_negative
  
  # Check if the anomaly on March 22nd, 2016 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_march_22_2016 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)



#Apr 28 2016
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the new CSV file
nitrate_data <- read_csv("BCG_MIA_2009_2023.csv")

# Convert the Time column to Date type
nitrate_data$Time <- mdy(nitrate_data$Time)

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
  
  # Find the anomaly value on April 28th, 2016 at the current depth
  anomaly_april_28_2016 <- april_anomalies_at_depth %>%
    filter(Time == ymd("2016-04-28")) %>%
    pull(Nitrate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for April at the current depth
  percentile_10_negative <- quantile(april_anomalies_at_depth$Nitrate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for April at the current depth
  percentile_90_positive <- quantile(april_anomalies_at_depth$Nitrate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on April 28th, 2016 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_april_28_2016 <= percentile_10_negative
  
  # Check if the anomaly on April 28th, 2016 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_april_28_2016 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)



#May 31 2016
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the new CSV file
nitrate_data <- read_csv("BCG_MIA_2009_2023.csv")

# Convert the Time column to Date type
nitrate_data$Time <- mdy(nitrate_data$Time)

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
  
  # Find the anomaly value on May 31st, 2016 at the current depth
  anomaly_may_31_2016 <- may_anomalies_at_depth %>%
    filter(Time == ymd("2016-05-31")) %>%
    pull(Nitrate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for May at the current depth
  percentile_10_negative <- quantile(may_anomalies_at_depth$Nitrate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for May at the current depth
  percentile_90_positive <- quantile(may_anomalies_at_depth$Nitrate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on May 31st, 2016 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_may_31_2016 <= percentile_10_negative
  
  # Check if the anomaly on May 31st, 2016 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_may_31_2016 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)


#-------------------------------------------------------------------------------

#Phosphate 

#Dec 17 2015
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the new CSV file
phosphate_data <- read_csv("BCG_MIA_2009_2023.csv")

# Convert the Time column to Date type
phosphate_data$Time <- mdy(phosphate_data$Time)

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
  # Filter data for anomalies in December at the current depth
  december_anomalies_at_depth <- phosphate_anomalies %>%
    filter(Depth == depth, Month == "Dec") %>%
    select(Time, Phosphate_Anomaly)
  
  # Find the anomaly value on December 17th, 2015 at the current depth
  anomaly_december_17_2015 <- december_anomalies_at_depth %>%
    filter(Time == ymd("2015-12-17")) %>%
    pull(Phosphate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for December at the current depth
  percentile_10_negative <- quantile(december_anomalies_at_depth$Phosphate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for December at the current depth
  percentile_90_positive <- quantile(december_anomalies_at_depth$Phosphate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on December 17th, 2015 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_december_17_2015 <= percentile_10_negative
  
  # Check if the anomaly on December 17th, 2015 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_december_17_2015 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)



#Feb 10 2016
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the new CSV file
phosphate_data <- read_csv("BCG_MIA_2009_2023.csv")

# Convert the Time column to Date type
phosphate_data$Time <- mdy(phosphate_data$Time)

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
  # Filter data for anomalies in February at the current depth
  february_anomalies_at_depth <- phosphate_anomalies %>%
    filter(Depth == depth, Month == "Feb") %>%
    select(Time, Phosphate_Anomaly)
  
  # Find the anomaly value on February 10th, 2016 at the current depth
  anomaly_february_10_2016 <- february_anomalies_at_depth %>%
    filter(Time == ymd("2016-02-10")) %>%
    pull(Phosphate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for February at the current depth
  percentile_10_negative <- quantile(february_anomalies_at_depth$Phosphate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for February at the current depth
  percentile_90_positive <- quantile(february_anomalies_at_depth$Phosphate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on February 10th, 2016 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_february_10_2016 <= percentile_10_negative
  
  # Check if the anomaly on February 10th, 2016 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_february_10_2016 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)



#Mar 22 2016
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the new CSV file
phosphate_data <- read_csv("BCG_MIA_2009_2023.csv")

# Convert the Time column to Date type
phosphate_data$Time <- mdy(phosphate_data$Time)

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
  # Filter data for anomalies in March at the current depth
  march_anomalies_at_depth <- phosphate_anomalies %>%
    filter(Depth == depth, Month == "Mar") %>%
    select(Time, Phosphate_Anomaly)
  
  # Find the anomaly value on March 22nd, 2016 at the current depth
  anomaly_march_22_2016 <- march_anomalies_at_depth %>%
    filter(Time == ymd("2016-03-22")) %>%
    pull(Phosphate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for March at the current depth
  percentile_10_negative <- quantile(march_anomalies_at_depth$Phosphate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for March at the current depth
  percentile_90_positive <- quantile(march_anomalies_at_depth$Phosphate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on March 22nd, 2016 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_march_22_2016 <= percentile_10_negative
  
  # Check if the anomaly on March 22nd, 2016 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_march_22_2016 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)



#Apr 28 2016
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the new CSV file
phosphate_data <- read_csv("BCG_MIA_2009_2023.csv")

# Convert the Time column to Date type
phosphate_data$Time <- mdy(phosphate_data$Time)

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
  
  # Find the anomaly value on April 28th, 2016 at the current depth
  anomaly_april_28_2016 <- april_anomalies_at_depth %>%
    filter(Time == ymd("2016-04-28")) %>%
    pull(Phosphate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for April at the current depth
  percentile_10_negative <- quantile(april_anomalies_at_depth$Phosphate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for April at the current depth
  percentile_90_positive <- quantile(april_anomalies_at_depth$Phosphate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on April 28th, 2016 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_april_28_2016 <= percentile_10_negative
  
  # Check if the anomaly on April 28th, 2016 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_april_28_2016 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)



#May 31 2016
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the new CSV file
phosphate_data <- read_csv("BCG_MIA_2009_2023.csv")

# Convert the Time column to Date type
phosphate_data$Time <- mdy(phosphate_data$Time)

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
  
  # Find the anomaly value on May 31st, 2016 at the current depth
  anomaly_may_31_2016 <- may_anomalies_at_depth %>%
    filter(Time == ymd("2016-05-31")) %>%
    pull(Phosphate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for May at the current depth
  percentile_10_negative <- quantile(may_anomalies_at_depth$Phosphate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for May at the current depth
  percentile_90_positive <- quantile(may_anomalies_at_depth$Phosphate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on May 31st, 2016 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_may_31_2016 <= percentile_10_negative
  
  # Check if the anomaly on May 31st, 2016 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_may_31_2016 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)


#-------------------------------------------------------------------------------

#Silicate

#Dec 17 2015
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the new CSV file
silicate_data <- read_csv("BCG_MIA_2009_2023.csv")

# Convert the Time column to Date type
silicate_data$Time <- mdy(silicate_data$Time)

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
  # Filter data for anomalies in December at the current depth
  december_anomalies_at_depth <- silicate_anomalies %>%
    filter(Depth == depth, Month == "Dec") %>%
    select(Time, Silicate_Anomaly)
  
  # Find the anomaly value on December 17th, 2015 at the current depth
  anomaly_december_17_2015 <- december_anomalies_at_depth %>%
    filter(Time == ymd("2015-12-17")) %>%
    pull(Silicate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for December at the current depth
  percentile_10_negative <- quantile(december_anomalies_at_depth$Silicate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for December at the current depth
  percentile_90_positive <- quantile(december_anomalies_at_depth$Silicate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on December 17th, 2015 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_december_17_2015 <= percentile_10_negative
  
  # Check if the anomaly on December 17th, 2015 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_december_17_2015 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)



#Feb 10 2016
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the new CSV file
silicate_data <- read_csv("BCG_MIA_2009_2023.csv")

# Convert the Time column to Date type
silicate_data$Time <- mdy(silicate_data$Time)

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
  # Filter data for anomalies in February at the current depth
  february_anomalies_at_depth <- silicate_anomalies %>%
    filter(Depth == depth, Month == "Feb") %>%
    select(Time, Silicate_Anomaly)
  
  # Find the anomaly value on February 10th, 2016 at the current depth
  anomaly_february_10_2016 <- february_anomalies_at_depth %>%
    filter(Time == ymd("2016-02-10")) %>%
    pull(Silicate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for February at the current depth
  percentile_10_negative <- quantile(february_anomalies_at_depth$Silicate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for February at the current depth
  percentile_90_positive <- quantile(february_anomalies_at_depth$Silicate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on February 10th, 2016 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_february_10_2016 <= percentile_10_negative
  
  # Check if the anomaly on February 10th, 2016 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_february_10_2016 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)



#Mar 22 2016
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the new CSV file
silicate_data <- read_csv("BCG_MIA_2009_2023.csv")

# Convert the Time column to Date type
silicate_data$Time <- mdy(silicate_data$Time)

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
  # Filter data for anomalies in March at the current depth
  march_anomalies_at_depth <- silicate_anomalies %>%
    filter(Depth == depth, Month == "Mar") %>%
    select(Time, Silicate_Anomaly)
  
  # Find the anomaly value on March 22nd, 2016 at the current depth
  anomaly_march_22_2016 <- march_anomalies_at_depth %>%
    filter(Time == ymd("2016-03-22")) %>%
    pull(Silicate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for March at the current depth
  percentile_10_negative <- quantile(march_anomalies_at_depth$Silicate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for March at the current depth
  percentile_90_positive <- quantile(march_anomalies_at_depth$Silicate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on March 22nd, 2016 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_march_22_2016 <= percentile_10_negative
  
  # Check if the anomaly on March 22nd, 2016 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_march_22_2016 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)



#Apr 28 2016
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the new CSV file
silicate_data <- read_csv("BCG_MIA_2009_2023.csv")

# Convert the Time column to Date type
silicate_data$Time <- mdy(silicate_data$Time)

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
  
  # Find the anomaly value on April 28th, 2016 at the current depth
  anomaly_april_28_2016 <- april_anomalies_at_depth %>%
    filter(Time == ymd("2016-04-28")) %>%
    pull(Silicate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for April at the current depth
  percentile_10_negative <- quantile(april_anomalies_at_depth$Silicate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for April at the current depth
  percentile_90_positive <- quantile(april_anomalies_at_depth$Silicate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on April 28th, 2016 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_april_28_2016 <= percentile_10_negative
  
  # Check if the anomaly on April 28th, 2016 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_april_28_2016 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)



#May 31 2016
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the new CSV file
silicate_data <- read_csv("BCG_MIA_2009_2023.csv")

# Convert the Time column to Date type
silicate_data$Time <- mdy(silicate_data$Time)

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
  
  # Find the anomaly value on May 31st, 2016 at the current depth
  anomaly_may_31_2016 <- may_anomalies_at_depth %>%
    filter(Time == ymd("2016-05-31")) %>%
    pull(Silicate_Anomaly)
  
  # Calculate the 10th percentile of negative anomalies for May at the current depth
  percentile_10_negative <- quantile(may_anomalies_at_depth$Silicate_Anomaly, probs = 0.1, na.rm = TRUE)
  
  # Calculate the 90th percentile of positive anomalies for May at the current depth
  percentile_90_positive <- quantile(may_anomalies_at_depth$Silicate_Anomaly, probs = 0.9, na.rm = TRUE)
  
  # Check if the anomaly on May 31st, 2016 is within the 10th percentile of negative anomalies
  is_in_10th_percentile_negative <- anomaly_may_31_2016 <= percentile_10_negative
  
  # Check if the anomaly on May 31st, 2016 is within the 10th percentile of positive anomalies
  is_in_10th_percentile_positive <- anomaly_may_31_2016 >= percentile_90_positive
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(Depth = depth, 
                                       Is_In_10th_Percentile_Negative = is_in_10th_percentile_negative,
                                       Is_In_10th_Percentile_Positive = is_in_10th_percentile_positive))
}

# View the results
print(results)
