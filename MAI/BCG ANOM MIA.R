
#Nitrate anomaly

rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(RColorBrewer)

# Read the CSV file
nitrate_data <- read_csv("BCG_MIA_2009_2023.csv")

# Remove the time of day from the "Time" column
nitrate_data <- nitrate_data %>%
  mutate(Time = as.Date(Time, format = "%m/%d/%Y"))

# Extract month, year, and day from the "Time" column
nitrate_data <- nitrate_data %>%
  mutate(Month = month(Time, label = TRUE), Year = year(Time), Day = day(Time))

# Calculate the monthly climatology for each "Depth"
monthly_climatology <- nitrate_data %>%
  group_by(Depth, Month) %>%
  summarise(Mean_Nitrate = mean(Nitrate, na.rm = TRUE)) %>%
  ungroup()

# Filter for the actual values between November 2015 and June 2016
actual_values_2015_2016 <- nitrate_data %>%
  filter(Time >= as.Date("2015-11-01") & Time < as.Date("2016-07-01"))

# Merge the actual values with the monthly climatology to calculate anomalies
anomalies <- actual_values_2015_2016 %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Anomaly = Nitrate - Mean_Nitrate)

# Create a custom segmented color palette for anomalies
custom_palette <- c(
  colorRampPalette(c("red3", "white"))(8)[1:7],
  "white",
  colorRampPalette(c("white", "darkgreen"))(8)[2:8]
)

# Define the breaks for the segmented color bar
breaks <- c(-5.5, -4.5, -3.5, -2.5, -1.5, -0.5, 0, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5)

# Extract unique days for each month to display on the x-axis
unique_days <- actual_values_2015_2016 %>%
  group_by(Month) %>%
  summarise(Day = unique(Day))

# Create a label with both month and day
unique_days <- unique_days %>%
  mutate(Label = paste(Month, Day, sep = " "))

# Create a named vector for scale_x_discrete
month_day_labels <- setNames(unique_days$Label, unique_days$Month)

# Define the correct order for the months
month_order <- c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")

# Add a placeholder for January 2016
jan_2016 <- data.frame(
  Month = "Jan",
  Depth = unique(nitrate_data$Depth),
  Anomaly = NA
)

# Add January 2016 data to the anomalies dataframe
anomalies <- bind_rows(anomalies, jan_2016)

# Plot the anomalies
ggplot(anomalies, aes(x = factor(Month, levels = month_order), y = Depth, fill = Anomaly)) +
  geom_tile(color = "white", height = 4) + # Adjust height to make rectangles narrower
  scale_fill_stepsn(colors = custom_palette,
                    breaks = breaks,
                    limits = c(-5.5, 5.5),
                    oob = scales::squish,
                    na.value = "grey", # Use grey for missing values
                    name = "Anomaly (µmol/L)") +
  labs(x = "Month", y = "Depth (m)", title = "Nitrate Concentration Anomalies (Nov 2015 - Jun 2016)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  scale_y_reverse() +
  scale_x_discrete(labels = month_day_labels)


#with values
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(RColorBrewer)

# Read the CSV file
nitrate_data <- read_csv("BCG_MIA_2009_2023.csv")

# Remove the time of day from the "Time" column
nitrate_data <- nitrate_data %>%
  mutate(Time = as.Date(Time, format = "%m/%d/%Y"))

# Extract month, year, and day from the "Time" column
nitrate_data <- nitrate_data %>%
  mutate(Month = month(Time, label = TRUE), Year = year(Time), Day = day(Time))

# Calculate the monthly climatology for each "Depth"
monthly_climatology <- nitrate_data %>%
  group_by(Depth, Month) %>%
  summarise(Mean_Nitrate = mean(Nitrate, na.rm = TRUE)) %>%
  ungroup()

# Filter for the actual values between November 2015 and June 2016
actual_values_2015_2016 <- nitrate_data %>%
  filter(Time >= as.Date("2015-11-01") & Time < as.Date("2016-07-01"))

# Merge the actual values with the monthly climatology to calculate anomalies
anomalies <- actual_values_2015_2016 %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Anomaly = Nitrate - Mean_Nitrate)

# Create a custom segmented color palette for anomalies
custom_palette <- c(
  colorRampPalette(c("red3", "white"))(8)[1:7],
  "white",
  colorRampPalette(c("white", "darkgreen"))(8)[2:8]
)

# Define the breaks for the segmented color bar
breaks <- c(-5.5, -4.5, -3.5, -2.5, -1.5, -0.5, 0, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5)

# Extract unique days for each month to display on the x-axis
unique_days <- actual_values_2015_2016 %>%
  group_by(Month) %>%
  summarise(Day = unique(Day))

# Create a label with both month and day
unique_days <- unique_days %>%
  mutate(Label = paste(Month, Day, sep = " "))

# Create a named vector for scale_x_discrete
month_day_labels <- setNames(unique_days$Label, unique_days$Month)

# Define the correct order for the months
month_order <- c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")

# Add a placeholder for January 2016
jan_2016 <- data.frame(
  Month = "Jan",
  Depth = unique(nitrate_data$Depth),
  Anomaly = NA
)

# Add January 2016 data to the anomalies dataframe
anomalies <- bind_rows(anomalies, jan_2016)

# Plot the anomalies with text labels
ggplot(anomalies, aes(x = factor(Month, levels = month_order), y = Depth, fill = Anomaly)) +
  geom_tile(color = "white", height = 4) + # Adjust height to make rectangles narrower
  geom_text(aes(label = ifelse(is.na(Anomaly), "", round(Anomaly, 2))), size = 3, color = "black") +
  scale_fill_stepsn(colors = custom_palette,
                    breaks = breaks,
                    limits = c(-5.5, 5.5),
                    oob = scales::squish,
                    na.value = "grey", # Use grey for missing values
                    name = "Anomaly (µmol/L)") +
  labs(x = "Month", y = "Depth (m)", title = "Nitrate Concentration Anomalies (Nov 2015 - Jun 2016)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  scale_y_reverse() +
  scale_x_discrete(labels = month_day_labels)

#-------------------------------------------------------------------------------

#Phosphate anomaly

rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(RColorBrewer)

# Read the CSV file
phosphate_data <- read_csv("BCG_MIA_2009_2023.csv")

# Remove the time of day from the "Time" column
phosphate_data <- phosphate_data %>%
  mutate(Time = as.Date(Time, format = "%m/%d/%Y"))

# Extract month, year, and day from the "Time" column
phosphate_data <- phosphate_data %>%
  mutate(Month = month(Time, label = TRUE), Year = year(Time), Day = day(Time))

# Calculate the monthly climatology for each "Depth"
monthly_climatology <- phosphate_data %>%
  group_by(Depth, Month) %>%
  summarise(Mean_Phosphate = mean(Phosphate, na.rm = TRUE)) %>%
  ungroup()

# Filter for the actual values between November 2015 and June 2016
actual_values_2015_2016 <- phosphate_data %>%
  filter(Time >= as.Date("2015-11-01") & Time < as.Date("2016-07-01"))

# Merge the actual values with the monthly climatology to calculate anomalies
anomalies <- actual_values_2015_2016 %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Anomaly = Phosphate - Mean_Phosphate)

# Create a custom segmented color palette for anomalies
custom_palette <- c(
  colorRampPalette(c("red3", "white"))(8)[1:7],
  "white",
  colorRampPalette(c("white", "darkgreen"))(8)[2:8]
)

# Define the breaks for the segmented color bar
breaks <- c(-0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5)

# Extract unique days for each month to display on the x-axis
unique_days <- actual_values_2015_2016 %>%
  group_by(Month) %>%
  summarise(Day = unique(Day))

# Create a label with both month and day
unique_days <- unique_days %>%
  mutate(Label = paste(Month, Day, sep = " "))

# Create a named vector for scale_x_discrete
month_day_labels <- setNames(unique_days$Label, unique_days$Month)

# Define the correct order for the months
month_order <- c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")

# Add a placeholder for January 2016
jan_2016 <- data.frame(
  Month = "Jan",
  Depth = unique(phosphate_data$Depth),
  Anomaly = NA
)

# Add January 2016 data to the anomalies dataframe
anomalies <- bind_rows(anomalies, jan_2016)

# Plot the anomalies
ggplot(anomalies, aes(x = factor(Month, levels = month_order), y = Depth, fill = Anomaly)) +
  geom_tile(color = "white", height = 4) + # Adjust height to make rectangles narrower
  scale_fill_stepsn(colors = custom_palette,
                    breaks = breaks,
                    limits = c(-0.5, 0.5),
                    oob = scales::squish,
                    na.value = "grey", # Use grey for missing values
                    name = "Anomaly (µmol/L)") +
  labs(x = "Month", y = "Depth (m)", title = "Phosphate Concentration Anomalies (Nov 2015 - Jun 2016)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  scale_y_reverse() +
  scale_x_discrete(labels = month_day_labels)


#with values
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(RColorBrewer)

# Read the CSV file
phosphate_data <- read_csv("BCG_MIA_2009_2023.csv")

# Remove the time of day from the "Time" column
phosphate_data <- phosphate_data %>%
  mutate(Time = as.Date(Time, format = "%m/%d/%Y"))

# Extract month, year, and day from the "Time" column
phosphate_data <- phosphate_data %>%
  mutate(Month = month(Time, label = TRUE), Year = year(Time), Day = day(Time))

# Calculate the monthly climatology for each "Depth"
monthly_climatology <- phosphate_data %>%
  group_by(Depth, Month) %>%
  summarise(Mean_Phosphate = mean(Phosphate, na.rm = TRUE)) %>%
  ungroup()

# Filter for the actual values between November 2015 and June 2016
actual_values_2015_2016 <- phosphate_data %>%
  filter(Time >= as.Date("2015-11-01") & Time < as.Date("2016-07-01"))

# Merge the actual values with the monthly climatology to calculate anomalies
anomalies <- actual_values_2015_2016 %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Anomaly = Phosphate - Mean_Phosphate)

# Create a custom segmented color palette for anomalies
custom_palette <- c(
  colorRampPalette(c("red3", "white"))(8)[1:7],
  "white",
  colorRampPalette(c("white", "darkgreen"))(8)[2:8]
)

# Define the breaks for the segmented color bar
breaks <- c(-0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5)

# Extract unique days for each month to display on the x-axis
unique_days <- actual_values_2015_2016 %>%
  group_by(Month) %>%
  summarise(Day = unique(Day))

# Create a label with both month and day
unique_days <- unique_days %>%
  mutate(Label = paste(Month, Day, sep = " "))

# Create a named vector for scale_x_discrete
month_day_labels <- setNames(unique_days$Label, unique_days$Month)

# Define the correct order for the months
month_order <- c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")

# Add a placeholder for January 2016
jan_2016 <- data.frame(
  Month = "Jan",
  Depth = unique(phosphate_data$Depth),
  Anomaly = NA
)

# Add January 2016 data to the anomalies dataframe
anomalies <- bind_rows(anomalies, jan_2016)

# Plot the anomalies
ggplot(anomalies, aes(x = factor(Month, levels = month_order), y = Depth, fill = Anomaly)) +
  geom_tile(color = "white", height = 4) + # Adjust height to make rectangles narrower
  geom_text(aes(label = round(Anomaly, 2)), size = 3) + # Add anomaly values as text
  scale_fill_stepsn(colors = custom_palette,
                    breaks = breaks,
                    limits = c(-0.5, 0.5),
                    oob = scales::squish,
                    na.value = "grey", # Use grey for missing values
                    name = "Anomaly (µmol/L)") +
  labs(x = "Month", y = "Depth (m)", title = "Phosphate Concentration Anomalies (Nov 2015 - Jun 2016)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  scale_y_reverse() +
  scale_x_discrete(labels = month_day_labels)

#-------------------------------------------------------------------------------

#Silicate anomaly

rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(RColorBrewer)

# Read the CSV file
silicate_data <- read_csv("BCG_MIA_2009_2023.csv")

# Remove the time of day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Time = as.Date(Time, format = "%m/%d/%Y"))

# Extract month, year, and day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Month = month(Time, label = TRUE), Year = year(Time), Day = day(Time))

# Calculate the monthly climatology for each "Depth"
monthly_climatology <- silicate_data %>%
  group_by(Depth, Month) %>%
  summarise(Mean_Silicate = mean(Silicate, na.rm = TRUE)) %>%
  ungroup()

# Filter for the actual values between November 2015 and June 2016
actual_values_2015_2016 <- silicate_data %>%
  filter(Time >= as.Date("2015-11-01") & Time < as.Date("2016-07-01"))

# Merge the actual values with the monthly climatology to calculate anomalies
anomalies <- actual_values_2015_2016 %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Anomaly = Silicate - Mean_Silicate)

# Create a custom segmented color palette for anomalies
custom_palette <- c(
  colorRampPalette(c("red3", "white"))(8)[1:7],
  "white",
  colorRampPalette(c("white", "darkgreen"))(8)[2:8]
)

# Define the breaks for the segmented color bar
breaks <- c(-2.5, -2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5)

# Extract unique days for each month to display on the x-axis
unique_days <- actual_values_2015_2016 %>%
  group_by(Month) %>%
  summarise(Day = unique(Day))

# Create a label with both month and day
unique_days <- unique_days %>%
  mutate(Label = paste(Month, Day, sep = " "))

# Create a named vector for scale_x_discrete
month_day_labels <- setNames(unique_days$Label, unique_days$Month)

# Define the correct order for the months
month_order <- c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")

# Add a placeholder for January 2016
jan_2016 <- data.frame(
  Month = "Jan",
  Depth = unique(silicate_data$Depth),
  Anomaly = NA
)

# Add January 2016 data to the anomalies dataframe
anomalies <- bind_rows(anomalies, jan_2016)

# Plot the anomalies
ggplot(anomalies, aes(x = factor(Month, levels = month_order), y = Depth, fill = Anomaly)) +
  geom_tile(color = "white", height = 4) + # Adjust height to make rectangles narrower
  scale_fill_stepsn(colors = custom_palette,
                    breaks = breaks,
                    limits = c(-2.5, 2.5),
                    oob = scales::squish,
                    na.value = "grey", # Use grey for missing values
                    name = "Anomaly (µmol/L)") +
  labs(x = "Month", y = "Depth (m)", title = "Silicate Concentration Anomalies (Nov 2015 - Jun 2016)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  scale_y_reverse() +
  scale_x_discrete(labels = month_day_labels)


#with values
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(RColorBrewer)

# Read the CSV file
silicate_data <- read_csv("BCG_MIA_2009_2023.csv")

# Remove the time of day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Time = as.Date(Time, format = "%m/%d/%Y"))

# Extract month, year, and day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Month = month(Time, label = TRUE), Year = year(Time), Day = day(Time))

# Calculate the monthly climatology for each "Depth"
monthly_climatology <- silicate_data %>%
  group_by(Depth, Month) %>%
  summarise(Mean_Silicate = mean(Silicate, na.rm = TRUE)) %>%
  ungroup()

# Filter for the actual values between November 2015 and June 2016
actual_values_2015_2016 <- silicate_data %>%
  filter(Time >= as.Date("2015-11-01") & Time < as.Date("2016-07-01"))

# Merge the actual values with the monthly climatology to calculate anomalies
anomalies <- actual_values_2015_2016 %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Anomaly = Silicate - Mean_Silicate)

# Create a custom segmented color palette for anomalies
custom_palette <- c(
  colorRampPalette(c("red3", "white"))(8)[1:7],
  "white",
  colorRampPalette(c("white", "darkgreen"))(8)[2:8]
)

# Define the breaks for the segmented color bar
breaks <- c(-2.5, -2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5)

# Extract unique days for each month to display on the x-axis
unique_days <- actual_values_2015_2016 %>%
  group_by(Month) %>%
  summarise(Day = unique(Day))

# Create a label with both month and day
unique_days <- unique_days %>%
  mutate(Label = paste(Month, Day, sep = " "))

# Create a named vector for scale_x_discrete
month_day_labels <- setNames(unique_days$Label, unique_days$Month)

# Define the correct order for the months
month_order <- c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")

# Add a placeholder for January 2016
jan_2016 <- data.frame(
  Month = "Jan",
  Depth = unique(silicate_data$Depth),
  Anomaly = NA
)

# Add January 2016 data to the anomalies dataframe
anomalies <- bind_rows(anomalies, jan_2016)

# Plot the anomalies with anomaly values displayed
ggplot(anomalies, aes(x = factor(Month, levels = month_order), y = Depth, fill = Anomaly)) +
  geom_tile(color = "white", height = 4) + # Adjust height to make rectangles narrower
  geom_text(aes(label = round(Anomaly, 2)), size = 3, color = "black") + # Add anomaly values
  scale_fill_stepsn(colors = custom_palette,
                    breaks = breaks,
                    limits = c(-2.5, 2.5),
                    oob = scales::squish,
                    na.value = "grey", # Use grey for missing values
                    name = "Anomaly (µmol/L)") +
  labs(x = "Month", y = "Depth (m)", title = "Silicate Concentration Anomalies (Nov 2015 - Jun 2016)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  scale_y_reverse() +
  scale_x_discrete(labels = month_day_labels)
