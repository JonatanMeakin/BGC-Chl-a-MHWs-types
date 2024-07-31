
#Silicate

#Anomaly plot 2013
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(RColorBrewer)

# Read the CSV file
silicate_data <- read_csv("BCG_PH_2009_2023.csv")

# Remove the time of day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Time = as.Date(Time))

# Extract month, year, and day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Month = month(Time, label = TRUE), Year = year(Time), Day = day(Time))

# Calculate the monthly climatology for each "Depth"
monthly_climatology <- silicate_data %>%
  group_by(Depth, Month) %>%
  summarise(Mean_Silicate = mean(Silicate, na.rm = TRUE)) %>%
  ungroup()

# Filter for the actual values between January 2013 and December 2013
actual_values_2013 <- silicate_data %>%
  filter(Time >= as.Date("2013-01-01") & Time < as.Date("2014-01-01"))

# Merge the actual values with the monthly climatology to calculate anomalies
anomalies <- actual_values_2013 %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Anomaly = Silicate - Mean_Silicate)

# Create a custom segmented color palette for anomalies
custom_palette <- c(
  colorRampPalette(c("red3", "white"))(8)[1:7],
  "white",
  colorRampPalette(c("white", "darkgreen"))(8)[2:8]
)

# Define the breaks for the segmented color bar
breaks <- c(-4.5, -3.5, -2.5, -1.5, -0.5, 0, 0.5, 1.5, 2.5, 3.5, 4.5)

# Extract unique days for each month to display on the x-axis
unique_days <- actual_values_2013 %>%
  group_by(Month) %>%
  summarise(Day = unique(Day))

# Create a label with both month and day
unique_days <- unique_days %>%
  mutate(Label = paste(Month, Day, sep = " "))

# Create a named vector for scale_x_discrete
month_day_labels <- setNames(unique_days$Label, unique_days$Month)

# Plot the anomalies
ggplot(anomalies, aes(x = Month, y = Depth, fill = Anomaly)) +
  geom_tile(color = "white") +
  scale_fill_stepsn(colors = custom_palette,
                    breaks = breaks,
                    limits = c(-4.5, 4.5),
                    oob = scales::squish,
                    name = "Anomaly (µmol/L)") +
  labs(x = "Month", y = "Depth (m)", title = "Silicate Concentration Anomalies (2013)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  scale_y_reverse() +
  scale_x_discrete(labels = month_day_labels)


##############################
#Anomaly plot 2013 with values
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(RColorBrewer)

# Read the CSV file
silicate_data <- read_csv("BCG_PH_2009_2023.csv")

# Remove the time of day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Time = as.Date(Time))

# Extract month, year, and day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Month = month(Time, label = TRUE), Year = year(Time), Day = day(Time))

# Calculate the monthly climatology for each "Depth"
monthly_climatology <- silicate_data %>%
  group_by(Depth, Month) %>%
  summarise(Mean_Silicate = mean(Silicate, na.rm = TRUE)) %>%
  ungroup()

# Filter for the actual values between January 2013 and December 2013
actual_values_2013 <- silicate_data %>%
  filter(Time >= as.Date("2013-01-01") & Time < as.Date("2014-01-01"))

# Merge the actual values with the monthly climatology to calculate anomalies
anomalies <- actual_values_2013 %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Anomaly = Silicate - Mean_Silicate)

# Create a custom segmented color palette for anomalies
custom_palette <- c(
  colorRampPalette(c("red3", "white"))(8)[1:7],
  "white",
  colorRampPalette(c("white", "darkgreen"))(8)[2:8]
)

# Define the breaks for the segmented color bar
breaks <- c(-4.5, -3.5, -2.5, -1.5, -0.5, 0, 0.5, 1.5, 2.5, 3.5, 4.5)

# Extract unique days for each month to display on the x-axis
unique_days <- actual_values_2013 %>%
  group_by(Month) %>%
  summarise(Day = unique(Day))

# Create a label with both month and day
unique_days <- unique_days %>%
  mutate(Label = paste(Month, Day, sep = " "))

# Create a named vector for scale_x_discrete
month_day_labels <- setNames(unique_days$Label, unique_days$Month)

# Plot the anomalies
ggplot(anomalies, aes(x = Month, y = Depth, fill = Anomaly, label = round(Anomaly, 1))) +
  geom_tile(color = "white") +
  geom_text(size = 3) +
  scale_fill_stepsn(colors = custom_palette,
                    breaks = breaks,
                    limits = c(-4.5, 4.5),
                    oob = scales::squish,
                    name = "Anomaly (µmol/L)") +
  labs(x = "Month", y = "Depth (m)", title = "Silicate Concentration Anomalies (2013)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  scale_y_reverse() +
  scale_x_discrete(labels = month_day_labels)


###########################
#Anomaly plot April 23 2013
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(RColorBrewer)

# Read the CSV file
silicate_data <- read_csv("BCG_PH_2009_2023.csv")

# Remove the time of day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Time = as.Date(Time))

# Extract month, year, and day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Month = month(Time, label = TRUE), Year = year(Time), Day = day(Time))

# Calculate the monthly climatology for each "Depth"
monthly_climatology <- silicate_data %>%
  group_by(Depth, Month) %>%
  summarise(Mean_Silicate = mean(Silicate, na.rm = TRUE)) %>%
  ungroup()

# Filter for the actual values for April 2013
actual_values_april_2013 <- silicate_data %>%
  filter(Year == 2013 & Month == "Apr")

# Merge the actual values with the monthly climatology to calculate anomalies
anomalies_april_2013 <- actual_values_april_2013 %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Anomaly = Silicate - Mean_Silicate)

# Create a custom segmented color palette for anomalies
custom_palette <- c(
  colorRampPalette(c("red3", "white"))(8)[1:7],
  "white",
  colorRampPalette(c("white", "darkgreen"))(8)[2:8]
)

# Define the breaks for the segmented color bar
breaks <- c(-5.5, -4.5, -3.5, -2.5, -1.5, -0.5, 0, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5)

# Extract unique days for April 2013 to display on the x-axis
unique_days_april_2013 <- actual_values_april_2013 %>%
  summarise(Day = unique(Day)) %>%
  mutate(Label = paste("Apr", Day, sep = " "))

# Create a named vector for scale_x_discrete
day_labels_april_2013 <- setNames(unique_days_april_2013$Label, unique_days_april_2013$Day)

# Plot the anomalies for April 2013 with a narrower aspect ratio
ggplot(anomalies_april_2013, aes(x = as.factor(Day), y = Depth, fill = Anomaly)) +
  geom_tile(color = "white") +
  scale_fill_stepsn(colors = custom_palette,
                    breaks = breaks,
                    limits = c(-5.5, 5.5),
                    oob = scales::squish,
                    name = "Anomaly (µmol/L)") +
  labs(x = "Day", y = "Depth (m)", title = "Silicate Concentration Anomalies (April 2013)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        plot.margin = unit(c(1,1,1,1), "cm"),
        aspect.ratio = 3) +  # Adjust the aspect ratio to make the plot narrower
  scale_y_reverse() +
  scale_x_discrete(labels = day_labels_april_2013)

#-------------------------------------------------------------------------------

#Anomaly plot 2014
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(RColorBrewer)

# Read the CSV file
silicate_data <- read_csv("BCG_PH_2009_2023.csv")

# Remove the time of day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Time = as.Date(Time))

# Extract month, year, and day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Month = month(Time, label = TRUE), Year = year(Time), Day = day(Time))

# Calculate the monthly climatology for each "Depth"
monthly_climatology <- silicate_data %>%
  group_by(Depth, Month) %>%
  summarise(Mean_Silicate = mean(Silicate, na.rm = TRUE)) %>%
  ungroup()

# Filter for the actual values between January 2014 and December 2014
actual_values_2014 <- silicate_data %>%
  filter(Time >= as.Date("2014-01-01") & Time < as.Date("2015-01-01"))

# Merge the actual values with the monthly climatology to calculate anomalies
anomalies <- actual_values_2014 %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Anomaly = Silicate - Mean_Silicate)

# Create a custom segmented color palette for anomalies
custom_palette <- c(
  colorRampPalette(c("red3", "white"))(8)[1:7],
  "white",
  colorRampPalette(c("white", "darkgreen"))(8)[2:8]
)

# Define the breaks for the segmented color bar
breaks <- c(-4.5, -3.5, -2.5, -1.5, -0.5, 0, 0.5, 1.5, 2.5, 3.5, 4.5)

# Extract unique days for each month to display on the x-axis
unique_days <- actual_values_2014 %>%
  group_by(Month) %>%
  summarise(Day = unique(Day))

# Create a label with both month and day
unique_days <- unique_days %>%
  mutate(Label = paste(Month, Day, sep = " "))

# Create a named vector for scale_x_discrete
month_day_labels <- setNames(unique_days$Label, unique_days$Month)

# Plot the anomalies
ggplot(anomalies, aes(x = Month, y = Depth, fill = Anomaly)) +
  geom_tile(color = "white") +
  scale_fill_stepsn(colors = custom_palette,
                    breaks = breaks,
                    limits = c(-4.5, 4.5),
                    oob = scales::squish,
                    name = "Anomaly (µmol/L)") +
  labs(x = "Month", y = "Depth (m)", title = "Silicate Concentration Anomalies (2014)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  scale_y_reverse() +
  scale_x_discrete(labels = month_day_labels)


##############################
#Anomaly plot 2014 with values
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(RColorBrewer)

# Read the CSV file
silicate_data <- read_csv("BCG_PH_2009_2023.csv")

# Remove the time of day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Time = as.Date(Time))

# Extract month, year, and day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Month = month(Time, label = TRUE), Year = year(Time), Day = day(Time))

# Calculate the monthly climatology for each "Depth"
monthly_climatology <- silicate_data %>%
  group_by(Depth, Month) %>%
  summarise(Mean_Silicate = mean(Silicate, na.rm = TRUE)) %>%
  ungroup()

# Filter for the actual values between January 2014 and December 2014
actual_values_2014 <- silicate_data %>%
  filter(Time >= as.Date("2014-01-01") & Time < as.Date("2015-01-01"))

# Merge the actual values with the monthly climatology to calculate anomalies
anomalies <- actual_values_2014 %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Anomaly = Silicate - Mean_Silicate)

# Create a custom segmented color palette for anomalies
custom_palette <- c(
  colorRampPalette(c("red3", "white"))(8)[1:7],
  "white",
  colorRampPalette(c("white", "darkgreen"))(8)[2:8]
)

# Define the breaks for the segmented color bar
breaks <- c(-4.5, -3.5, -2.5, -1.5, -0.5, 0, 0.5, 1.5, 2.5, 3.5, 4.5)

# Extract unique days for each month to display on the x-axis
unique_days <- actual_values_2014 %>%
  group_by(Month) %>%
  summarise(Day = unique(Day))

# Create a label with both month and day
unique_days <- unique_days %>%
  mutate(Label = paste(Month, Day, sep = " "))

# Create a named vector for scale_x_discrete
month_day_labels <- setNames(unique_days$Label, unique_days$Month)

# Plot the anomalies with text annotations
ggplot(anomalies, aes(x = Month, y = Depth, fill = Anomaly)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Anomaly, 2)), size = 3) +
  scale_fill_stepsn(colors = custom_palette,
                    breaks = breaks,
                    limits = c(-4.5, 4.5),
                    oob = scales::squish,
                    name = "Anomaly (µmol/L)") +
  labs(x = "Month", y = "Depth (m)", title = "Silicate Concentration Anomalies (2014)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  scale_y_reverse() +
  scale_x_discrete(labels = month_day_labels)


#########################
#Anomaly plot Apr 28 2014
# Anomaly plot April 28 2014
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(RColorBrewer)

# Read the CSV file
silicate_data <- read_csv("BCG_PH_2009_2023.csv")

# Remove the time of day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Time = as.Date(Time))

# Extract month, year, and day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Month = month(Time, label = TRUE), Year = year(Time), Day = day(Time))

# Calculate the monthly climatology for each "Depth"
monthly_climatology <- silicate_data %>%
  group_by(Depth, Month) %>%
  summarise(Mean_Silicate = mean(Silicate, na.rm = TRUE)) %>%
  ungroup()

# Filter for the actual values for April 2014
actual_values_april_2014 <- silicate_data %>%
  filter(Year == 2014 & Month == "Apr")

# Merge the actual values with the monthly climatology to calculate anomalies
anomalies_april_2014 <- actual_values_april_2014 %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Anomaly = Silicate - Mean_Silicate)

# Create a custom segmented color palette for anomalies
custom_palette <- c(
  colorRampPalette(c("red3", "white"))(8)[1:7],
  "white",
  colorRampPalette(c("white", "darkgreen"))(8)[2:8]
)

# Define the breaks for the segmented color bar
breaks <- c(-5.5, -4.5, -3.5, -2.5, -1.5, -0.5, 0, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5)

# Extract unique days for April 2014 to display on the x-axis
unique_days_april_2014 <- actual_values_april_2014 %>%
  summarise(Day = unique(Day)) %>%
  mutate(Label = paste("Apr", Day, sep = " "))

# Create a named vector for scale_x_discrete
day_labels_april_2014 <- setNames(unique_days_april_2014$Label, unique_days_april_2014$Day)

# Plot the anomalies for April 2014 with a narrower aspect ratio
ggplot(anomalies_april_2014, aes(x = as.factor(Day), y = Depth, fill = Anomaly)) +
  geom_tile(color = "white") +
  scale_fill_stepsn(colors = custom_palette,
                    breaks = breaks,
                    limits = c(-5.5, 5.5),
                    oob = scales::squish,
                    name = "Anomaly (µmol/L)") +
  labs(x = "Day", y = "Depth (m)", title = "Silicate Concentration Anomalies (April 2014)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        plot.margin = unit(c(1,1,1,1), "cm"),
        aspect.ratio = 3) +  # Adjust the aspect ratio to make the plot narrower
  scale_y_reverse() +
  scale_x_discrete(labels = day_labels_april_2014)


#########################
#Anomaly plot Oct 22 2014
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(RColorBrewer)

# Read the CSV file
silicate_data <- read_csv("BCG_PH_2009_2023.csv")

# Remove the time of day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Time = as.Date(Time))

# Extract month, year, and day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Month = month(Time, label = TRUE), Year = year(Time), Day = day(Time))

# Calculate the monthly climatology for each "Depth"
monthly_climatology <- silicate_data %>%
  group_by(Depth, Month) %>%
  summarise(Mean_Silicate = mean(Silicate, na.rm = TRUE)) %>%
  ungroup()

# Filter for the actual values for October 2014
actual_values_october_2014 <- silicate_data %>%
  filter(Year == 2014 & Month == "Oct")

# Merge the actual values with the monthly climatology to calculate anomalies
anomalies_october_2014 <- actual_values_october_2014 %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Anomaly = Silicate - Mean_Silicate)

# Create a custom segmented color palette for anomalies
custom_palette <- c(
  colorRampPalette(c("red3", "white"))(8)[1:7],
  "white",
  colorRampPalette(c("white", "darkgreen"))(8)[2:8]
)

# Define the breaks for the segmented color bar
breaks <- c(-5.5, -4.5, -3.5, -2.5, -1.5, -0.5, 0, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5)

# Extract unique days for October 2014 to display on the x-axis
unique_days_october_2014 <- actual_values_october_2014 %>%
  summarise(Day = unique(Day)) %>%
  mutate(Label = paste("Oct", Day, sep = " "))

# Create a named vector for scale_x_discrete
day_labels_october_2014 <- setNames(unique_days_october_2014$Label, unique_days_october_2014$Day)

# Plot the anomalies for October 2014 with a narrower aspect ratio
ggplot(anomalies_october_2014, aes(x = as.factor(Day), y = Depth, fill = Anomaly)) +
  geom_tile(color = "white") +
  scale_fill_stepsn(colors = custom_palette,
                    breaks = breaks,
                    limits = c(-5.5, 5.5),
                    oob = scales::squish,
                    name = "Anomaly (µmol/L)") +
  labs(x = "Day", y = "Depth (m)", title = "Silicate Concentration Anomalies (October 2014)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        plot.margin = unit(c(1,1,1,1), "cm"),
        aspect.ratio = 3) +  # Adjust the aspect ratio to make the plot narrower
  scale_y_reverse() +
  scale_x_discrete(labels = day_labels_october_2014)

#-------------------------------------------------------------------------------

#Anomaly plot 2015
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(RColorBrewer)

# Read the CSV file
silicate_data <- read_csv("BCG_PH_2009_2023.csv")

# Remove the time of day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Time = as.Date(Time))

# Extract month, year, and day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Month = month(Time, label = TRUE), Year = year(Time), Day = day(Time))

# Calculate the monthly climatology for each "Depth"
monthly_climatology <- silicate_data %>%
  group_by(Depth, Month) %>%
  summarise(Mean_Silicate = mean(Silicate, na.rm = TRUE)) %>%
  ungroup()

# Filter for the actual values between January 2015 and December 2015
actual_values_2015 <- silicate_data %>%
  filter(Time >= as.Date("2015-01-01") & Time < as.Date("2016-01-01"))

# Merge the actual values with the monthly climatology to calculate anomalies
anomalies <- actual_values_2015 %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Anomaly = Silicate - Mean_Silicate)

# Create a custom segmented color palette for anomalies
custom_palette <- c(
  colorRampPalette(c("red3", "white"))(8)[1:7],
  "white",
  colorRampPalette(c("white", "darkgreen"))(8)[2:8]
)

# Define the breaks for the segmented color bar
breaks <- c(-4.5, -3.5, -2.5, -1.5, -0.5, 0, 0.5, 1.5, 2.5, 3.5, 4.5)

# Extract unique days for each month to display on the x-axis
unique_days <- actual_values_2015 %>%
  group_by(Month) %>%
  summarise(Day = unique(Day))

# Create a label with both month and day
unique_days <- unique_days %>%
  mutate(Label = paste(Month, Day, sep = " "))

# Create a named vector for scale_x_discrete
month_day_labels <- setNames(unique_days$Label, unique_days$Month)

# Plot the anomalies
ggplot(anomalies, aes(x = Month, y = Depth, fill = Anomaly)) +
  geom_tile(color = "white") +
  scale_fill_stepsn(colors = custom_palette,
                    breaks = breaks,
                    limits = c(-4.5, 4.5),
                    oob = scales::squish,
                    name = "Anomaly (µmol/L)") +
  labs(x = "Month", y = "Depth (m)", title = "Silicate Concentration Anomalies (2015)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  scale_y_reverse() +
  scale_x_discrete(labels = month_day_labels)


##############################
#Anomaly plot 2015 with values
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(RColorBrewer)

# Read the CSV file
silicate_data <- read_csv("BCG_PH_2009_2023.csv")

# Remove the time of day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Time = as.Date(Time))

# Extract month, year, and day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Month = month(Time, label = TRUE), Year = year(Time), Day = day(Time))

# Calculate the monthly climatology for each "Depth"
monthly_climatology <- silicate_data %>%
  group_by(Depth, Month) %>%
  summarise(Mean_Silicate = mean(Silicate, na.rm = TRUE)) %>%
  ungroup()

# Filter for the actual values between January 2015 and December 2015
actual_values_2015 <- silicate_data %>%
  filter(Time >= as.Date("2015-01-01") & Time < as.Date("2016-01-01"))

# Merge the actual values with the monthly climatology to calculate anomalies
anomalies <- actual_values_2015 %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Anomaly = Silicate - Mean_Silicate)

# Create a custom segmented color palette for anomalies
custom_palette <- c(
  colorRampPalette(c("red3", "white"))(8)[1:7],
  "white",
  colorRampPalette(c("white", "darkgreen"))(8)[2:8]
)

# Define the breaks for the segmented color bar
breaks <- c(-4.5, -3.5, -2.5, -1.5, -0.5, 0, 0.5, 1.5, 2.5, 3.5, 4.5)

# Extract unique days for each month to display on the x-axis
unique_days <- actual_values_2015 %>%
  group_by(Month) %>%
  summarise(Day = unique(Day))

# Create a label with both month and day
unique_days <- unique_days %>%
  mutate(Label = paste(Month, Day, sep = " "))

# Create a named vector for scale_x_discrete
month_day_labels <- setNames(unique_days$Label, unique_days$Month)

# Plot the anomalies with values displayed in rectangles
ggplot(anomalies, aes(x = Month, y = Depth, fill = Anomaly)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Anomaly, 1)), size = 3, color = "black") +
  scale_fill_stepsn(colors = custom_palette,
                    breaks = breaks,
                    limits = c(-4.5, 4.5),
                    oob = scales::squish,
                    name = "Anomaly (µmol/L)") +
  labs(x = "Month", y = "Depth (m)", title = "Silicate Concentration Anomalies (2015)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  scale_y_reverse() +
  scale_x_discrete(labels = month_day_labels)


#########################
#Anomaly plot Oct 14 2015
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(RColorBrewer)

# Read the CSV file
silicate_data <- read_csv("BCG_PH_2009_2023.csv")

# Remove the time of day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Time = as.Date(Time))

# Extract month, year, and day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Month = month(Time, label = TRUE), Year = year(Time), Day = day(Time))

# Calculate the monthly climatology for each "Depth"
monthly_climatology <- silicate_data %>%
  group_by(Depth, Month) %>%
  summarise(Mean_Silicate = mean(Silicate, na.rm = TRUE)) %>%
  ungroup()

# Filter for the actual values for October 2015
actual_values_october_2015 <- silicate_data %>%
  filter(Year == 2015 & Month == "Oct")

# Merge the actual values with the monthly climatology to calculate anomalies
anomalies_october_2015 <- actual_values_october_2015 %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Anomaly = Silicate - Mean_Silicate)

# Create a custom segmented color palette for anomalies
custom_palette <- c(
  colorRampPalette(c("red3", "white"))(8)[1:7],
  "white",
  colorRampPalette(c("white", "darkgreen"))(8)[2:8]
)

# Define the breaks for the segmented color bar
breaks <- c(-5.5, -4.5, -3.5, -2.5, -1.5, -0.5, 0, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5)

# Extract unique days for October 2015 to display on the x-axis
unique_days_october_2015 <- actual_values_october_2015 %>%
  summarise(Day = unique(Day)) %>%
  mutate(Label = paste("Oct", Day, sep = " "))

# Create a named vector for scale_x_discrete
day_labels_october_2015 <- setNames(unique_days_october_2015$Label, unique_days_october_2015$Day)

# Plot the anomalies for October 2015 with a narrower aspect ratio
ggplot(anomalies_october_2015, aes(x = as.factor(Day), y = Depth, fill = Anomaly)) +
  geom_tile(color = "white") +
  scale_fill_stepsn(colors = custom_palette,
                    breaks = breaks,
                    limits = c(-5.5, 5.5),
                    oob = scales::squish,
                    name = "Anomaly (µmol/L)") +
  labs(x = "Day", y = "Depth (m)", title = "Silicate Concentration Anomalies (October 2015)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        plot.margin = unit(c(1,1,1,1), "cm"),
        aspect.ratio = 3) +  # Adjust the aspect ratio to make the plot narrower
  scale_y_reverse() +
  scale_x_discrete(labels = day_labels_october_2015)

#-------------------------------------------------------------------------------

#Anomaly plot 2016
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(RColorBrewer)

# Read the CSV file
silicate_data <- read_csv("BCG_PH_2009_2023.csv")

# Remove the time of day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Time = as.Date(Time))

# Extract month, year, and day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Month = month(Time, label = TRUE), Year = year(Time), Day = day(Time))

# Calculate the monthly climatology for each "Depth"
monthly_climatology <- silicate_data %>%
  group_by(Depth, Month) %>%
  summarise(Mean_Silicate = mean(Silicate, na.rm = TRUE)) %>%
  ungroup()

# Filter for the actual values between January 2016 and December 2016
actual_values_2016 <- silicate_data %>%
  filter(Time >= as.Date("2016-01-01") & Time < as.Date("2017-01-01"))

# Merge the actual values with the monthly climatology to calculate anomalies
anomalies <- actual_values_2016 %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Anomaly = Silicate - Mean_Silicate)

# Create a custom segmented color palette for anomalies
custom_palette <- c(
  colorRampPalette(c("red3", "white"))(8)[1:7],
  "white",
  colorRampPalette(c("white", "darkgreen"))(8)[2:8]
)

# Define the breaks for the segmented color bar
breaks <- c(-4.5, -3.5, -2.5, -1.5, -0.5, 0, 0.5, 1.5, 2.5, 3.5, 4.5)

# Extract unique days for each month to display on the x-axis
unique_days <- actual_values_2016 %>%
  group_by(Month) %>%
  summarise(Day = unique(Day))

# Create a label with both month and day
unique_days <- unique_days %>%
  mutate(Label = paste(Month, Day, sep = " "))

# Create a named vector for scale_x_discrete
month_day_labels <- setNames(unique_days$Label, unique_days$Month)

# Plot the anomalies
ggplot(anomalies, aes(x = Month, y = Depth, fill = Anomaly)) +
  geom_tile(color = "white") +
  scale_fill_stepsn(colors = custom_palette,
                    breaks = breaks,
                    limits = c(-4.5, 4.5),
                    oob = scales::squish,
                    name = "Anomaly (µmol/L)") +
  labs(x = "Month", y = "Depth (m)", title = "Silicate Concentration Anomalies (2016)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  scale_y_reverse() +
  scale_x_discrete(labels = month_day_labels)


##############################
#Anomaly plot 2016 with values
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(RColorBrewer)

# Read the CSV file
silicate_data <- read_csv("BCG_PH_2009_2023.csv")

# Remove the time of day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Time = as.Date(Time))

# Extract month, year, and day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Month = month(Time, label = TRUE), Year = year(Time), Day = day(Time))

# Calculate the monthly climatology for each "Depth"
monthly_climatology <- silicate_data %>%
  group_by(Depth, Month) %>%
  summarise(Mean_Silicate = mean(Silicate, na.rm = TRUE)) %>%
  ungroup()

# Filter for the actual values between January 2016 and December 2016
actual_values_2016 <- silicate_data %>%
  filter(Time >= as.Date("2016-01-01") & Time < as.Date("2017-01-01"))

# Merge the actual values with the monthly climatology to calculate anomalies
anomalies <- actual_values_2016 %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Anomaly = Silicate - Mean_Silicate)

# Create a custom segmented color palette for anomalies
custom_palette <- c(
  colorRampPalette(c("red3", "white"))(8)[1:7],
  "white",
  colorRampPalette(c("white", "darkgreen"))(8)[2:8]
)

# Define the breaks for the segmented color bar
breaks <- c(-4.5, -3.5, -2.5, -1.5, -0.5, 0, 0.5, 1.5, 2.5, 3.5, 4.5)

# Extract unique days for each month to display on the x-axis
unique_days <- actual_values_2016 %>%
  group_by(Month) %>%
  summarise(Day = unique(Day))

# Create a label with both month and day
unique_days <- unique_days %>%
  mutate(Label = paste(Month, Day, sep = " "))

# Create a named vector for scale_x_discrete
month_day_labels <- setNames(unique_days$Label, unique_days$Month)

# Plot the anomalies with text annotations
ggplot(anomalies, aes(x = Month, y = Depth, fill = Anomaly)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Anomaly, 2)), size = 3) +
  scale_fill_stepsn(colors = custom_palette,
                    breaks = breaks,
                    limits = c(-4.5, 4.5),
                    oob = scales::squish,
                    name = "Anomaly (µmol/L)") +
  labs(x = "Month", y = "Depth (m)", title = "Silicate Concentration Anomalies (2016)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  scale_y_reverse() +
  scale_x_discrete(labels = month_day_labels)


#########################
#Anomaly plot Sep 27 2016
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(RColorBrewer)

# Read the CSV file
silicate_data <- read_csv("BCG_PH_2009_2023.csv")

# Remove the time of day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Time = as.Date(Time))

# Extract month, year, and day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Month = month(Time, label = TRUE), Year = year(Time), Day = day(Time))

# Calculate the monthly climatology for each "Depth"
monthly_climatology <- silicate_data %>%
  group_by(Depth, Month) %>%
  summarise(Mean_Silicate = mean(Silicate, na.rm = TRUE)) %>%
  ungroup()

# Filter for the actual values for September 27, 2016
actual_values_sep_2016 <- silicate_data %>%
  filter(Year == 2016 & Month == "Sep" & Day == 27)

# Merge the actual values with the monthly climatology to calculate anomalies
anomalies_sep_2016 <- actual_values_sep_2016 %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Anomaly = Silicate - Mean_Silicate)

# Create a custom segmented color palette for anomalies
custom_palette <- c(
  colorRampPalette(c("red3", "white"))(8)[1:7],
  "white",
  colorRampPalette(c("white", "darkgreen"))(8)[2:8]
)

# Define the breaks for the segmented color bar
breaks <- c(-5.5, -4.5, -3.5, -2.5, -1.5, -0.5, 0, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5)

# Extract unique days for September 2016 to display on the x-axis
unique_days_sep_2016 <- actual_values_sep_2016 %>%
  summarise(Day = unique(Day)) %>%
  mutate(Label = paste("Sep", Day, sep = " "))

# Create a named vector for scale_x_discrete
day_labels_sep_2016 <- setNames(unique_days_sep_2016$Label, unique_days_sep_2016$Day)

# Plot the anomalies for September 27, 2016 with a narrower aspect ratio
ggplot(anomalies_sep_2016, aes(x = as.factor(Day), y = Depth, fill = Anomaly)) +
  geom_tile(color = "white") +
  scale_fill_stepsn(colors = custom_palette,
                    breaks = breaks,
                    limits = c(-5.5, 5.5),
                    oob = scales::squish,
                    name = "Anomaly (µmol/L)") +
  labs(x = "Day", y = "Depth (m)", title = "Silicate Concentration Anomalies (September 27, 2016)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        plot.margin = unit(c(1,1,1,1), "cm"),
        aspect.ratio = 3) +  # Adjust the aspect ratio to make the plot narrower
  scale_y_reverse() +
  scale_x_discrete(labels = day_labels_sep_2016)

#-------------------------------------------------------------------------------

#Anomaly plot 2019
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(RColorBrewer)

# Read the CSV file
silicate_data <- read_csv("BCG_PH_2009_2023.csv")

# Remove the time of day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Time = as.Date(Time))

# Extract month, year, and day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Month = month(Time, label = TRUE), Year = year(Time), Day = day(Time))

# Calculate the monthly climatology for each "Depth"
monthly_climatology <- silicate_data %>%
  group_by(Depth, Month) %>%
  summarise(Mean_Silicate = mean(Silicate, na.rm = TRUE)) %>%
  ungroup()

# Filter for the actual values between January 2019 and December 2019
actual_values_2019 <- silicate_data %>%
  filter(Time >= as.Date("2019-01-01") & Time < as.Date("2020-01-01"))

# Merge the actual values with the monthly climatology to calculate anomalies
anomalies <- actual_values_2019 %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Anomaly = Silicate - Mean_Silicate)

# Create a custom segmented color palette for anomalies
custom_palette <- c(
  colorRampPalette(c("red3", "white"))(8)[1:7],
  "white",
  colorRampPalette(c("white", "darkgreen"))(8)[2:8]
)

# Define the breaks for the segmented color bar
breaks <- c(-4.5, -3.5, -2.5, -1.5, -0.5, 0, 0.5, 1.5, 2.5, 3.5, 4.5)

# Extract unique days for each month to display on the x-axis
unique_days <- actual_values_2019 %>%
  group_by(Month) %>%
  summarise(Day = unique(Day))

# Create a label with both month and day
unique_days <- unique_days %>%
  mutate(Label = paste(Month, Day, sep = " "))

# Create a named vector for scale_x_discrete
month_day_labels <- setNames(unique_days$Label, unique_days$Month)

# Plot the anomalies
ggplot(anomalies, aes(x = Month, y = Depth, fill = Anomaly)) +
  geom_tile(color = "white") +
  scale_fill_stepsn(colors = custom_palette,
                    breaks = breaks,
                    limits = c(-4.5, 4.5),
                    oob = scales::squish,
                    name = "Anomaly (µmol/L)") +
  labs(x = "Month", y = "Depth (m)", title = "Silicate Concentration Anomalies (2019)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  scale_y_reverse() +
  scale_x_discrete(labels = month_day_labels)


##############################
#Anomaly plot 2019 with values
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(RColorBrewer)

# Read the CSV file
silicate_data <- read_csv("BCG_PH_2009_2023.csv")

# Remove the time of day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Time = as.Date(Time))

# Extract month, year, and day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Month = month(Time, label = TRUE), Year = year(Time), Day = day(Time))

# Calculate the monthly climatology for each "Depth"
monthly_climatology <- silicate_data %>%
  group_by(Depth, Month) %>%
  summarise(Mean_Silicate = mean(Silicate, na.rm = TRUE)) %>%
  ungroup()

# Filter for the actual values between January 2019 and December 2019
actual_values_2019 <- silicate_data %>%
  filter(Time >= as.Date("2019-01-01") & Time < as.Date("2020-01-01"))

# Merge the actual values with the monthly climatology to calculate anomalies
anomalies <- actual_values_2019 %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Anomaly = Silicate - Mean_Silicate)

# Create a custom segmented color palette for anomalies
custom_palette <- c(
  colorRampPalette(c("red3", "white"))(8)[1:7],
  "white",
  colorRampPalette(c("white", "darkgreen"))(8)[2:8]
)

# Define the breaks for the segmented color bar
breaks <- c(-4.5, -3.5, -2.5, -1.5, -0.5, 0, 0.5, 1.5, 2.5, 3.5, 4.5)

# Extract unique days for each month to display on the x-axis
unique_days <- actual_values_2019 %>%
  group_by(Month) %>%
  summarise(Day = unique(Day))

# Create a label with both month and day
unique_days <- unique_days %>%
  mutate(Label = paste(Month, Day, sep = " "))

# Create a named vector for scale_x_discrete
month_day_labels <- setNames(unique_days$Label, unique_days$Month)

# Plot the anomalies
ggplot(anomalies, aes(x = Month, y = Depth, fill = Anomaly)) +
  geom_tile(color = "white") +
  scale_fill_stepsn(colors = custom_palette,
                    breaks = breaks,
                    limits = c(-4.5, 4.5),
                    oob = scales::squish,
                    name = "Anomaly (µmol/L)") +
  geom_text(aes(label = round(Anomaly, 2)), size = 3, color = "black") +
  labs(x = "Month", y = "Depth (m)", title = "Silicate Concentration Anomalies (2019)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  scale_y_reverse() +
  scale_x_discrete(labels = month_day_labels)


#########################
#Anomaly plot May 23 2019
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(RColorBrewer)

# Read the CSV file
silicate_data <- read_csv("BCG_PH_2009_2023.csv")

# Remove the time of day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Time = as.Date(Time))

# Extract month, year, and day from the "Time" column
silicate_data <- silicate_data %>%
  mutate(Month = month(Time, label = TRUE), Year = year(Time), Day = day(Time))

# Calculate the monthly climatology for each "Depth"
monthly_climatology <- silicate_data %>%
  group_by(Depth, Month) %>%
  summarise(Mean_Silicate = mean(Silicate, na.rm = TRUE)) %>%
  ungroup()

# Filter for the actual values for May 23, 2019
actual_values_may_2019 <- silicate_data %>%
  filter(Year == 2019 & Month == "May" & Day == 23)

# Merge the actual values with the monthly climatology to calculate anomalies
anomalies_may_2019 <- actual_values_may_2019 %>%
  left_join(monthly_climatology, by = c("Depth", "Month")) %>%
  mutate(Anomaly = Silicate - Mean_Silicate)

# Create a custom segmented color palette for anomalies
custom_palette <- c(
  colorRampPalette(c("red3", "white"))(8)[1:7],
  "white",
  colorRampPalette(c("white", "darkgreen"))(8)[2:8]
)

# Define the breaks for the segmented color bar
breaks <- c(-5.5, -4.5, -3.5, -2.5, -1.5, -0.5, 0, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5)

# Extract unique days for May 2019 to display on the x-axis
unique_days_may_2019 <- actual_values_may_2019 %>%
  summarise(Day = unique(Day)) %>%
  mutate(Label = paste("May", Day, sep = " "))

# Create a named vector for scale_x_discrete
day_labels_may_2019 <- setNames(unique_days_may_2019$Label, unique_days_may_2019$Day)

# Plot the anomalies for May 23, 2019 with a narrower aspect ratio
ggplot(anomalies_may_2019, aes(x = as.factor(Day), y = Depth, fill = Anomaly)) +
  geom_tile(color = "white") +
  scale_fill_stepsn(colors = custom_palette,
                    breaks = breaks,
                    limits = c(-5.5, 5.5),
                    oob = scales::squish,
                    name = "Anomaly (µmol/L)") +
  labs(x = "Day", y = "Depth (m)", title = "Silicate Concentration Anomalies (May 23, 2019)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        plot.margin = unit(c(1,1,1,1), "cm"),
        aspect.ratio = 3) +  # Adjust the aspect ratio to make the plot narrower
  scale_y_reverse() +
  scale_x_discrete(labels = day_labels_may_2019)
