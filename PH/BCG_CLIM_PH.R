
# NITRATE

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

# Ensure that the custom palette covers the range of values
max_value <- max(monthly_climatology$Average_Nitrate, na.rm = TRUE)
breaks <- seq(0, max_value, by = 1)

# Define a custom palette transitioning from white to orange to dark red
custom_palette <- c("#FFFFFF", "#FFF5F0", "#FFE4B2", "#FFDAB9", "#FF8C00", 
                    "#FF4500", "#DC143C", "#B22222", "#8B0000")

# Plot the results
ggplot(monthly_climatology, aes(x = Month, y = Depth, fill = Average_Nitrate)) +
  geom_tile(color = "white") +
  scale_fill_stepsn(colours = custom_palette,
                    name = "Nitrate (µmol/L)",
                    breaks = breaks,
                    limits = c(0, max_value),
                    guide = guide_colorbar(barwidth = 2, barheight = 20, 
                                           title.position = "top", 
                                           title.hjust = 0.5)) +
  scale_y_reverse() +
  labs(title = "Monthly Climatology of Nitrate at Different Depths",
       x = "Month",
       y = "Depth (m)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")



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

# Ensure that the custom palette covers the range of values
max_value <- max(monthly_climatology$Average_Nitrate, na.rm = TRUE)
breaks <- seq(0, max_value, by = 1)

# Define a custom palette transitioning from white to orange to dark red
custom_palette <- c("#FFFFFF", "#FFF5F0", "#FFE4B2", "#FFDAB9", "#FF8C00", 
                    "#FF4500", "#DC143C", "#B22222", "#8B0000")

# Plot the results
ggplot(monthly_climatology, aes(x = Month, y = Depth, fill = Average_Nitrate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Average_Nitrate, 2)), color = "black", size = 3) +
  scale_fill_stepsn(colours = custom_palette,
                    name = "Nitrate (µmol/L)",
                    breaks = breaks,
                    limits = c(0, max_value),
                    guide = guide_colorbar(barwidth = 2, barheight = 20, 
                                           title.position = "top", 
                                           title.hjust = 0.5)) +
  scale_y_reverse() +
  labs(title = "Monthly Climatology of Nitrate at Different Depths",
       x = "Month",
       y = "Depth (m)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")




#-------------------------------------------------------------------------------

# PHOSPHATE

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

# Ensure that the custom palette covers the range of values
max_value <- max(monthly_climatology$Average_Phosphate, na.rm = TRUE)
breaks <- seq(0, max_value, by = 0.075)

# Define a custom palette transitioning from white to orange to dark red
custom_palette <- c("#FFFFFF", "#FFF5F0", "#FFE4B2", "#FFDAB9", "#FF8C00", 
                    "#FF4500", "#DC143C", "#B22222", "#8B0000")

# Plot the results
ggplot(monthly_climatology, aes(x = Month, y = Depth, fill = Average_Phosphate)) +
  geom_tile(color = "white") +
  scale_fill_stepsn(colours = custom_palette,
                    name = "Phosphate (µmol/L)",
                    breaks = breaks,
                    limits = c(0, max_value),
                    guide = guide_colorbar(barwidth = 2, barheight = 20, 
                                           title.position = "top", 
                                           title.hjust = 0.5)) +
  scale_y_reverse() +
  labs(title = "Monthly Climatology of Phosphate at Different Depths",
       x = "Month",
       y = "Depth (m)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")



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

# Ensure that the custom palette covers the range of values
max_value <- max(monthly_climatology$Average_Phosphate, na.rm = TRUE)
breaks <- seq(0, max_value, by = 0.075)

# Define a custom palette transitioning from white to orange to dark red
custom_palette <- c("#FFFFFF", "#FFF5F0", "#FFE4B2", "#FFDAB9", "#FF8C00", 
                    "#FF4500", "#DC143C", "#B22222", "#8B0000")

# Plot the results
ggplot(monthly_climatology, aes(x = Month, y = Depth, fill = Average_Phosphate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Average_Phosphate, 3)), color = "black", size = 3) +
  scale_fill_stepsn(colours = custom_palette,
                    name = "Phosphate (µmol/L)",
                    breaks = breaks,
                    limits = c(0, max_value),
                    guide = guide_colorbar(barwidth = 2, barheight = 20, 
                                           title.position = "top", 
                                           title.hjust = 0.5)) +
  scale_y_reverse() +
  labs(title = "Monthly Climatology of Phosphate at Different Depths",
       x = "Month",
       y = "Depth (m)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")




#-------------------------------------------------------------------------------

# SILICATE

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

# Ensure that the custom palette covers the range of values
max_value <- max(monthly_climatology$Average_Silicate, na.rm = TRUE)
breaks <- seq(0, max_value, by = 0.5)

# Define a custom palette transitioning from white to orange to dark red
custom_palette <- c("#FFFFFF", "#FFF5F0", "#FFE4B2", "#FFDAB9", "#FF8C00", 
                    "#FF4500", "#DC143C", "#B22222", "#8B0000")

# Plot the results
ggplot(monthly_climatology, aes(x = Month, y = Depth, fill = Average_Silicate)) +
  geom_tile(color = "white") +
  scale_fill_stepsn(colours = custom_palette,
                    name = "Silicate (µmol/L)",
                    breaks = breaks,
                    limits = c(0, max_value),
                    guide = guide_colorbar(barwidth = 2, barheight = 20, 
                                           title.position = "top", 
                                           title.hjust = 0.5)) +
  scale_y_reverse() +
  labs(title = "Monthly Climatology of Silicate at Different Depths",
       x = "Month",
       y = "Depth (m)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")



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

# Ensure that the custom palette covers the range of values
max_value <- max(monthly_climatology$Average_Silicate, na.rm = TRUE)
breaks <- seq(0, max_value, by = 0.5)

# Define a custom palette transitioning from white to orange to dark red
custom_palette <- c("#FFFFFF", "#FFF5F0", "#FFE4B2", "#FFDAB9", "#FF8C00", 
                    "#FF4500", "#DC143C", "#B22222", "#8B0000")

# Plot the results
ggplot(monthly_climatology, aes(x = Month, y = Depth, fill = Average_Silicate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Average_Silicate, 2)), color = "black", size = 3) +
  scale_fill_stepsn(colours = custom_palette,
                    name = "Silicate (µmol/L)",
                    breaks = breaks,
                    limits = c(0, max_value),
                    guide = guide_colorbar(barwidth = 2, barheight = 20, 
                                           title.position = "top", 
                                           title.hjust = 0.5)) +
  scale_y_reverse() +
  labs(title = "Monthly Climatology of Silicate at Different Depths",
       x = "Month",
       y = "Depth (m)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")
