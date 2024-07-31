
#NITRATE

#Climatology
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the CSV file
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

# Ensure that the custom palette covers the range of values
max_value <- max(monthly_climatology$Average_Nitrate, na.rm = TRUE)
breaks <- seq(0, max_value, by = 0.5) # Set breaks every 0.5

# Define a custom palette transitioning from white to orange to dark red
custom_palette <- c("#FFFFFF", "#FFF5F0", "#FFE4B2", "#FFDAB9", "#FF8C00", 
                    "#FF4500", "#DC143C", "#B22222", "#8B0000")

# Plot the results
ggplot(monthly_climatology, aes(x = Month, y = Depth, fill = Average_Nitrate)) +
  geom_tile(color = "white", height = 4) +  # Adjust the height parameter
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


#With values
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the CSV file
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

# Ensure that the custom palette covers the range of values
max_value <- max(monthly_climatology$Average_Nitrate, na.rm = TRUE)
breaks <- seq(0, max_value, by = 0.5) # Set breaks every 0.5

# Define a custom palette transitioning from white to orange to dark red
custom_palette <- c("#FFFFFF", "#FFF5F0", "#FFE4B2", "#FFDAB9", "#FF8C00", 
                    "#FF4500", "#DC143C", "#B22222", "#8B0000")

# Plot the results
ggplot(monthly_climatology, aes(x = Month, y = Depth, fill = Average_Nitrate)) +
  geom_tile(color = "white", height = 4) +  # Adjust the height parameter
  geom_text(aes(label = round(Average_Nitrate, 1)), size = 3) +  # Display values
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

#PHOSPHATE

#Climatology
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the CSV file
data <- read_csv("BCG_MIA_2009_2023.csv")

# Convert the Time column to Date type
data$Time <- mdy(data$Time)

# Extract the month from the Time column
data$Month <- month(data$Time, label = TRUE, abbr = TRUE)

# Calculate the monthly climatology for Phosphate at each Depth
monthly_climatology <- data %>%
  group_by(Depth, Month) %>%
  summarise(Average_Phosphate = mean(Phosphate, na.rm = TRUE)) %>%
  ungroup()

# Ensure that the custom palette covers the range of values
max_value <- max(monthly_climatology$Average_Phosphate, na.rm = TRUE)
breaks <- seq(0, max_value, by = 0.05) # Modified breaks to every 0.05

# Define the custom palette transitioning from white to orange to dark red
custom_palette <- c("#FFFFFF", "#FFF5F0", "#FFE4B2", "#FFDAB9", "#FF8C00", 
                    "#FF4500", "#DC143C", "#B22222", "#8B0000")

# Plot the results
ggplot(monthly_climatology, aes(x = Month, y = Depth, fill = Average_Phosphate)) +
  geom_tile(color = "white", height = 4) +  # Adjust the height parameter
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


#With values
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the CSV file
data <- read_csv("BCG_MIA_2009_2023.csv")

# Convert the Time column to Date type
data$Time <- mdy(data$Time)

# Extract the month from the Time column
data$Month <- month(data$Time, label = TRUE, abbr = TRUE)

# Calculate the monthly climatology for Phosphate at each Depth
monthly_climatology <- data %>%
  group_by(Depth, Month) %>%
  summarise(Average_Phosphate = mean(Phosphate, na.rm = TRUE)) %>%
  ungroup()

# Ensure that the custom palette covers the range of values
max_value <- max(monthly_climatology$Average_Phosphate, na.rm = TRUE)
breaks <- seq(0, max_value, by = 0.05) # Modified breaks to every 0.05

# Define the custom palette transitioning from white to orange to dark red
custom_palette <- c("#FFFFFF", "#FFF5F0", "#FFE4B2", "#FFDAB9", "#FF8C00", 
                    "#FF4500", "#DC143C", "#B22222", "#8B0000")

# Plot the results
ggplot(monthly_climatology, aes(x = Month, y = Depth, fill = Average_Phosphate)) +
  geom_tile(color = "white", height = 4) +  # Adjust the height parameter
  geom_text(aes(label = sprintf("%.2f", Average_Phosphate)), size = 2, color = "black") + # Display values
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

#SILICATE

#Climatology
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the CSV file
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

# Ensure that the custom palette covers the range of values
max_value <- max(monthly_climatology$Average_Silicate, na.rm = TRUE)
breaks <- seq(0, max_value, by = 0.25) # Adjusted to 0.25

# Define a custom palette transitioning from white to orange to dark red
custom_palette <- c("#FFFFFF", "#FFF5F0", "#FFE4B2", "#FFDAB9", "#FF8C00", 
                    "#FF4500", "#DC143C", "#B22222", "#8B0000")

# Plot the results
ggplot(monthly_climatology, aes(x = Month, y = Depth, fill = Average_Silicate)) +
  geom_tile(color = "white", height = 4) +  # Adjust the height parameter
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


#With values
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)

# Read the CSV file
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

# Ensure that the custom palette covers the range of values
max_value <- max(monthly_climatology$Average_Silicate, na.rm = TRUE)
breaks <- seq(0, max_value, by = 0.25) # Adjusted to 0.25

# Define a custom palette transitioning from white to orange to dark red
custom_palette <- c("#FFFFFF", "#FFF5F0", "#FFE4B2", "#FFDAB9", "#FF8C00", 
                    "#FF4500", "#DC143C", "#B22222", "#8B0000")

# Plot the results
ggplot(monthly_climatology, aes(x = Month, y = Depth, fill = Average_Silicate)) +
  geom_tile(color = "white", height = 4) +  # Adjust the height parameter
  geom_text(aes(label = round(Average_Silicate, 2)), size = 3) +  # Display values
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
