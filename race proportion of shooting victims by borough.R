# Install required packages if not already installed
install.packages(c("ggplot2", "tidyverse", "dplyr"))

# Load the required libraries
library(ggplot2)
library(tidyverse)
library(dplyr)

# Set the working directory
setwd("C:/Users/Surface Laptop 3/Desktop/data final projecct")

# Read the census_block_loc.csv file
df <- read.csv('census_block_loc.csv')

# Read the nyc_census_tracts.csv file
nyc_census_tracts <- read.csv("nyc_census_tracts.csv")

# Read the NYPD_Shooting_Incident_Data__Historic_.csv file
nypd_shooting_data <- read.csv("NYPD_Shooting_Incident_Data__Historic_.csv")
str(df)
str(nyc_census_tracts)
str(nypd_shooting_data)


"Explore race proportion of shooting victims by borough"

library(ggplot2)

# Filter the data to exclude missing or unknown race values
filtered_data <- nypd_shooting_data[!is.na(nypd_shooting_data$VIC_RACE) & nypd_shooting_data$VIC_RACE != "", ]

# Create a bar plot showing race proportion by borough
ggplot(filtered_data, aes(x = BORO, fill = VIC_RACE)) +
  geom_bar(position = "fill") +
  labs(x = "Borough", y = "Proportion", fill = "Race") +
  ggtitle("Race Proportion of Shooting Victims by Borough")
  
  
  
  "Make bar chart for income and potentially other variables by borough"
library(ggplot2)

# Filter the data to exclude missing or unknown income values
filtered_data <- nyc_census_tracts[!is.na(nyc_census_tracts$Income), ]

# Create a bar chart for income by borough
ggplot(filtered_data, aes(x = Borough, y = Income)) +
  geom_bar(stat = "summary", fun = "mean", fill = "blue") +
  labs(x = "Borough", y = "Income", title = "Average Income by Borough")
  
  
  

