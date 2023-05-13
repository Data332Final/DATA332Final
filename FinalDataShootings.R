library(readxl)
library(dplyr)
library(tidyr)
library(rpart)
library(rpart.plot)
library(modelr)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(purrr)
library(shiny)
library(DT)
library(rsconnect)
library(tidyverse)
library(readr)

rm(list=ls())

setwd("~/git_data/332/DATA332Final/Data")

options(scipen = 999)
column_classes <- c("numeric", "numeric", "numeric", "character", "character")

census <- read.csv('nyc_census_tracts.csv')
censusBlock <- read.csv('census_block_loc.csv', stringsAsFactors = FALSE, colClasses = column_classes)
shootingData <- read.csv('NYPD_Shooting_Data.csv')

censusBlock$BlockCode <- as.character(censusBlock$BlockCode)
censusBlock$BlockCode <- substr(censusBlock$BlockCode, 1, nchar(censusBlock$BlockCode) - 4)
names(censusBlock)[names(censusBlock) == "BlockCode"] <- "CensusTract"
shootingData <- shootingData[complete.cases(shootingData), ]

censusBlock$Latitude <- as.character(censusBlock$Latitude)
censusBlock$Latitude <- substr(censusBlock$Latitude, 1, 5)
censusBlock$Longitude <- as.character(censusBlock$Longitude)
censusBlock$Longitude <- substr(censusBlock$Longitude, 1, 6)

shootingData$Latitude <- as.character(shootingData$Latitude)
shootingData$Latitude <- substr(shootingData$Latitude, 1, 5)
shootingData$Longitude <- as.character(shootingData$Longitude)
shootingData$Longitude <- substr(shootingData$Longitude, 1, 6)


censusBlock <- subset(censusBlock, State == "NY")
mergedData <- merge(census, censusBlock, by = "CensusTract", all.x = TRUE)
mergedData <- mergedData[complete.cases(mergedData), ]
finalData <- merge(shootingData, mergedData, by = c("Longitude", "Latitude"), all.x = TRUE)
finalData <- finalData[complete.cases(finalData), ]
finalData <- finalData[!duplicated(finalData$INCIDENT_KEY), ]

finalShootingData <- finalData %>%
  select(OCCUR_DATE, VIC_AGE_GROUP, VIC_RACE, VIC_SEX, Lon_Lat, CensusTract, Borough, TotalPop, 
         Men, Women, Hispanic, White, Black, Native, Asian, Income, Poverty, Professional, Construction,
         Transit, Unemployment)

#Shooting incidents for each VIC_RACE category within each Borough
pivot_table <- finalShootingData %>%
  group_by(Borough, VIC_RACE) %>%
  summarise(count = n()) 

ggplot(pivot_table, aes(x = Borough, y = count, fill = VIC_RACE)) +
  geom_bar(stat = "identity") +
  labs(x = "Borough", y = "Count", fill = "VIC_RACE") +
  ggtitle("Shooting Incidents by Borough and VIC_RACE") +
  theme_minimal()

#Shooting incidents for different VIC_AGE_GROUP categories within each combination of Borough and Income.
pivot_table2 <- finalShootingData %>%
  group_by(Borough, Income, VIC_AGE_GROUP) %>%
  summarise(count = n()) 

ggplot(pivot_table2, aes(x = Borough, y = count, fill = VIC_AGE_GROUP)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Borough", y = "Count", fill = "VIC_AGE_GROUP") +
  ggtitle("Shooting Incidents by Borough, Income, and VIC_AGE_GROUP") +
  theme_minimal()

