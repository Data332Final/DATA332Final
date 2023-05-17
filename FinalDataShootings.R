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
library(data.table)
library(leaflet)
library(leaflet.extras)
library(rgl)


rm(list=ls())

#setwd("~/git_data/332/DATA332Final/Data")
#setwd("C:/Users/lukec/OneDrive/Documents/data332/DATA332Final/Data")

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
  labs(x = "Borough", y = "# of Shooting Incidents", fill = "VIC_RACE") +
  ggtitle("Shooting Incidents by Borough and VIC_RACE") +
  theme_minimal()

#Shooting incidents for different VIC_AGE_GROUP categories within each combination of Borough and Income.
pivot_table2 <- finalShootingData %>%
  group_by(Borough, Income, VIC_AGE_GROUP) %>%
  summarise(count = n()) %>%
  filter(VIC_AGE_GROUP != "1022")

ggplot(pivot_table2, aes(x = Borough, y = Income, fill = VIC_AGE_GROUP)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Borough", y = "Income", fill = "VIC_AGE_GROUP") +
  ggtitle("Shooting Incidents by Borough, Income, and Age") +
  theme_minimal()

#setting up data frame to generate columns for time series analysis visualizations  
shooting_date <- setDT(finalShootingData) %>%
  #reformatting Date.sent column to enable splitting  
  .[,shooting_date := as.Date(OCCUR_DATE, format = "%m/%d/%Y")] %>%
  .[,year_num := year(shooting_date)] %>%
  .[,month_num := month(shooting_date)]

# assigning month name to numerical month values
shooting_date$month <- factor(month.name[shooting_date$month_num], levels = month.name)
# converting year values to four digit format
shooting_date$year <- ifelse(nchar(shooting_date$year_num) == 2, paste0("20",shooting_date$year_num), paste0("200",shooting_date$year_num))

year_of_shooting <- shooting_date %>%
  group_by(Borough, year) %>%
  summarise(count = n())

write.csv(year_of_shooting, "year_of_shooting.csv", row.names = FALSE)

ggplot(year_of_shooting, aes(x = year, y = count, group = Borough)) +
  geom_line(aes(colour = Borough), lwd=1.0) + 
  xlab("Year") + 
  ylab("# of Shooting Incidents") + 
  ggtitle("# of Shooting Incidents Each Year by Borough")
  
month_of_shooting <- shooting_date %>%
  group_by(Borough, month) %>%
  summarise(count = n())

write.csv(month_of_shooting, "montyh_of_shooting.csv", row.names = FALSE)

ggplot(month_of_shooting, aes(x = month, y = count, group = Borough)) +
  geom_line(aes(colour = Borough), lwd=1.0) + 
  xlab("Month") + 
  ylab("# of Shooting Incidents") + 
  ggtitle("Cumulative # of Shooting Incidents Each Month by Borough") 

# extracting lat and lon values
finalShootingData$longitude <- as.numeric(str_extract(finalShootingData$Lon_Lat, "-?\\d+\\.\\d+"))
finalShootingData$latitude <- as.numeric(sub(".*\\s(-?\\d+\\.\\d+).*", "\\1", finalShootingData$Lon_Lat))

pivot_table5 <- finalShootingData %>%
  group_by(longitude, latitude) %>%
  summarise(Shootings = n()) %>%
  filter(Shootings > 9)

leaflet(pivot_table5) %>%
  addTiles() %>%
  addAwesomeMarkers(data = pivot_table5, lng = ~longitude, lat = ~latitude, label = ~Shootings, icon = awesomeIcons(icon = "star", markerColor = "darkblue")) 

pivot_table6 <- finalShootingData %>%
  group_by(Borough, Poverty) %>%
  summarise(Count = n()) %>%
  mutate(Absolute_poverty = Count * Poverty) %>%
  mutate(Total_poverty = sum(Absolute_poverty)) %>%
  mutate(Avg_poverty = Total_poverty/sum(Count)) %>%
  mutate(Total_shootings = sum(Count)) %>%
  distinct(Avg_poverty, Total_shootings)

ggplot(pivot_table6, aes(x = reorder(Borough, Total_shootings), y = Total_shootings, fill = Avg_poverty)) +
  geom_col() +
  labs(x = "Borough", y = "# of Shooting Incidents", fill = "Average Poverty %") +
  scale_fill_gradient(low="cyan", high="black") +
  ggtitle("Effect of Poverty on Number of Shootings in Each Borough") +
  theme_minimal()

# created in order to bind to pivot_table8
pivot_table7 <- finalShootingData %>%
  group_by(Borough, Unemployment) %>%
  summarise(Count = n()) %>%
  mutate(Absolute_unemployment = Count * Unemployment) %>%
  mutate(Total_unemployment = sum(Absolute_unemployment)) %>%
  mutate(Avg_unemployment = Total_unemployment/sum(Count)) %>%
  distinct(Avg_unemployment)

pivot_table8 <- finalShootingData %>%
  group_by(Borough, Income) %>%
  summarise(Count = n()) %>%
  mutate(Absolute_income = Count * Income) %>%
  mutate(Total_income = sum(Absolute_income)) %>%
  mutate(Avg_income = round(Total_income/sum(Count)), 0) %>%
  mutate(Total_shootings = sum(Count)) %>%
  distinct(Avg_income, Total_shootings) %>%
  cbind(pivot_table7$Avg_unemployment) %>%
  dplyr::rename("Avg_unemployment" = "...4")

ggplot(pivot_table8, aes(x = reorder(Borough, -Avg_unemployment), y = Total_shootings, size = Avg_unemployment, color = Avg_income)) +
  geom_point() +
  geom_text(aes(label = paste0("$",Avg_income), vjust = 1.5)) + 
  scale_size(range = c(5,10)) +
  scale_color_gradient(low="#FFC330", high="steelblue") +
  labs(x = "Borough", y = "# of Shooting Incidents", size = "Average Unemployment", color = "Average Income")






  
  
  
