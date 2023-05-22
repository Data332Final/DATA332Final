library(readxl)
library(dplyr)
library(tidyr)
library(rpart)
library(rpart.plot)
library(modelr)
library(ggplot2)
library(shiny)
library(DT)
library(rsconnect)
library(readr)
library(data.table)
library(leaflet)
library(leaflet.extras)
library(clipr)
library(stringr)
library(tidyverse)

rm(list=ls())

#setwd("~/git_data/332/DATA332Final/Data")
setwd("C:/Users/lukec/OneDrive/Documents/data332/DATA332Final/Data")

options(scipen = 999)
column_classes <- c("numeric", "numeric", "numeric", "character", "character")

# read in all of the separate csv files into data frames
census <- read.csv('nyc_census_tracts.csv')
censusBlock <- read.csv('census_block_loc.csv', stringsAsFactors = FALSE, colClasses = column_classes)
shootingData <- read.csv('NYPD_Shooting_Data.csv')

# cleaned the data to make merge possible
censusBlock$BlockCode <- as.character(censusBlock$BlockCode)
censusBlock$BlockCode <- substr(censusBlock$BlockCode, 1, nchar(censusBlock$BlockCode) - 4)
names(censusBlock)[names(censusBlock) == "BlockCode"] <- "CensusTract"
shootingData <- shootingData[complete.cases(shootingData), ]

# made the latitudes and longitudes the same length so the merge can happen
censusBlock$Latitude <- as.character(censusBlock$Latitude)
censusBlock$Latitude <- substr(censusBlock$Latitude, 1, 5)
censusBlock$Longitude <- as.character(censusBlock$Longitude)
censusBlock$Longitude <- substr(censusBlock$Longitude, 1, 6)

# same as above but for the NYPD Shooting Data
shootingData$Latitude <- as.character(shootingData$Latitude)
shootingData$Latitude <- substr(shootingData$Latitude, 1, 5)
shootingData$Longitude <- as.character(shootingData$Longitude)
shootingData$Longitude <- substr(shootingData$Longitude, 1, 6)

# merged the data, removed NAs, and took out duplicated records of shootings
censusBlock <- subset(censusBlock, State == "NY")
mergedData <- merge(census, censusBlock, by = "CensusTract", all.x = TRUE)
mergedData <- mergedData[complete.cases(mergedData), ]
finalData <- merge(shootingData, mergedData, by = c("Longitude", "Latitude"), all.x = TRUE)
finalData <- finalData[complete.cases(finalData), ]
finalData <- finalData[!duplicated(finalData$INCIDENT_KEY), ]

# selected the columns needed for our analysis
finalShootingData <- finalData %>%
  select(OCCUR_DATE, VIC_AGE_GROUP, VIC_RACE, VIC_SEX, Lon_Lat, CensusTract, Borough, TotalPop, 
         Men, Women, Hispanic, White, Black, Native, Asian, Income, Poverty, Professional, Construction,
         Transit, Unemployment)

# creating a dataframe for inquiring ChatGPT about prediction models
llm_snapshot <- finalShootingData %>%
  sample_n(10)

# creating object for inquiring ChatGPT about prediction models
output_text <- capture.output(print(llm_snapshot))

# copying lines of code to clipboard
write_clip(output_text)

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

# making a pivot table by year for time series analysis
year_of_shooting <- shooting_date %>%
  group_by(Borough, year) %>%
  summarise(count = n())

# saving pivot table
write.csv(year_of_shooting, "year_of_shooting.csv", row.names = FALSE)

# making a pivot table by month for time series analysis
month_of_shooting <- shooting_date %>%
  group_by(Borough, month) %>%
  summarise(count = n())

# saving pivot table
write.csv(month_of_shooting, "month_of_shooting.csv", row.names = FALSE)

# extracting lat and lon values
finalShootingData$longitude <- as.numeric(str_extract(finalShootingData$Lon_Lat, "-?\\d+\\.\\d+"))
finalShootingData$latitude <- as.numeric(sub(".*\\s(-?\\d+\\.\\d+).*", "\\1", finalShootingData$Lon_Lat))

# making a pivot table for a geospatial visualization showing all the locations where at least 10 shootings occurred
shooting_coordinates <- finalShootingData %>%
  group_by(longitude, latitude) %>%
  summarise(Shootings = n()) %>%
  filter(Shootings > 9)

# saving pivot table
write.csv(shooting_coordinates, "shooting_coordinates.csv", row.names = FALSE)

# calculating total population of each census tract in the boroughs to use in shooting_rate pivot
borough_pop <- finalShootingData %>%
  group_by(Borough, TotalPop) %>%
  distinct(TotalPop) %>%
  group_by(Borough) %>%
  summarise(Borough_pop = as.numeric(sum(TotalPop)))

# calculating the rate of annual shootings per 1000 people in each borough
shooting_rate <- finalShootingData %>%
  group_by(Borough) %>%
  summarise(Count = n()) %>%
  mutate(Incidents_per_1000 = Count * 1000) %>%
  mutate(Annual_incidents_per_1000 = Incidents_per_1000/17) %>%
  cbind(borough_pop$Borough_pop) %>%
  dplyr::rename("Borough_pop" = "borough_pop$Borough_pop") %>%
  mutate(Shooting_rate = round(Annual_incidents_per_1000 / Borough_pop, 2))

# saving pivot table
write.csv(shooting_rate, "shooting_rate.csv", row.names = FALSE)

# making a pivot table to explore poverty's effects on the number of shootings
poverty_and_shootings <- finalShootingData %>%
  group_by(Borough, Poverty) %>%
  summarise(Count = n()) %>%
  mutate(Absolute_poverty = Count * Poverty) %>%
  mutate(Total_poverty = sum(Absolute_poverty)) %>%
  mutate(Avg_poverty = Total_poverty/sum(Count)) %>%
  mutate(Total_shootings = sum(Count)) %>%
  distinct(Avg_poverty, Total_shootings)

# saving pivot table
write.csv(poverty_and_shootings, "poverty_and_shootings.csv", row.names = FALSE)

# created in order to bind to income_and_shootings
unemployment_and_shootings <- finalShootingData %>%
  group_by(Borough, Unemployment) %>%
  summarise(Count = n()) %>%
  mutate(Absolute_unemployment = Count * Unemployment) %>%
  mutate(Total_unemployment = sum(Absolute_unemployment)) %>%
  mutate(Avg_unemployment = Total_unemployment/sum(Count)) %>%
  distinct(Avg_unemployment)

# saving pivot table
write.csv(unemployment_and_shootings, "unemployment_and_shootings.csv", row.names = FALSE)

# making a pivot table to explore the effects of income and unemployment on the number of shootings
income_and_shootings <- finalShootingData %>%
  group_by(Borough, Income) %>%
  summarise(Count = n()) %>%
  mutate(Absolute_income = Count * Income) %>%
  mutate(Total_income = sum(Absolute_income)) %>%
  mutate(Avg_income = round(Total_income/sum(Count)), 0) %>%
  mutate(Total_shootings = sum(Count)) %>%
  distinct(Avg_income, Total_shootings) %>%
  cbind(unemployment_and_shootings$Avg_unemployment) %>%
  dplyr::rename("Avg_unemployment" = "...4")

# saving pivot table
write.csv(income_and_shootings, "income_and_shootings.csv", row.names = FALSE)

# calculating the average number of men per census tract by borough for use in calculating popRatio in women_pivot
men_pivot <- finalShootingData %>%
  group_by(Borough, Men)%>%
  summarise(Count = n())%>%
  mutate(absoluteCount = Count*Men)%>%
  mutate(totalMen = sum(absoluteCount))%>%
  mutate(averageMen = totalMen/sum(Count))%>%
  distinct(averageMen)

# calculating the average number of women per census tract and male to female ratio by borough 
women_pivot <- finalShootingData %>%
  group_by(Borough, Women)%>%
  summarise(Count = n())%>%
  mutate(absoluteCount = Count*Women)%>%
  mutate(totalWomen = sum(absoluteCount))%>%
  mutate(averageWomen = totalWomen/sum(Count))%>%
  distinct(averageWomen)%>%
  cbind(men_pivot$averageMen)%>%
  dplyr::rename("averageMen" = "...3")%>%
  mutate(popRatio = averageMen / averageWomen)

# creating pivot for number of shooting incidents involving men
victim_men_pivot <- finalShootingData %>%
  group_by(Borough, VIC_SEX)%>%
  summarise(Count = n())%>%
  filter(VIC_SEX == 'M')

# calculating number of shootings involving women and the ratio of male to female shootings 
victim_women_pivot <- finalShootingData %>%
  group_by(Borough, VIC_SEX)%>%
  summarise(Count = n())%>%
  filter(VIC_SEX == 'F')%>%
  cbind(victim_men_pivot$Count)%>%
  dplyr::rename("countMen" = "...4")%>%
  mutate(victimRatio = countMen / Count)%>%
  cbind(women_pivot$popRatio)%>%
  dplyr::rename("popRatio" = "...6")%>%
  mutate(totalShootings = countMen + Count)%>%
  select(Borough, victimRatio, popRatio, totalShootings)

# saving pivot table
write.csv(victim_women_pivot, "sex_ratio_and_shootings.csv", row.names = FALSE)

# creating pivot for binding total shootings column to other race pivots  
population_race_pivot <- finalShootingData%>%
  group_by(Borough)%>%
  summarise(Count = n())

# Calculating the % of black people in the population of each borough
average_black_percent <- finalShootingData%>%
  group_by(Borough, Black)%>%
  summarise(Count = n())%>%
  mutate(absoluteCount = Count*Black)%>%
  mutate(totalBlack = sum(absoluteCount))%>%
  mutate(averageBlack = totalBlack/sum(Count))%>%
  distinct(averageBlack)

# calculating the % of shootings involving the black population  
victim_black_pivot <- finalShootingData %>%
  group_by(Borough, VIC_RACE)%>%
  summarise(Count = n())%>%
  filter(VIC_RACE == "BLACK")%>%
  cbind(population_race_pivot$Count)%>%
  dplyr::rename("totalShootings" = "...4")%>%
  mutate(blackPercent = (Count / totalShootings)*100)%>%
  cbind(average_black_percent$averageBlack)%>%
  dplyr::rename("populationAverageBlack" = "...6")

# saving pivot table
write.csv(victim_black_pivot, "victim_black_pivot.csv", row.names = FALSE)

# added rows for boroughs where shooting incidents did not occur for purposes of binding later
victim_native_extra_data <- data.frame(Borough = c("Manhattan", "Staten Island"),
                       VIC_RACE = "AMERICAN INDIAN/ALASKAN NATIVE",
                       Count = 0)

# calculating the % of total shootings involving native individuals by borough
victim_native_pivot <- finalShootingData %>%
  group_by(Borough, VIC_RACE)%>%
  summarise(Count = n())%>%
  filter(VIC_RACE == "AMERICAN INDIAN/ALASKAN NATIVE") %>%
  # adding rows created in victim_native_extra_data to pivot table
  bind_rows(victim_native_extra_data) %>%
  cbind(population_race_pivot$Count)%>%
  dplyr::rename("totalShootings" = "...4")%>%
  mutate(nativePercent = (Count / totalShootings)*100)

# calculating the % of total shootings involving Asian individuals by borough
victim_asian_pivot <- finalShootingData %>%
  group_by(Borough, VIC_RACE)%>%
  summarise(Count = n())%>%
  filter(VIC_RACE == "ASIAN / PACIFIC ISLANDER") %>%
  cbind(population_race_pivot$Count)%>%
  dplyr::rename("totalShootings" = "...4")%>%
  mutate(asianPercent = (Count / totalShootings)*100)

# calculating the % of total shootings involving white individuals by borough
victim_white_pivot <- finalShootingData %>%
  group_by(Borough, VIC_RACE)%>%
  summarise(Count = n())%>%
  filter(VIC_RACE == "WHITE") %>%
  cbind(population_race_pivot$Count)%>%
  dplyr::rename("totalShootings" = "...4")%>%
  mutate(whitePercent = (Count / totalShootings)*100)

# calculating the % of total shootings involving Hispanic individuals by borough  
victim_hispanic_pivot <- finalShootingData %>%
  group_by(Borough) %>%
  summarise(
    Count_BLACK_HISPANIC = sum(case_when(VIC_RACE == "BLACK HISPANIC" ~ 1, TRUE ~ 0)),
    Count_WHITE_HISPANIC = sum(case_when(VIC_RACE == "WHITE HISPANIC" ~ 1, TRUE ~ 0))) %>%
  group_by(Borough) %>%
  # combining separate columns from original data into one hispanic distinction
  mutate(HISPANIC = sum(Count_BLACK_HISPANIC + Count_WHITE_HISPANIC)) %>%
  cbind(population_race_pivot$Count) %>%
  dplyr::rename("totalShootings" = "...5") %>%
  mutate(hispanicPercent = (HISPANIC / totalShootings)*100)

# merging the race proportions calculated in the pivot tables above  
victim_race_pivot <- finalShootingData %>%
  group_by(Borough) %>%
  summarise(Count = n()) %>%
  cbind(victim_black_pivot$blackPercent, 
        victim_asian_pivot$asianPercent,
        victim_white_pivot$whitePercent,
        victim_hispanic_pivot$hispanicPercent,
        victim_native_pivot$nativePercent) %>%
  dplyr::rename("blackPercent" = "victim_black_pivot$blackPercent",
                "asianPercent" = "victim_asian_pivot$asianPercent",
                "whitePercent" = "victim_white_pivot$whitePercent",
                "hispanicPercent" = "victim_hispanic_pivot$hispanicPercent",
                "nativePercent" = "victim_native_pivot$nativePercent")  

# saving pivot table
write.csv(victim_race_pivot, "victim_race_pivot.csv", row.names = FALSE)

# making a pivot table to explore the effects of commuting with public transit on the number of shootings
public_transit_and_shootings <- finalShootingData %>%
  group_by(Borough, Transit) %>%
  summarise(Count = n()) %>%
  mutate(Absolute_transit = Count * Transit) %>%
  mutate(Total_transit = sum(Absolute_transit)) %>%
  mutate(Avg_transit = Total_transit/sum(Count)) %>%
  mutate(Total_shootings = sum(Count)) %>%
  distinct(Avg_transit, Total_shootings)

# saving pivot table
write.csv(public_transit_and_shootings, "public_transit_and_shootings.csv", row.names = FALSE)

# making pivot table for shootings by age group for each borough
age_and_shootings <- finalShootingData %>%
  group_by(Borough, VIC_AGE_GROUP) %>%
  summarise(Count = n()) %>%
  filter(VIC_AGE_GROUP != "1022",
         VIC_AGE_GROUP != "UNKNOWN")

# saving pivot table
write.csv(age_and_shootings, "age_and_shootings.csv", row.names = FALSE)
  
# shooting incidents for each VIC_RACE category within each Borough
shootings_by_race <- finalShootingData %>%
  group_by(Borough, VIC_RACE) %>%
  summarise(count = n()) %>%
  filter(VIC_RACE != "UNKNOWN")

# saving pivot table
write.csv(shootings_by_race, "shootings_by_race.csv", row.names = FALSE)
  
# converting categorical variables to factors
finalShootingData$VIC_AGE_GROUP <- as.factor(finalShootingData$VIC_AGE_GROUP)
finalShootingData$VIC_RACE <- as.factor(finalShootingData$VIC_RACE)
finalShootingData$VIC_SEX <- as.factor(finalShootingData$VIC_SEX)
finalShootingData$Borough <- as.factor(finalShootingData$Borough)

# selecting columns to compare against income in prediction model
final_shooting_data <- finalShootingData %>%
  select(VIC_AGE_GROUP, VIC_RACE, VIC_SEX, Borough, TotalPop, 
         Men, Women, Hispanic, White, Black, Native, Asian, Income, Poverty,
         Transit, Unemployment)

# saving pivot table
write.csv(final_shooting_data, "final_shooting_data.csv", row.names = FALSE)

