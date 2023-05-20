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

# same as above for separate data frame
shootingData$Latitude <- as.character(shootingData$Latitude)
shootingData$Latitude <- substr(shootingData$Latitude, 1, 5)
shootingData$Longitude <- as.character(shootingData$Longitude)
shootingData$Longitude <- substr(shootingData$Longitude, 1, 6)

# merged the data, removed NAs and took out duplicated records of shootings
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

#shooting incidents for each VIC_RACE category within each Borough
# pivot_table <- finalShootingData %>%
#   group_by(Borough, VIC_RACE) %>%
#   summarise(count = n()) %>%
#   filter(VIC_RACE != "UNKNOWN")
# 
# ggplot(pivot_table, aes(x = Borough, y = count, fill = VIC_RACE)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Borough", y = "# of Shooting Incidents", fill = "VIC_RACE") +
#   ggtitle("Shooting Incidents by Borough and VIC_RACE") +
#   theme_minimal()

# #Shooting incidents for different VIC_AGE_GROUP categories within each combination of Borough and Income.
# pivot_table2 <- finalShootingData %>%
#   group_by(Borough, Income, VIC_AGE_GROUP) %>%
#   summarise(count = n()) %>%
#   filter(VIC_AGE_GROUP != "1022")
# 
# ggplot(pivot_table2, aes(x = Borough, y = Income, fill = VIC_AGE_GROUP)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Borough", y = "Income", fill = "VIC_AGE_GROUP") +
#   ggtitle("Shooting Incidents by Borough, Income, and Age") +
#   theme_minimal()

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

ggplot(year_of_shooting, aes(x = year, y = count, group = Borough)) +
  geom_line(aes(colour = Borough), lwd=1.0) + 
  xlab("Year") + 
  ylab("# of Shooting Incidents") + 
  ggtitle("# of Shooting Incidents Each Year by Borough")

# making a pivot table by month for time series analysis
month_of_shooting <- shooting_date %>%
  group_by(Borough, month) %>%
  summarise(count = n())

# saving pivot table
write.csv(month_of_shooting, "month_of_shooting.csv", row.names = FALSE)

ggplot(month_of_shooting, aes(x = month, y = count, group = Borough)) +
  geom_line(aes(colour = Borough), lwd=1.0) + 
  xlab("Month") + 
  ylab("# of Shooting Incidents") + 
  ggtitle("Cumulative # of Shooting Incidents Each Month by Borough") 

# extracting lat and lon values
finalShootingData$longitude <- as.numeric(str_extract(finalShootingData$Lon_Lat, "-?\\d+\\.\\d+"))
finalShootingData$latitude <- as.numeric(sub(".*\\s(-?\\d+\\.\\d+).*", "\\1", finalShootingData$Lon_Lat))

# making a pivot table for geospatial visualization showing all the locations where at least 10 shootings occurred
shooting_coordinates <- finalShootingData %>%
  group_by(longitude, latitude) %>%
  summarise(Shootings = n()) %>%
  filter(Shootings > 9)

# saving pivot table
write.csv(shooting_coordinates, "shooting_coordinates.csv", row.names = FALSE)

leaflet(shooting_coordinates) %>%
  addTiles() %>%
  addAwesomeMarkers(data = shooting_coordinates, lng = ~longitude, lat = ~latitude, label = ~Shootings, icon = awesomeIcons(icon = "star", markerColor = "darkblue")) 

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

ggplot(poverty_and_shootings, aes(x = reorder(Borough, Total_shootings), y = Total_shootings, fill = Avg_poverty)) +
  geom_col() +
  labs(x = "Borough", y = "# of Shooting Incidents", fill = "Average Poverty %") +
  scale_fill_gradient(low="cyan", high="black") +
  ggtitle("Effect of Poverty on Number of Shootings in Each Borough") +
  theme_minimal()

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

ggplot(income_and_shootings, aes(x = reorder(Borough, -Avg_unemployment), y = Total_shootings, size = Avg_unemployment, color = Avg_income)) +
  geom_point() +
  geom_text(aes(label = paste0("$",Avg_income), vjust = 1.5)) + 
  scale_size(range = c(5,10)) +
  scale_color_gradient(low="#FFC330", high="steelblue") +
  labs(x = "Borough", y = "# of Shooting Incidents", size = "Average Unemployment", color = "Average Income")

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

ggplot(victim_women_pivot, aes(x = Borough, y = totalShootings, fill = victimRatio/popRatio)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low="white", high="darkblue") +
  geom_text(aes(label = round(victimRatio/popRatio, 2), vjust = 1.5)) +
  labs(x = "Borough", y = "Total Number of Shootings", fill = "Victim Ratio Versus Population Ratio")

#creating pivot for binding total shootings column to other race pivots  
population_race_pivot <- finalShootingData%>%
  group_by(Borough)%>%
  summarise(Count = n())





#
average_black_percent <- finalShootingData%>%
  group_by(Borough, Black)%>%
  summarise(Count = n())%>%
  mutate(absoluteCount = Count*Black)%>%
  mutate(totalBlack = sum(absoluteCount))%>%
  mutate(averageBlack = totalBlack/sum(Count))%>%
  distinct(averageBlack)
  
victim_black_pivot <- finalShootingData %>%
  group_by(Borough, VIC_RACE)%>%
  summarise(Count = n())%>%
  filter(VIC_RACE == "BLACK")%>%
  cbind(population_race_pivot$Count)%>%
  dplyr::rename("totalShootings" = "...4")%>%
  mutate(blackPercent = (Count / totalShootings)*100)%>%
  cbind(average_black_percent$averageBlack)%>%
  dplyr::rename("populationAverageBlack" = "...6")
  
ggplot(victim_black_pivot, aes(x = Borough)) +
  geom_point(aes(y = blackPercent)) +
  geom_col(aes(y = populationAverageBlack)) 
  
ggplot(victim_black_pivot, aes(x = Borough)) +
  geom_point(aes(y = blackPercent/100, color = "Percentage of Shootings Involving Black Population"), size = 5) +
  geom_segment(aes(x = Borough, xend = Borough, y = 0, yend = populationAverageBlack/100), color = "blue") +
  geom_point(aes(x = Borough, y = populationAverageBlack/100, color = "Percentage of Black Population"), size = 5) +
  geom_text(aes(y = blackPercent/100, label = round(blackPercent/100, 2)), vjust = 1.75, color = "black") +
  geom_text(aes(y = populationAverageBlack/100, label = round(populationAverageBlack/100, 2)), vjust = -1, color = "black") +
  labs(y = "% of Black Population", color = "Color Meaning") +
  ggtitle("Black Shooting Ratio Versus Black Population Ratio by Borough") +
  scale_color_manual(values = c("Percentage of Shootings Involving Black Population" = "red", "Percentage of Black Population" = "blue"), 
                     labels = c("Percentage of Shootings Involving Black Population", "Percentage of Black Population")) +
  scale_size_manual(values = c(5), 
                    labels = c("Percentage of Shootings Involving Black Population", "Percentage of Black Population"))

# added rows for boroughs where shooting incidents did not occur for purposes of binding later
victim_native_extra_data <- data.frame(Borough = c("Manhattan", "Staten Island"),
                       VIC_RACE = "AMERICAN INDIAN/ALASKAN NATIVE",
                       Count = 0)

victim_native_pivot <- finalShootingData %>%
  group_by(Borough, VIC_RACE)%>%
  summarise(Count = n())%>%
  filter(VIC_RACE == "AMERICAN INDIAN/ALASKAN NATIVE") %>%
  # adding rows created to pivot table
  bind_rows(victim_native_extra_data) %>%
  cbind(population_race_pivot$Count)%>%
  dplyr::rename("totalShootings" = "...4")%>%
  mutate(nativePercent = (Count / totalShootings)*100)

victim_asian_pivot <- finalShootingData %>%
  group_by(Borough, VIC_RACE)%>%
  summarise(Count = n())%>%
  filter(VIC_RACE == "ASIAN / PACIFIC ISLANDER") %>%
  cbind(population_race_pivot$Count)%>%
  dplyr::rename("totalShootings" = "...4")%>%
  mutate(asianPercent = (Count / totalShootings)*100)

victim_white_pivot <- finalShootingData %>%
  group_by(Borough, VIC_RACE)%>%
  summarise(Count = n())%>%
  filter(VIC_RACE == "WHITE") %>%
  cbind(population_race_pivot$Count)%>%
  dplyr::rename("totalShootings" = "...4")%>%
  mutate(whitePercent = (Count / totalShootings)*100)
  
victim_hispanic_pivot <- finalShootingData %>%
  group_by(Borough) %>%
  summarise(
    Count_BLACK_HISPANIC = sum(case_when(VIC_RACE == "BLACK HISPANIC" ~ 1, TRUE ~ 0)),
    Count_WHITE_HISPANIC = sum(case_when(VIC_RACE == "WHITE HISPANIC" ~ 1, TRUE ~ 0))) %>%
  group_by(Borough) %>%
  mutate(HISPANIC = sum(Count_BLACK_HISPANIC + Count_WHITE_HISPANIC)) %>%
  cbind(population_race_pivot$Count) %>%
  dplyr::rename("totalShootings" = "...5") %>%
  mutate(hispanicPercent = (HISPANIC / totalShootings)*100)
  
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

ggplot(victim_race_pivot, aes(x = Borough)) +
  geom_point(aes(y = blackPercent/100, color = "Percentage of Shootings Involving Black Population"), size = 5) +
  geom_point(aes(y = asianPercent/100, color = "Percentage of Shootings Involving Asian Population"), size = 5) +
  geom_point(aes(y = whitePercent/100, color = "Percentage of Shootings Involving White Population"), size = 5) +
  geom_point(aes(y = hispanicPercent/100, color = "Percentage of Shootings Involving Hispanic Population"), size = 5) +
  geom_point(aes(y = nativePercent/100, color = "Percentage of Shootings Involving Native Population"), size = 5) +
  labs(y = "% of Shooting Incidents by Race", color = "Color Meaning") +
  ggtitle("Demographic Breakdown of Shootings by Borough")

public_transit_and_shootings <- finalShootingData %>%
  group_by(Borough, Transit) %>%
  summarise(Count = n()) %>%
  mutate(Absolute_transit = Count * Transit) %>%
  mutate(Total_transit = sum(Absolute_transit)) %>%
  mutate(Avg_transit = Total_transit/sum(Count)) %>%
  mutate(Total_shootings = sum(Count)) %>%
  distinct(Avg_transit, Total_shootings)

ggplot(public_transit_and_shootings, aes(x = Borough, y = Total_shootings)) +
  geom_segment(aes(x = reorder(Borough, -Avg_transit), xend = reorder(Borough, -Avg_transit), y = 0, yend = Total_shootings, color = Avg_transit), lwd = 2.5) +
  geom_point(aes(color = Avg_transit), size = 10) +
  scale_color_gradient(low = "#FFC300", high = "#C70039") +
  labs(title = "Effect of Public Transit Usage on Number of Shootings in Each Borough",
       x = "Borough",
       y = "# of Shooting Incidents",
       color = "% of Population Commuting on Public Transit")

age_and_shootings <- finalShootingData %>%
  group_by(Borough, VIC_AGE_GROUP) %>%
  summarise(Count = n()) %>%
  filter(VIC_AGE_GROUP != "1022",
         VIC_AGE_GROUP != "UNKNOWN")

ggplot(age_and_shootings, aes(x=Borough, y=VIC_AGE_GROUP, fill=Count)) +
  geom_tile() +
  scale_fill_gradient(low="lightblue", high="black") +
  labs(title = "Heatmap of Shootings by Borough and Age of Victim", x = "Borough", y = "Age of Victim", y = "# of Shooting Incidents") +
  theme_minimal() +
  geom_text(aes(label = Count), color = "white")
  
  

  
  
  
