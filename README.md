# **NYPD Shooting Incident and Borough Census Data Analysis** 👮

## Introduction
- This document will provide a data dictionary, describe the data cleaning and preparation processes, and provide analysis through visualizations and predictive modeling. The NYPD_Shooting_Data file contains data pertaining to shooting incidents that occurred in the five New York boroughs over a seventeen-year period (2006-2022), which we pulled from data.gov. This data will be analyzed alongside the nyc_census_tracts file to determine the socioeconomic and demoghraphic factors that most heavily contribute to the number/proportion of shooting incidents in New York. Data for the latter file was originally taken from the American Community Survey 2015 5-year estimates, and we pulled this data from Kaggle.

## Data Dictionary :orange_book:
The columns we used within the initial datasets include the following:
- OCCUR_DATE: The date the shooting incident occurred
- VIC_AGE_GROUP: Age range speciifcation for the victim that was shot
- VIC_RACE: The race of the victim that was shot
- VIC_SEX: The sex of the victim that was shot
- Lon_Lat: The longitude and latitude coordinates for the shooting incident
- CensusTract: A code identifying the speciifc area of the associated demographic and socieoconmic data; a much smaller subdivision of a county or borough
- Borough: Name of the borough (the Bronx, Queens, Brooklyn, Manhattan, and Staten Island)
- TotalPop: Number of people living in a specific census tract
- Men: Number of men living in a specific census tract
- Women: Number of women living in a specific census tract
- Hispanic: Percentage of population that is Hispanic/Latino
- White: Percentage of population that is white
- Black: Percentage of population that is black
- Native: Percentage of population that is Native American
- Asian: Percentage of population that is Asian
- Income: Median household income ($)
- Poverty: Percentage of population living under poverty level
- Transit: Percentage of population that commutes on public transportation
- Unemployment: Percentage of population that is unemployed
---
## Data Cleaning :broom:
1. We reformatted key columns to enable merging of the data into one comprehensive data file.
```
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
```

2. We filtered out the columns we deemed unnecessary for our analysis.
```
finalShootingData <- finalData %>%
  select(OCCUR_DATE, VIC_AGE_GROUP, VIC_RACE, VIC_SEX, Lon_Lat, CensusTract, Borough, TotalPop, 
         Men, Women, Hispanic, White, Black, Native, Asian, Income, Poverty, Professional, Construction,
         Transit, Unemployment)
```

- Note: We later deemed the professional and construction variables unnecessary and did not perform analysis for them which, which is why they were omitted in the data dictionary section.

3. We filtered out extraneous values in certain columns. 
```
age_and_shootings <- finalShootingData %>%
  filter(VIC_AGE_GROUP != "1022",
         VIC_AGE_GROUP != "UNKNOWN")

shootings_by_race <- finalShootingData %>%
  filter(VIC_RACE != "UNKNOWN")
```
---
## Data Prep :hammer:
1. We changed the schema of the date column to allow for extracting of year and month values. We then changed the format of those two columns to ensure that months were displayed in chronological order and all years contained four digits.
```
shooting_date <- setDT(finalShootingData) %>% 
  .[,shooting_date := as.Date(OCCUR_DATE, format = "%m/%d/%Y")] %>%
  .[,year_num := year(shooting_date)] %>%
  .[,month_num := month(shooting_date)]

shooting_date$month <- factor(month.name[shooting_date$month_num], levels = month.name)
shooting_date$year <- ifelse(nchar(shooting_date$year_num) == 2, paste0("20",shooting_date$year_num), paste0("200",shooting_date$year_num))
```

2. We extracted the latitutde and longitude values for use in the geospatial map.
```
finalShootingData$longitude <- as.numeric(str_extract(finalShootingData$Lon_Lat, "-?\\d+\\.\\d+"))
finalShootingData$latitude <- as.numeric(sub(".*\\s(-?\\d+\\.\\d+).*", "\\1", finalShootingData$Lon_Lat))
```

3. We added rows for boroughs where shooting incidents did not occur for a specific race to enable binding 
```
victim_native_extra_data <- data.frame(Borough = c("Manhattan", "Staten Island"),
                       VIC_RACE = "AMERICAN INDIAN/ALASKAN NATIVE",
                       Count = 0)

victim_native_pivot <- finalShootingData %>%
  bind_rows(victim_native_extra_data)
```

4. We created pivot tables that would be used for charting using just the original columns from the merged dataframe.
```
shooting_coordinates <- finalShootingData %>%
  group_by(longitude, latitude) %>%
  summarise(Shootings = n()) %>%
  filter(Shootings > 9)

year_of_shooting <- shooting_date %>%
  group_by(Borough, year) %>%
  summarise(count = n())
```
- This was done for five pivot tables in total.

5. We created pivot tables for the purpose of calculating new variables and binding them to the pivot tables used for visualizations.
```
# calculating total population of each census tract in the boroughs to use in shooting_rate pivot
borough_pop <- finalShootingData %>%
  group_by(Borough, TotalPop) %>%
  distinct(TotalPop) %>%
  group_by(Borough) %>%
  summarise(Borough_pop = as.numeric(sum(TotalPop)))

# calculating the average number of men per census tract by borough for use in calculating popRatio in women_pivot
men_pivot <- finalShootingData %>%
  group_by(Borough, Men)%>%
  summarise(Count = n())%>%
  mutate(absoluteCount = Count*Men)%>%
  mutate(totalMen = sum(absoluteCount))%>%
  mutate(averageMen = totalMen/sum(Count))%>%
  distinct(averageMen)
```

- This process accounted for eleven pivot tables in our r script.

6. We created pivot tables that calculated new variables and joined variables from existing pivot tables to plot the desired charts.
```
victim_black_pivot <- finalShootingData %>%
  group_by(Borough, VIC_RACE)%>%
  summarise(Count = n())%>%
  filter(VIC_RACE == "BLACK")%>%
  cbind(population_race_pivot$Count)%>%
  dplyr::rename("totalShootings" = "...4")%>%
  mutate(blackPercent = (Count / totalShootings)*100)%>%
  cbind(average_black_percent$averageBlack)%>%
  dplyr::rename("populationAverageBlack" = "...6")
```

- This was done for the seven remaining pivot tables that were used for charting purposes.

7. We saved thirteen pivot tables required for charting to csv files for use in the shiny script
```
write.csv(year_of_shooting, "year_of_shooting.csv", row.names = FALSE)
```

8. We converted categorical variables to factors and selected sixteen columns for use in the linear regression model that we made (without using an LLM).
```
finalShootingData$VIC_AGE_GROUP <- as.factor(finalShootingData$VIC_AGE_GROUP)
finalShootingData$VIC_RACE <- as.factor(finalShootingData$VIC_RACE)
finalShootingData$VIC_SEX <- as.factor(finalShootingData$VIC_SEX)
finalShootingData$Borough <- as.factor(finalShootingData$Borough)

final_shooting_data <- finalShootingData %>%
  select(VIC_AGE_GROUP, VIC_RACE, VIC_SEX, Borough, TotalPop, 
         Men, Women, Hispanic, White, Black, Native, Asian, Income, Poverty,
         Transit, Unemployment)
```
---
## Data Analysis :mag:
1.  We read the pivot tables into the shiny script.
```
age_and_shootings <- read.csv("age_and_shootings.csv")
public_transit_and_shootings <- read.csv("public_transit_and_shootings.csv")
year_of_shooting <- read.csv("year_of_shooting.csv")
month_of_shooting <- read.csv("month_of_shooting.csv")
shooting_coordinates <- read.csv("shooting_coordinates.csv")
poverty_and_shootings <- read.csv("poverty_and_shootings.csv")
income_and_shootings <- read.csv("income_and_shootings.csv")
victim_women_pivot <- read.csv("sex_ratio_and_shootings.csv")
shooting_rate <- read.csv("shooting_rate.csv")
shootings_by_race <- read.csv("shootings_by_race.csv")
victim_race_pivot <- read.csv("victim_race_pivot.csv")
victim_black_pivot <- read.csv("victim_black_pivot.csv")
```
2. We set up the UI configuration in the following manner.
```
ui <- fluidPage( 
  
  titlePanel(title = "NYPD Shooting Incident and Borough Census Data"),
  h4("Socioeconomic and Statistical Analysis of Shooting Incidents in the NY Boroughs"),
  
  tabsetPanel(
  
    tabPanel("Shootings by Borough and Age of Victim",
             h1("Shootings by Borough and Age of Victim"),
             p("This chart reveals which NY boroughs and age groups comprised the greatest portions of the
               total shooting incidents from the NYPD shooting data. Brooklyn and the Bronx ranked first and
               second in terms of boroughs, and 18-24 and 25-44 ranked first and second in terms of
               the age group for the victims in the shootings. The 25-44 age group in Brooklyn accounted for
               the most shooting victims, with a total of 4030."),
             fluidRow(
               column(width = 10,
                      plotOutput('plot_01', height = "650px")),
               column(width = 2,
                      tableOutput("age_and_shootings_pivot"))
             )
    ),
 ```
 - Fifteen tabs were created in the shiny app: one for project overview, two for the predictive models, and twelve for data exploration/analysis visualizations

3. We displayed the pivot table outputs for the twelve visualizations on their respective tabs.
```
output$age_and_shootings_pivot <- renderTable({
    age_and_shootings})
```

4. We created time series analysis charts by month and year.
```
year_of_shooting_chart <-
    ggplot(year_of_shooting, aes(x = year, y = count, group = Borough)) +
    geom_line(aes(colour = Borough), lwd=1.0) + 
    xlab("Year") + 
    ylab("# of Shooting Incidents") + 
    ggtitle("# of Shooting Incidents Each Year by Borough")
```

5. We created lollipop and bubble charts to identify the effect various socioeconmic and demographic factors had on the number or proportion of shooting incidents.
```
public_transit_and_shootings_chart <-
    ggplot(public_transit_and_shootings, aes(x = Borough, y = Total_shootings)) +
    geom_segment(aes(x = reorder(Borough, -Avg_transit), xend = reorder(Borough, -Avg_transit), y = 0, yend = Total_shootings, color = Avg_transit), lwd = 2.5) +
    geom_point(aes(color = Avg_transit), size = 10) +
    scale_color_gradient(low = "#FFC300", high = "#C70039") +
    labs(title = "Effect of Public Transit Usage on Number of Shootings in Each Borough",
         x = "Borough",
         y = "# of Shooting Incidents",
         color = "% of Population Commuting on Public Transit")
```
- This was done for four charts in total.

6. We created a heatmap to assess shootings by borough and victim age.
```
age_and_shootings_heatmap <-
    ggplot(age_and_shootings, aes(x=Borough, y=VIC_AGE_GROUP, fill=Count)) +
    geom_tile() +
    scale_fill_gradient(low="lightblue", high="black") +
    labs(title = "Heatmap of Shootings by Borough and Age of Victim", x = "Borough", y = "Age of Victim", y = "# of Shooting Incidents") +
    theme_minimal() +
    geom_text(aes(label = Count), color = "white")
```

7. We created standard and grouped bar charts to show relationships between sociocomonic/demographic factors and the number of shooting incidents.
```
shootings_by_race_chart <-
    ggplot(shootings_by_race, aes(x = Borough, y = count, fill = VIC_RACE)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Borough", y = "# of Shooting Incidents", fill = "Race of Victim") +
    ggtitle("Shooting Incidents by Borough and Race of Victim") +
    theme_minimal()
```
- This was done for four charts in total.

8. We created a geospatial map to display all locations where ten or more shootings occurred.
```
shooting_coordinates_map <-
    leaflet(shooting_coordinates) %>%
    addTiles() %>%
    addAwesomeMarkers(data = shooting_coordinates, lng = ~longitude, lat = ~latitude, label = ~Shootings, icon = awesomeIcons(icon = "star", markerColor = "darkblue")) 
```

9. We created a linear regression model that displayed the relationship between public transit usage and income for all 21,400 observations.
```
output$pred_model <- renderPlot({
    lm_model <- lm(Income ~ ., data = final_shooting_data)
  
    final_shooting_data <- final_shooting_data %>%
      add_predictions(lm_model)
    final_shooting_data %>%
      ggplot(aes(Transit, pred, group = 1)) +
      geom_point(color = "steelblue") +
      geom_smooth(se = FALSE) +
      labs(title = "Income by Public Transit Usage Prediction Model", x = "% of People Commuting on Public Transit", y = "Income")
  })
```

---
## Shinyapp Link :link:
- (https://sallyboutaleb21.shinyapps.io/rscripts/)
    
    
