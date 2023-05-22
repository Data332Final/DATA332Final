library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
library(DT)
library(data.table)
library(lubridate)
library(leaflet)
library(modelr)

rm(list = ls())

#setwd("C:/Users/lukec/OneDrive/Documents/data332/DATA332Final/pivotTables")

# reading in pivot tables
age_and_shootings <- read.csv("age_and_shootings.csv")
public_transit_and_shootings <- read.csv("public_transit_and_shootings.csv")
year_of_shooting <- read.csv("year_of_shooting.csv")
month_of_shooting <- read.csv("month_of_shooting.csv")
shooting_coordinates <- read.csv("shooting_coordinates.csv")
poverty_and_shootings <- read.csv("poverty_and_shootings.csv")
unemployment_and_shootings <- read.csv("unemployment_and_shootings.csv")
income_and_shootings <- read.csv("income_and_shootings.csv")
victim_women_pivot <- read.csv("sex_ratio_and_shootings.csv")
shooting_rate <- read.csv("shooting_rate.csv")
shootings_by_race <- read.csv("shootings_by_race.csv")
victim_race_pivot <- read.csv("victim_race_pivot.csv")
victim_black_pivot <- read.csv("victim_black_pivot.csv")
final_shooting_data <- read.csv("final_shooting_data.csv")

#setting up ui
ui<-fluidPage( 
  
  titlePanel(title = "NYPD Shooting Incident and Borough Census Data"),
  h4("Socioeconomic and Statistical Analysis of Shooting Incidents in the NY Boroughs"),
  
  # Create a tabset panel to organize charts in different pages
  tabsetPanel(
    
    # First tab content
    tabPanel("Project Overview",
             h1("Research Focus and Objectives, Requirements, Scope, and Idea Backlog"),
             p(
               HTML("Focus and Objectives:<br>
                    This study seeks to explore and analyze the demographic and socioeconomic factors that contribute to the number of 
                    shootings within each of the five NY boroughs. As a means to accomplish this end, we generated twelve charts and two prediction models to 
                    examine these relationships proposed above. Another objective of our research is examining how the demographic breakdowns or ratios of 
                    shooting victims for variables such as sex or race of the victim compare to the demographic breakdowns of those variables within the boroughs 
                    where they occurred to see if they are proportional. The datasets for this research were chosen as a result of their volume, with the merged 
                    dataset containing over fifty unique variables and 21,400 observations. Another factor was the cleanliness of the data; little to no cleaning 
                    had to be done beyond eliminating columns we deemed unnecessary/information poor. For the more practical purposes of why we chose this research 
                    topic, gaining a deeper understanding of the contributing factors of shooting incidents can help provide the insights and information necessary 
                    to implement response plans and programs to minimize the prevalence of the primary causes of the shootings.While we do not have the jurisdiction 
                    required to do such a thing, we felt this topic of study was a worthwhile research endeavor for these reasons.")
             ),
             p(
               HTML("Requirements:<br>
          - Explore and analyze the demographic and socioeconomic factors that contribute to the number of shootings within each of the five NY boroughs.<br>
          - Explore the proportion between demographic breakdowns of shooting victims and the demographic makeup of the boroughs’ census data.<br>
          - Generate a model that predicts the number of shooting incidents or based on key independent variables.")
             ),
             p(
               HTML("Scope: <br>
          - Conduct data exploration and visualization for all relationships deemed potentially insightful. <br>
          - Explore and find out which factors together give us the most insightful models. <br>
          - Display all models into an interactive shiny app to give users an easy way to view the found information.")
               ),
             p(
               HTML("Idea Backlog:<br>
          - Create custom icon markers within the geospatial map to indicate the number of shootings, e.g., locations with 10-20 shootings is one color, 20-30 shootings is a different color, etc.<br>
          - Overlay borough boundary lines onto the geospatial map.<br>
          - Complete borough race breakdown vs shooting race breakdown for all five races present in the data like we did for black shooting victims.<br>
          - Analyze the percentage of people that work professional versus construction jobs in each borough and its potential impact on the number of shootings.")
             )
    ),
    
    # Second tab content
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

    # Third tab content
    tabPanel("Shootings by Public Transit Usage and Borough",
             h1("Shootings by Public Transit Usage and Borough"),
             p("With the exception of Manhattan, there is a fairly strong correlation between the number of
               shootings and the percentage of citizens that commute on public transit, where as the percentage of inhabitants
               taking public transit goes down, the number of shootings goes down as well. The case of Manhattan
               shows that other socioeconomic factors may play an important role in affecting the total number of
               shootings. Brooklyn ranked just below Manhattan in terms of the population that takes public transit,
               with a value of 66.08%, and it had the most shooting incidents, with a value of 8,768. Staten Island
               ranked lowest in both categories with values of 44.34% and 619."),
             fluidRow(
               column(width = 9,
                      plotOutput('plot_02', height = "625px")),
               column(width = 3,
                      tableOutput("public_transit_and_shootings_pivot"))
             )
    ),
    
    # Fourth tab content
    tabPanel("Shootings by Year and Borough",
             h1("Shootings by Year and Borough"),
             p("While there are inconsistencies among the boroughs, three general trends that can be identified are
               the gradual decrease in incidents from 2006 to 2018/2019, followed by increases through 2020/2021, and
               finally, a drop-off in the number of incidents through 2022. The scope of this project does not cover what 
               contributed to the rise or fall in shooting incidents reported from year-to-year, so it may be worth 
               exploring what factors contributed to the steep increase in all boroughs between 2019 and 2020. As 
               mentioned in previous charts, Brooklyn consistently reported the most incidents, while Staten Island
               reported the least. "),
             fluidRow(
               column(width = 10,
                      plotOutput('plot_03', height = "625px")),
               column(width = 2,
                      tableOutput("year_of_shooting_pivot"))
             )
    ),
    
    # Fifth tab content
    tabPanel("Shootings by Month and Borough",
             h1("Shootings by Month and Borough"),
             p("Disregarding minor inconcistencies, common trends that can be seen here are increases in the number of
               shooting incidents between the start of spring and the end of summer, as well as decreases from the beginning of
              fall through the end of winter; December is an exception, however. This is a logical relationship
               because people are more likely to spend time outside--where these shootings occur--when it is warm
               rather than when it is cold."),
             fluidRow(
               column(width = 10,
                      plotOutput('plot_04', height = "650px")),
               column(width = 2,
                      tableOutput("month_of_shooting_pivot"))
             )
    ),

    # Sixth tab content
    tabPanel("Geospatial Shooting Incident Map",
             h1("Locations of Shooting Hotspots"),
             p("This map shows that shooting hotspots are most higly concentrated in upper Manhattan, the south side of
               the Bronx, and central Brooklyn. There were 145 locations where ten or more shootings occurred (indicated
               by the markers), and there were seven locations with more than thirty shooting incidents."),
             fluidRow(
               column(width = 10,
                      leafletOutput('plot_05', height = "650px")),
               column(width = 2,
                      tableOutput("shooting_coordinates_pivot"))
             )
    ),

    # Seventh tab content
    tabPanel("Shootings by Poverty and Borough",
             h1("Shootings by Poverty and Borough"),
             p("With inconsistent increases in shootings by borough relative to their average poverty rate, it appears
               there is a weak positive correlation between poverty levels and the number of shooting incidents. Poverty
               rate alone is a very ineffective variable in regard to explaining what contributes to the number of shooting
               incidents by borough. The Bronx had the highest poverty rate (33.83%), and Queens had the lowest (15.82%)."),
             fluidRow(
               column(width = 9,
                      plotOutput('plot_06', height = "650px")),
               column(width = 3,
                      tableOutput("poverty_and_shootings_pivot"))
             )
    ),

    # Eighth tab content
    tabPanel("Shootings by Income, Unemployment Rates, and Borough",
             h1("Shootings by Income, Unemployment Rates, and Borough"),
             p("With the exception of the Bronx (or Brooklyn, pending on how you look at it), there is a fairly weak correlation
               between the number of shooting incidents and average unemployment rate, where as unemployment goes down, the number
               of shootings does as well. Average income and the number of shooting incidents share the opposite relationship (inverse
               correlation), and the strength of this relationship is less than that of unemployment and shootings. Staten Island had the
               lowest average unemployment, and Queens had the highest median household income."),
             fluidRow(
               column(width = 8,
                      plotOutput('plot_07', height = "650px")),
               column(width = 4,
                      tableOutput("income_and_shootings_pivot"))
             )
    ),

    # Ninth tab content
    tabPanel("Shootings by Male-to-Female Shooting Incident Ratio and Borough",
             h1("Shootings by Male-to-Female Shooting Incident Ratio and Borough"),
             p("The population ratio expresses the average male-to-female ratio in regards to population, and victimRatio
               expresses the average male-to-female ratio regarding victims of shooting incidents. The Bronx ranks the highest with a final
               ratio value of 15.61 (shown on the chart), which means that, factoring in the male-to-female population ratio, there are 15.61
               men shot for every woman that is shot. Staten Island has the lowest final ratio (11.89)."),
             fluidRow(
               column(width = 9,
                      plotOutput('plot_08', height = "650px")),
               column(width = 3,
                      tableOutput("victim_women_pivot"))
             )
    ),

    # Tenth tab content
    tabPanel("Annual Shooting Rate Per 1000 People by Borough",
             h1("Annual Shooting Rate Per 1000 People by Borough"),
             p("The total number of years included within the NYPD Shooting dataset is seventeen; thus, the shooting rate per 1000
               people was reduced by a factor of seventeen to arrive at the annual values shown. Brooklyn has the highest shooting rate
               among the five, with a value of 93 victims for every 100,000 people, and Staten Island has the lowest, with a value of 12
               shooting victims for every 100,000 people. Queens had the greatest population based on our census data, but the shooting
               rate is far lower than that of Brooklyn and the Bronx; thus, the total number of shooting incidents reported in Queens is 
               relatively small."),
             plotOutput('plot_09', height = "625px"),
             column(4, tableOutput("shooting_rate_pivot"))
    ),

    # Eleventh tab content
    tabPanel("Shootings by Race of Victim and Borough",
             h1("Shootings by Race of Victim and Borough"),
             p("The vast majority of the shooting incidents that occurred, regardless of which borough, involved black individuals.
               Hispanic victims constituted the second largest group. Black victims accounted for 15,618 of the 21,400 shootings that
               were reported, while victims of native backgrounds only accounted for nine of the shootings."),
             fluidRow(
               column(width = 9,
                      plotOutput('plot_10', height = "650px")),
               column(width = 3,
                      tableOutput("shootings_by_race_pivot"))
             )
    ),

    # Twelfth tab content
    tabPanel("Percentage of Shooting Incident Victims by Race and Borough",
             h1("Percentage of Shooting Incident Victims by Race and Borough"),
             p("The sole objective of this chart is to capture the differences in race proportions of the shooting victims. Victims with
               white, Asian, and native backgrounds account for a very small portion of the shooting incidents in each borough. No native
               victims were reported in Queens and Staten Island. As for black victims, they accounted for at least 60% of shooting victims
               in each borough, with values reaching as high as 84.2% in Brooklyn."),
             plotOutput('plot_11', height = "625px"),
             column(4, tableOutput("victim_race_pivot"))
    ),
    
    # Thirteenth tab content
    tabPanel("Percentage of Black Population Versus Percentage of Black Shooting Victims",
             h1("Percentage of Black Population Versus Percentage of Black Shooting Victims"),
             p("While the black population accounts for a large portion of the total population within the five boroughs, there is a clear
               disparity between these statistics and the percentage of shootings involving the black population. The smallest discrepancy 
               occurred in Brooklyn, with a 27% difference/overrepresentation of black individuals in shooting incidents. For Staten Island,
               the ratio between the two is 2.57:1, producing a forty-four percent discrepancy, which is considerably larger than the ratios
               for the other four boroughs."),
               plotOutput('plot_12', height = "625px"),
               column(4,tableOutput("victim_black_pivot"))
    ),
    
    # Fourteenth tab content
    tabPanel("Income by Public Transit Usage Prediction Model",
             h1("Income by Public Transit Usage Prediction Model"),
             p("Each point in this plot represents one of the 21,400 total shooting incidents. The model's prediction line shows the
               relationship between income and and fifteen other selected variables from the final_shooting_data csv file. In the context of this visualization, we can see
               that, on average, households in census tracts where more people are likely to use public transit have lower income than in
               census tracts with lower public transit usage. This provides us with greater insight regarding the socieconomic factors that
               affect the number of shooting incidents in an area."),
             plotOutput('pred_model', height = "625px"),
    ),
    
    # Fifteenth tab content
    tabPanel("Prediction Model Rationale",
             h1("Prediction Model Rationale"),
             p(
               HTML("Implementing the model:<br>
                    One of the models ChatGPT suggested would be the best for creating prediction models with our data would be a linear regression model.
                    We tried implementing this and other predictive model types, e.g., decision tree, random forest, support vector machine modeling, etc.,
                    but we could not get any of them to work due to consistent inabilities to load or install the necessary packages. We did, however, produce
                    a predictive linear regression model ourselves, and we inquired ChatGPT about its rationale for suggesting this type of predictive model.")
             ),
             p(
               HTML('Linear regression model rationale inquiry:<br>
                    When I asked ChatGPT if there "are research studies or journal articles on similar topics that used a predictive linear regression model",
                    the LLM gave me the following scholarly journal article: "Delving into Factors Influencing New
                    York Crime Data with the Tools of Machine Learning".')
             ),
             p(
               HTML('Research article summary:<br>
                    I asked ChatGPT to provide a five-sentence summary of the article, and this is what it returned: "The study focuses on 
                    using machine learning techniques to analyze crime data in New York Citys five boroughs: Brooklyn, Bronx, Manhattan, Queens, 
                    and Staten Island for the year 2019. The goal is to predict crime hotspots and types of criminal offenses using supervised 
                    classification models such as Decision Tree, Multivariate Linear Regression, and kNN. The results show that Multivariate Linear 
                    Regression yielded the highest accuracy in predicting the borough where the crime occurred, while  Decision Tree performed well
                    in predicting the type of crime. The study emphasizes the importance of utilizing data-driven tools for crime pattern detection to 
                    enhance crime detection, prevention, and resource allocation for safer communities."') 
             ),
             p(
               HTML("Research correlation:<br>
                    There is strong correlation between the research article described above and ours because both pertain to identifying the relationships that affect 
                    crimes occuring within the five NY boroughs, so it is logical to use the same type of prediction model, that being a (mutlivariate) linear regression.")
             ),
             p(
               HTML('Article citation:<br>
                    When I inquired about how I would cite research article, ChatGPT returned the following: 
                    Pinto, M., Wei, H., Konate, K., & Touray, I. (2023). Delving into Factors Influencing 
                    New York Crime Data with the Tools of Machine Learning. Journal of Criminal Analysis, 31(3), 234-251.
                    ')
             ),
             p(
               HTML("ChatGPT Citation:<br>
                    The citation for all the inquiries made to ChatGPT for these purposes is displayed in the following:
                    OpenAI. (2023). ChatGPT, Windows. Retrieved May 21, 2023, from https://www.openai.com")
             ),
     
           )
  )
)


server<-function(input,output){
  
  # displaying pivot table for first chart
  output$age_and_shootings_pivot <- renderTable({
    age_and_shootings})

  # displaying pivot table for second chart
  output$public_transit_and_shootings_pivot <- renderTable({
    public_transit_and_shootings})

  # displaying pivot table for third chart
  output$year_of_shooting_pivot <- renderTable({
    year_of_shooting})

  # displaying pivot table for fourth chart
  output$month_of_shooting_pivot <- renderTable({
    month_of_shooting})

  # displaying pivot table for fifth chart
  output$shooting_coordinates_pivot <- renderTable({
    shooting_coordinates})

  # displaying pivot table for sixth chart
  output$poverty_and_shootings_pivot <- renderTable({
    poverty_and_shootings})

  # displaying pivot table for seventh chart
  output$income_and_shootings_pivot <- renderTable({
    income_and_shootings})

  # displaying pivot table for eighth chart
  output$victim_women_pivot <- renderTable({
    victim_women_pivot})
  
  # displaying pivot table for ninth chart
  output$shooting_rate_pivot <- renderTable({
    shooting_rate})
  
  # displaying pivot table for tenth chart
  output$shootings_by_race_pivot <- renderTable({
    shootings_by_race})
  
  # displaying pivot table for eleventh chart
  output$victim_race_pivot <- renderTable({
    victim_race_pivot})
  
  # displaying pivot table for twelfth chart
  output$victim_black_pivot <- renderTable({
    victim_black_pivot})
  
  # creating a heatmap for shootings by borough and age of victim
  age_and_shootings_heatmap <-
    ggplot(age_and_shootings, aes(x=Borough, y=VIC_AGE_GROUP, fill=Count)) +
    geom_tile() +
    scale_fill_gradient(low="lightblue", high="black") +
    labs(title = "Heatmap of Shootings by Borough and Age of Victim", x = "Borough", y = "Age of Victim", y = "# of Shooting Incidents") +
    theme_minimal() +
    geom_text(aes(label = Count), color = "white")
  
  # creating a lollipop chart for shootings by public transit usage and borough
  public_transit_and_shootings_chart <-
    ggplot(public_transit_and_shootings, aes(x = Borough, y = Total_shootings)) +
    geom_segment(aes(x = reorder(Borough, -Avg_transit), xend = reorder(Borough, -Avg_transit), y = 0, yend = Total_shootings, color = Avg_transit), lwd = 2.5) +
    geom_point(aes(color = Avg_transit), size = 10) +
    scale_color_gradient(low = "#FFC300", high = "#C70039") +
    labs(title = "Effect of Public Transit Usage on Number of Shootings in Each Borough",
         x = "Borough",
         y = "# of Shooting Incidents",
         color = "% of Population Commuting on Public Transit")
  
  # creating a line chart for shootings by year and borough
  year_of_shooting_chart <-
    ggplot(year_of_shooting, aes(x = year, y = count, group = Borough)) +
    geom_line(aes(colour = Borough), lwd=1.0) + 
    xlab("Year") + 
    ylab("# of Shooting Incidents") + 
    ggtitle("# of Shooting Incidents Each Year by Borough")
  
  # creating a line chart for shootings by month and borough
  month_of_shooting_chart <-
    ggplot(month_of_shooting, aes(x = factor(month, levels = month.name), y = count, group = Borough)) +
    geom_line(aes(colour = Borough), lwd=1.0) + 
    xlab("Month") + 
    ylab("# of Shooting Incidents") + 
    ggtitle("Cumulative # of Shooting Incidents Each Month by Borough") 
  
  # creating a geospatial map for locations of shooting hotspots
  shooting_coordinates_map <-
    leaflet(shooting_coordinates) %>%
    addTiles() %>%
    addAwesomeMarkers(data = shooting_coordinates, lng = ~longitude, lat = ~latitude, label = ~Shootings, icon = awesomeIcons(icon = "star", markerColor = "darkblue")) 
  
  # creating a bar chart for shootings by poverty and borough
  poverty_and_shootings_chart <-
    ggplot(poverty_and_shootings, aes(x = reorder(Borough, Total_shootings), y = Total_shootings, fill = Avg_poverty)) +
    geom_col() +
    labs(x = "Borough", y = "# of Shooting Incidents", fill = "Average Poverty %") +
    scale_fill_gradient(low="cyan", high="black") +
    ggtitle("Effect of Poverty on Number of Shootings in Each Borough") +
    theme_minimal()
  
  # creating a bubble chart for shootings by income, unemployment rates, and borough
  income_and_shootings_chart <-
    ggplot(income_and_shootings, aes(x = reorder(Borough, -Avg_unemployment), y = Total_shootings, size = Avg_unemployment, color = Avg_income)) +
    geom_point() +
    geom_text(aes(label = paste0("$",Avg_income), vjust = 1.5)) + 
    scale_size(range = c(5,10)) +
    scale_color_gradient(low="#FFC330", high="steelblue") +
    ggtitle("Effects of Income and Unemployment Rates on Shootings") +
    labs(x = "Borough", y = "# of Shooting Incidents", size = "Average Unemployment Rate", color = "Average Income")
  
  # creating a bar chart for shootings by male-to-female shooting incident ratio and borough
  sex_ratio_and_shootings_chart <- 
    ggplot(victim_women_pivot, aes(x = reorder(Borough, -victimRatio/popRatio), y = totalShootings, fill = victimRatio/popRatio)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(low="white", high="darkblue") +
    labs(x = "Borough", y = "# of Shooting Incidents", fill = "Victim Ratio Versus Population Ratio")

  # creating a bar chart for annual shooting rate per 1000 people by borough
  shooting_rate_chart <- 
    ggplot(shooting_rate, aes(x = reorder(Borough, -Shooting_rate), y = Shooting_rate, fill = as.factor(Borough))) +
    geom_col(show.legend = FALSE) +
    labs(x = "Borough", y = "Annual # of Shootings Per 1000 People") +
    ggtitle("Annual # of Shooting Incidents Per 1000 People by Borough") +
    geom_text(aes(label = Shooting_rate, vjust = 1.5)) +
    theme_minimal()
  
  # creating a grouped bar chart for shootings by race of victim and borough
  shootings_by_race_chart <-
    ggplot(shootings_by_race, aes(x = Borough, y = count, fill = VIC_RACE)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Borough", y = "# of Shooting Incidents", fill = "Race of Victim") +
    ggtitle("Shooting Incidents by Borough and Race of Victim") +
    theme_minimal()
    
  # creating a bubble chart for percentage of shooting incident victims by race and borough  
  victim_race_pivot_chart <-
    ggplot(victim_race_pivot, aes(x = Borough)) +
    geom_point(aes(y = blackPercent/100, color = "Percentage of Shootings Involving Black Population"), size = 5) +
    geom_point(aes(y = asianPercent/100, color = "Percentage of Shootings Involving Asian Population"), size = 5) +
    geom_point(aes(y = whitePercent/100, color = "Percentage of Shootings Involving White Population"), size = 5) +
    geom_point(aes(y = hispanicPercent/100, color = "Percentage of Shootings Involving Hispanic Population"), size = 5) +
    geom_point(aes(y = nativePercent/100, color = "Percentage of Shootings Involving Native Population"), size = 5) +
    labs(y = "% of Shooting Incident Victims by Race", color = "Color Meaning") +
    ggtitle("Demographic Breakdown of Shootings by Borough")
  
  # creating a lollipop/bubble chart for percentage of black population versus percentage of black shooting victims
  victim_black_pivot_chart <-
    ggplot(victim_black_pivot, aes(x = Borough)) +
    geom_point(aes(y = blackPercent/100, color = "Percentage of Shootings Involving Black Population"), size = 5) +
    geom_segment(aes(x = Borough, xend = Borough, y = 0, yend = populationAverageBlack/100), color = "blue") +
    geom_point(aes(x = Borough, y = populationAverageBlack/100, color = "Percentage of Black Population"), size = 5) +
    geom_text(aes(y = blackPercent/100, label = round(blackPercent/100, 2)), vjust = 2, color = "black") +
    geom_text(aes(y = populationAverageBlack/100, label = round(populationAverageBlack/100, 2)), vjust = -1, color = "black") +
    labs(y = "% of Black Population", color = "Color Meaning") +
    ggtitle("Black Shooting Ratio Versus Black Population Ratio by Borough") +
    scale_color_manual(values = c("Percentage of Shootings Involving Black Population" = "red", "Percentage of Black Population" = "blue"), 
                       labels = c("Percentage of Shootings Involving Black Population", "Percentage of Black Population")) +
    scale_size_manual(values = c(5), 
                      labels = c("Percentage of Shootings Involving Black Population", "Percentage of Black Population"))
  
  # creating and plotting linear regression model
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
  
    
  # displaying chart visualizations
  output$plot_01 = renderPlot({age_and_shootings_heatmap})
  output$plot_02 = renderPlot({public_transit_and_shootings_chart})
  output$plot_03 = renderPlot({year_of_shooting_chart})
  output$plot_04 = renderPlot({month_of_shooting_chart})
  output$plot_05 = renderLeaflet({shooting_coordinates_map})
  output$plot_06 = renderPlot({poverty_and_shootings_chart})
  output$plot_07 = renderPlot({income_and_shootings_chart})
  output$plot_08 = renderPlot({sex_ratio_and_shootings_chart})
  output$plot_09 = renderPlot({shooting_rate_chart})
  output$plot_10 = renderPlot({shootings_by_race_chart})
  output$plot_11 = renderPlot({victim_race_pivot_chart})
  output$plot_12 = renderPlot({victim_black_pivot_chart})
  
  
}


shinyApp(ui=ui, server=server) 
