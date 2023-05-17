library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
library(DT)
library(data.table)
library(lubridate)
library(leaflet)
library(modelr)
library(tidyverse)

rm(list = ls())

setwd("C:/Users/lukec/OneDrive/Documents/data332/DATA332Final/pivotTables")

# change names when pivot tables are made
# add or remove according to needs
#vis1 <- read.csv("  .csv")
#vis2 <- read.csv("  .csv")
year_of_shooting <- read.csv("year_of_shooting.csv")
month_of_shooting <- read.csv("month_of_shooting.csv")
shooting_coordinates <- read.csv("shooting_coordinates.csv")
poverty_and_shootings <- read.csv("poverty_and_shootings.csv")
unemployment_and_shootings <- read.csv("unemployment_and_shootings.csv")
income_and_shootings <- read.csv("income_and_shootings.csv")


#setting up ui
ui<-fluidPage( 
  
  titlePanel(title = "Awesome Shooting Title"),
  h4("Awesome Shooting Subtitle"),
  
  # Create a tabset panel with eight tabs
  tabsetPanel(
    
    # First tab content
    # tabPanel("vis1",
    #          div(style = "text-align: center;",
    #              h1("vis1 title"),
    #              p("Chart explanation"),
    #              plotOutput('plot_01', height = "675px"),
    #              # creating a pivot table object
    #              column(2, tableOutput("vis_1_pivot"))
    #          )
    # ),
    
    # Second tab content
    # tabPanel("vis2",
    #          div(style = "text-align: center;",
    #              h1("vis2 title"),
    #              p("Chart explanation"),
    #              plotOutput('plot_02', height = "675px"),
    #              # creating a pivot table object
    #              column(2, tableOutput("vis_2_pivot"))
    #          )
    # ),
    
    # Third tab content
    tabPanel("Shootings by Year and Borough",
             div(style = "text-align: center;",
                 h1("Shootings by Year and Borough"),
                 p("Chart explanation"),
                 plotOutput('plot_03', height = "675px"),
                 # creating a pivot table object
                 column(2, tableOutput("year_of_shooting_pivot"))
             )
    ),
    
    # Fourth tab content
    tabPanel("Shootings by Month and Borough",
             div(style = "text-align: center;",
                 h1("Shootings by Month and Borough"),
                 p("Chart explanation"),
                 plotOutput('plot_04', height = "675px"),
                 # creating a pivot table object
                 column(2, tableOutput("month_of_shooting_pivot"))
             )
    ),
    
    # Fifth tab content
    tabPanel("Geospatial Shooting Incident Map",
             div(style = "text-align: center;",
                 h1("Locations of Shootings"),
                 p("Chart explanation"),
                 leafletOutput('plot_05', height = "675px"),
                 # creating a pivot table object
                 column(2, tableOutput("shooting_coordinates_pivot"))
             )
    ),
    
    # Sixth tab content
    tabPanel("Effect of Poverty on Shootings by Borough",
             div(style = "text-align: center;",
                 h1("Effect of Poverty on Shootings by Borough"),
                 p("Effect of Poverty on Shootings by Borough"),
                 plotOutput('plot_06', height = "675px"),
                 # creating a pivot table object
                 column(2, tableOutput("poverty_and_shootings_pivot"))
             )
    ),
    
    # Seventh tab content
    tabPanel("Effects of Income and Unemployment Rates on Shootings",
             div(style = "text-align: center;",
                 h1("Effects of Income and Unemployment Rates on Shootings"),
                 p("Chart explanation"),
                 plotOutput('plot_07', height = "675px"),
                 # creating a pivot table object
                 column(2, tableOutput("income_and_shootings_pivot"))
             )
    ),
    
    # Eighth tab content
    tabPanel("vis8",
             div(style = "text-align: center;",
                 h1("vis8 title"),
                 p("Chart explanation"),
                 plotOutput('plot_08', height = "675px"),
                 # creating a pivot table object
                 column(2, tableOutput("vis_8_pivot"))
             )
    ),
    
    # Ninth tab content
    # tabPanel("Leaflet map",
    #          div(style = "text-align: center;",
    #              h1("map title"),
    #              p("chart explanation"),
    #              leafletOutput('map', height = "675px")
    #          )
    # ),
    
  )
)


server<-function(input,output){
  
  # # displaying pivot table
  # vis_1_pivot <- renderTable({
  #   vis1_dataframe})
  # 
  # # displaying pivot table
  # vis_2_pivot <- renderTable({
  #   vis2_dataframe})
  # 
  # # displaying pivot table
  # year_of_shooting_pivot <- renderTable({
  #   year_of_shooting})
  # 
  # # displaying pivot table
  # month_of_shooting_pivot <- renderTable({
  #   month_of_shooting})
  # 
  # # displaying pivot table
  # shooting_coordinates_pivot <- renderTable({
  #   shooting_coordinates})
  # 
  # # displaying pivot table
  # poverty_and_shootings_pivot <- renderTable({
  #   poverty_and_shootings})
  # 
  # # displaying pivot table
  # income_and_shootings_pivot <- renderTable({
  #   income_and_shootings})
  # 
  # # displaying pivot table
  # vis8_pivot <- renderTable({
  #   vis8_dataframe})
  
  #vis1 <-
    #chart code
  
  #vis2 <-
    #chart code
  
  year_of_shooting <-
    ggplot(year_of_shooting, aes(x = year, y = count, group = Borough)) +
    geom_line(aes(colour = Borough), lwd=1.0) + 
    xlab("Year") + 
    ylab("# of Shooting Incidents") + 
    ggtitle("# of Shooting Incidents Each Year by Borough")
  
  month_of_shooting <-
    ggplot(month_of_shooting, aes(x = month, y = count, group = Borough)) +
    geom_line(aes(colour = Borough), lwd=1.0) + 
    xlab("Month") + 
    ylab("# of Shooting Incidents") + 
    ggtitle("Cumulative # of Shooting Incidents Each Month by Borough") 
  
  shooting_coordinates <-
    leaflet(shooting_coordinates) %>%
    addTiles() %>%
    addAwesomeMarkers(data = shooting_coordinates, lng = ~longitude, lat = ~latitude, label = ~Shootings, icon = awesomeIcons(icon = "star", markerColor = "darkblue")) 
  
  poverty_and_shootings <-
    ggplot(poverty_and_shootings, aes(x = reorder(Borough, Total_shootings), y = Total_shootings, fill = Avg_poverty)) +
    geom_col() +
    labs(x = "Borough", y = "# of Shooting Incidents", fill = "Average Poverty %") +
    scale_fill_gradient(low="cyan", high="black") +
    ggtitle("Effect of Poverty on Number of Shootings in Each Borough") +
    theme_minimal()
  
   income_and_shootings<-
    ggplot(income_and_shootings, aes(x = reorder(Borough, -Avg_unemployment), y = Total_shootings, size = Avg_unemployment, color = Avg_income)) +
    geom_point() +
    geom_text(aes(label = paste0("$",Avg_income), vjust = 1.5)) + 
    scale_size(range = c(5,10)) +
    scale_color_gradient(low="#FFC330", high="steelblue") +
    labs(x = "Borough", y = "# of Shooting Incidents", size = "Average Unemployment", color = "Average Income")
  
  #vis8 <-
    #chart code

  
  
  
  # displaying chart visualizations
  output$plot_01 = renderPlot({vis1})
  output$plot_02 = renderPlot({vis2})
  output$plot_03 = renderPlot({year_of_shooting})
  output$plot_04 = renderPlot({month_of_shooting})
  output$plot_05 = renderLeaflet({shooting_coordinates})
  output$plot_06 = renderPlot({poverty_and_shootings})
  output$plot_07 = renderPlot({income_and_shootings})
  output$plot_08 = renderPlot({vis8})
  output$map = renderLeaflet({leaflet_map})
  
  
}


shinyApp(ui=ui, server=server) 