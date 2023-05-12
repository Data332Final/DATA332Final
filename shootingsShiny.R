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

setwd(C:/Users/lukec/OneDrive/Documents/data332/DATA332Final/pivotTables)

# change names when pivot tables are made
# add or remove according to needs
vis1 <- read.csv("  .csv")
vis2 <- read.csv("  .csv")
vis3 <- read.csv("  .csv")
vis4 <- read.csv("  .csv")
vis5 <- read.csv("  .csv")
vis6 <- read.csv("  .csv")
vis7 <- read.csv("  .csv")
vis8 <- read.csv("  .csv")


#setting up ui
ui<-fluidPage( 
  
  titlePanel(title = "Awesome Shooting Title"),
  h4("Awesome Shooting Subtitle"),
  
  # Create a tabset panel with eight tabs
  tabsetPanel(
    
    # First tab content
    tabPanel("vis1",
             div(style = "text-align: center;",
                 h1("vis1 title"),
                 p("Chart explanation"),
                 plotOutput('plot_01', height = "675px"),
                 # creating a pivot table object
                 column(2, tableOutput("vis_1_pivot"))
             )
    ),
    
    # Second tab content
    tabPanel("vis2",
             div(style = "text-align: center;",
                 h1("vis2 title"),
                 p("Chart explanation"),
                 plotOutput('plot_02', height = "675px"),
                 # creating a pivot table object
                 column(2, tableOutput("vis_2_pivot"))
             )
    ),
    
    # Third tab content
    tabPanel("vis3",
             div(style = "text-align: center;",
                 h1("vis3 title"),
                 p("Chart explanation"),
                 plotOutput('plot_03', height = "675px"),
                 # creating a pivot table object
                 column(2, tableOutput("vis_3_pivot"))
             )
    ),
    
    # Fourth tab content
    tabPanel("vis4",
             div(style = "text-align: center;",
                 h1("vis4 title"),
                 p("Chart explanation"),
                 plotOutput('plot_04', height = "675px"),
                 # creating a pivot table object
                 column(2, tableOutput("vis_4_pivot"))
             )
    ),
    
    # Fifth tab content
    tabPanel("vis5",
             div(style = "text-align: center;",
                 h1("vis5 title"),
                 p("Chart explanation"),
                 plotOutput('plot_05', height = "675px"),
                 # creating a pivot table object
                 column(2, tableOutput("vis_5_pivot"))
             )
    ),
    
    # Sixth tab content
    tabPanel("vis6",
             div(style = "text-align: center;",
                 h1("vis6 title"),
                 p("Chart explanation"),
                 plotOutput('plot_06', height = "675px"),
                 # creating a pivot table object
                 column(2, tableOutput("vis_6_pivot"))
             )
    ),
    
    # Seventh tab content
    tabPanel("vis7",
             div(style = "text-align: center;",
                 h1("vis7 title"),
                 p("Chart explanation"),
                 plotOutput('plot_07', height = "675px"),
                 # creating a pivot table object
                 column(2, tableOutput("vis_7_pivot"))
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
    tabPanel("Leaflet map",
             div(style = "text-align: center;",
                 h1("map title"),
                 p("chart explanation"),
                 leafletOutput('map', height = "675px")
             )
    ),
    
  )
)


server<-function(input,output){
  
  # displaying pivot table
  vis_1_pivot <- renderTable({
    vis1_dataframe})
  
  # displaying pivot table
  vis_2_pivot <- renderTable({
    vis2_dataframe})
  
  # displaying pivot table
  vis3_pivot <- renderTable({
    vis3_dataframe})
  
  # displaying pivot table
  vis4_pivot <- renderTable({
    vis4_dataframe})
  
  # displaying pivot table
  vis5_pivot <- renderTable({
    vis5_dataframe})
  
  # displaying pivot table
  vis6_pivot <- renderTable({
    vis6_dataframe})
  
  # displaying pivot table
  vis7_pivot <- renderTable({
    vis7_dataframe})
  
  # displaying pivot table
  vis8_pivot <- renderTable({
    vis8_dataframe})
  
  vis1 <-
    chart code
  
  vis2 <-
    chart code
  
  vis3 <-
    chart code
  
  vis4 <-
    chart code
  
  vis5 <-
    chart code
  
  vis6 <-
    chart code
  
  vis7 <-
    chart code
  
  vis8 <-
    chart code

  leaflet_map <-
    leaflet() %>% 
    addTiles() %>% 
    addMarkers(lng = , lat = , label = )
  
  
  # displaying chart visualizations
  output$plot_01 = renderPlot({vis1})
  output$plot_02 = renderPlot({vis2})
  output$plot_03 = renderPlot({vis3})
  output$plot_04 = renderPlot({vis4})
  output$plot_05 = renderPlot({vis5})
  output$plot_06 = renderPlot({vis6})
  output$plot_07 = renderPlot({vis7})
  output$plot_08 = renderPlot({vis8})
  output$map = renderLeaflet({leaflet_map})
  
  
}


shinyApp(ui=ui, server=server) 