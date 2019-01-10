
library(shiny)
library(ggplot2)
library(dplyr)
library(sf)
library(mapview)

trying.again.wfto <- read.delim("trying again wfto copy.txt", 
                                comment.char="#")
wfto <- trying.again.wfto
CountryName <- wfto$CountryName

shinyUI(fluidPage(
  
  titlePanel("WFTO Asia Snapshot"), 
  sidebarLayout(
    sidebarPanel(
      selectInput("highlight", "Select a Histogram:",
                  choices = c ( "Members by Country" = 1,
                                "Members by Type" = 2,
                                "Members by Status" = 3
                  )),
      selectInput("mekong", "Choose country to view a list of members:",
                  choices = c ("Armenia" = 1, 
                               "Bangladesh" = 2, 
                               "Cambodia" = 3, 
                               "China"= 4, 
                               "Hong Kong"= 5,
                               "India" = 6,
                               "Indonesia" = 7,
                               "Japan" = 8,
                               "Korea" = 9,
                               "Loas" = 10,
                               "Malaysia" = 11,
                               "Mongolia" = 12,
                               "Nepal" = 13,
                               "Pakistan" = 14,
                               "Philippines" = 15,
                               "Sri Lanka" = 16,
                               "Taiwan" = 17,
                               "Thailand" = 18,
                               "Vietnam" = 19
                               ) ,
                  selected = "Thailand")
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Countries", plotOutput("distPlot"),
                           br(), br(),
                           tableOutput("selected_mekong")        
                           ),
                  tabPanel("Languages", plotOutput("distplot2")),
                  tabPanel("Members", tableOutput("allmems"))
      )
    ))
)
)

