library(shiny)
library(ggplot2)
library(dplyr)
library(sf)
library(mapview)

trying.again.wfto <- read.delim("trying again wfto copy 3.txt", 
                                comment.char="#")
wfto <- trying.again.wfto
CountryName <- wfto$CountryName

shinyUI(fluidPage(
  
  titlePanel("WFTO Asia Snapshot"), 
  sidebarLayout(
    sidebarPanel(
      selectInput("highlight", "Select a Country:",
                  choices = c ("All" = 1,
                                "Armenia" = 2, 
                               "Bangladesh" = 3, 
                               "Cambodia" = 4, 
                               "China"= 5, 
                               "Hong Kong"= 6,
                               "India" = 7,
                               "Indonesia" = 8,
                               "Japan" = 9,
                               "Korea" = 10,
                               "Loas" = 11,
                               "Malaysia" = 12,
                               "Mongolia" = 13,
                               "Nepal" = 14,
                               "Pakistan" = 15,
                               "Philippines" = 16,
                               "Sri Lanka" = 17,
                               "Taiwan" = 18,
                               "Thailand" = 19,
                               "Vietnam" = 20
                  ) ,
                  selected = "Thailand")
    ),
    
    mainPanel(
      plotOutput("distPlot")
      )
    ))
)


