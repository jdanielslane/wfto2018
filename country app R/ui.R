
library(shiny)
library(ggplot2)
library(dplyr)

trying.again.wfto <- read.delim("trying again wfto.txt", 
                                comment.char="#")
wfto <- trying.again.wfto
CountryName <- wfto$CountryName

shinyUI(fluidPage(
  
  titlePanel("WFTO Asia Snapshot"), 
  sidebarLayout(
    sidebarPanel(
    selectInput ("country", "All Countries:",
          choices = wfto$CountryName,
          selected = "Thailand"),
    selectInput("mekong", "Choose country from Greater Mekong region for list of WFTO Asia members:",
              choices = c ("Cambodia" = 1, 
                 "Laos" = 2, 
                 "Thailand" = 3, 
                 "Myanmar"= 4, 
                 "Vietnam"= 5) ,
    selected = "Thailand")
),
    
  mainPanel(
    plotOutput("distPlot"),
     br(), br(),
    tableOutput("selected_mekong")
))
)
)



