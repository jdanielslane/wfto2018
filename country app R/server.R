#install.packages("DT")

library(DT)
library(shiny)
library(ggplot2)
library(leaflet)
library (rworldmap)
library(dplyr)

trying.again.wfto <- read.delim("trying again wfto.txt", 
                      comment.char="#")
wfto <- trying.again.wfto

CountryName <- wfto$CountryName

shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    
    wfto <- trying.again.wfto
    
    ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
      geom_histogram (stat="count", show.legend = FALSE) + 
      labs(title="WFTO Asia Member Organizations by Country") +
      labs(x="Country", y="Number of Member Organizations") +
      theme(text = element_text(size=20),
            axis.text.x = element_text(angle=45, hjust=1))
    
})
  
  output$selected_mekong <- renderTable ({
  
    CountryName <- wfto$CountryName 
    
    if(input$mekong == 1) {
      wfto %>% 
        filter(CountryName == "Cambodia") %>% 
        select (Organisation.Name)
    }
    else if (input$mekong == 2) {
      wfto %>% 
        filter(CountryName == "Laos") %>% 
        select (Organisation.Name)
    }
    else if (input$mekong == 3) {
      wfto %>% 
        filter(CountryName == "Thailand") %>% 
        select (Organisation.Name)
    }
    
    else if (input$mekong == 4) {
      wfto %>% 
        filter(CountryName == "Myanmar") %>% 
        select(Organisation.Name)
    }
    
    else if (input$mekong == 5) {
      wfto %>% 
        filter(CountryName == "Vietnam") %>% 
        select (Organisation.Name)
    }
  })
    
   
    
  })


