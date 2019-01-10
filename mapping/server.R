library(DT)
library(shiny)
library(ggplot2)
library(leaflet)
library(rworldmap)
library(dplyr)
library(sf)
library(mapview)

trying.again.wfto <- read.delim("trying again wfto copy.txt", 
                                comment.char="#")
wfto <- trying.again.wfto

CountryName <- wfto$CountryName

shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    
    wfto <- trying.again.wfto
    
    if(input$highlight == 1) {
      ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
        geom_histogram (stat="count", show.legend = FALSE) + 
        labs(title="WFTO Asia Member Organizations by Country") +
        labs(x="Country", y="Number of Member Organizations") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1))
    }
    else if(input$highlight == 2) {
      ggplot(wfto, aes(wfto$CountryName, fill = wfto$Membership.Type)) + 
        scale_fill_discrete(name="Membership Type", breaks=c("FTN", "FTO", "FTSO"), 
                            labels=c("Fair Trade Network", "Fair Trade Organization", "Fair Trade Support Organization")) +
        geom_histogram (stat="count", show.legend = TRUE) + 
        labs(title="WFTO Asia Member Organizations by Membership Type") +
        labs(x="Country", y="Number of Member Organizations") +
        theme(text = element_text(size=20), 
              axis.text.x = element_text(angle=45, hjust=1))
    }
    else if(input$highlight == 3) {
      ggplot(wfto, aes(wfto$CountryName, fill = wfto$Guaranteed.)) + 
        scale_fill_discrete(name="Membership Status", labels=c("Provisional", "Guaranteed")) +
        geom_histogram (stat="count", show.legend = TRUE) + 
        labs(title="WFTO Asia Member Organizations by Membership Status") +
        labs(x="Country", y="Number of Member Organizations") +
        theme(text = element_text(size=20), 
              axis.text.x = element_text(angle=45, hjust=1))
      
    }
    
  })
  
  output$selected_mekong <- renderTable ({
    
    CountryName <- wfto$CountryName 
    
    if(input$mekong == 1) {
      wfto %>% 
        filter(CountryName == "Armenia") %>% 
        select (Organisation.Name)
    }
    else if (input$mekong == 2) {
      wfto %>% 
        filter(CountryName == "Bangladesh") %>% 
        select (Organisation.Name)
    }
    else if (input$mekong == 3) {
      wfto %>% 
        filter(CountryName == "Cambodia") %>% 
        select (Organisation.Name)
    }
    
    else if (input$mekong == 4) {
      wfto %>% 
        filter(CountryName == "China") %>% 
        select(Organisation.Name)
    }
    
    else if (input$mekong == 5) {
      wfto %>% 
        filter(CountryName == "Hong Kong") %>% 
        select (Organisation.Name)
    }
    
    else if (input$mekong == 6) {
      wfto %>% 
        filter(CountryName == "India") %>% 
        select (Organisation.Name)
      
    }
    
    else if (input$mekong == 7) {
      wfto %>% 
        filter(CountryName == "Indonesia") %>% 
        select (Organisation.Name)
      
    }
    
    else if (input$mekong == 8) {
      wfto %>% 
        filter(CountryName == "Japan") %>% 
        select (Organisation.Name)
      
    }
    
    else if (input$mekong == 9) {
      wfto %>% 
        filter(CountryName == "Korea") %>% 
        select (Organisation.Name)
      
    }
    
    else if (input$mekong == 10) {
      wfto %>% 
        filter(CountryName == "Loas") %>% 
        select (Organisation.Name)
      
    }
    
    else if (input$mekong == 11) {
      wfto %>% 
        filter(CountryName == "Malaysia") %>% 
        select (Organisation.Name)
      
    }
    
    else if (input$mekong == 12) {
      wfto %>% 
        filter(CountryName == "Mongolia") %>% 
        select (Organisation.Name)
    
    }
    
    else if (input$mekong == 13) {
      wfto %>% 
        filter(CountryName == "Nepal") %>% 
        select (Organisation.Name)
    }
    
    else if (input$mekong == 14) {
      wfto %>% 
        filter(CountryName == "Pakistan") %>% 
        select (Organisation.Name)
      
    }
    
    else if (input$mekong == 15) {
      wfto %>% 
        filter(CountryName == "Philippines") %>% 
        select (Organisation.Name)
      
    }
    
    else if (input$mekong == 16) {
      wfto %>% 
        filter(CountryName == "Sri Lanka") %>% 
        select (Organisation.Name)
      
    }
    
    else if (input$mekong == 17) {
      wfto %>% 
        filter(CountryName == "Taiwan") %>% 
        select (Organisation.Name)
      
    }
    
    else if (input$mekong == 18) {
      wfto %>% 
        filter(CountryName == "Thailand") %>% 
        select (Organisation.Name)
    }
    
    else if (input$mekong == 19) {
      wfto %>% 
        filter(CountryName == "Vietnam") %>% 
        select (Organisation.Name)
      
    }
    
    
  })
  
})
