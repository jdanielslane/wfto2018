library(DT)
library(shiny)
library(ggplot2)
library(leaflet)
library(rworldmap)
library(dplyr)
library(sf)
library(mapview)

trying.again.wfto <- read.delim("trying again wfto copy 3.txt", 
                                comment.char="#")
wfto <- trying.again.wfto

CountryName <- wfto$CountryName

shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    
    wfto <- trying.again.wfto
    CountryName <- wfto$CountryName 
    
    if(input$highlight == 1) {
      ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
        geom_bar (stat="count", show.legend = FALSE) + 
        scale_fill_manual(values=c(rep("#267DC1",19))) +
        labs(title="WFTO Asia Member Organizations by Country") +
        labs(x="Country", y="Number of Member Organizations") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1))
    } 
    else if(input$highlight == 2) {
      ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
        geom_bar (stat="count", show.legend = FALSE) + 
        scale_fill_manual(values=c("#267DC1", rep("dark grey",18))) +
        labs(title="WFTO Asia Member Organizations by Country") +
        labs(x="Country", y="Number of Member Organizations") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1))
    }
    else if (input$highlight == 3) {
      ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
        geom_bar (stat="count", show.legend = FALSE) + 
        scale_fill_manual(values=c(rep("dark grey", 1), "#267DC1", rep("dark grey",17))) +
        labs(title="WFTO Asia Member Organizations by Country") +
        labs(x="Country", y="Number of Member Organizations") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1))
    }
    else if (input$highlight == 4) {
      ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
        geom_bar (stat="count", show.legend = FALSE) + 
        scale_fill_manual(values=c(rep("dark grey", 2), "#267DC1", rep("dark grey",16))) +
        labs(title="WFTO Asia Member Organizations by Country") +
        labs(x="Country", y="Number of Member Organizations") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1))
    }
    else if (input$highlight == 5) {
      ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
        geom_bar (stat="count", show.legend = FALSE) + 
        scale_fill_manual(values=c(rep("dark grey", 3), "#267DC1", rep("dark grey",15))) +
        labs(title="WFTO Asia Member Organizations by Country") +
        labs(x="Country", y="Number of Member Organizations") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1))
    }
    else if (input$highlight == 6) {
      ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
        geom_bar (stat="count", show.legend = FALSE) + 
        scale_fill_manual(values=c(rep("dark grey", 4), "#267DC1", rep("dark grey",14))) +
        labs(title="WFTO Asia Member Organizations by Country") +
        labs(x="Country", y="Number of Member Organizations") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1))
    }
    else if (input$highlight == 7) {
      ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
        geom_bar (stat="count", show.legend = FALSE) + 
        scale_fill_manual(values=c(rep("dark grey", 5), "#267DC1", rep("dark grey",13))) +
        labs(title="WFTO Asia Member Organizations by Country") +
        labs(x="Country", y="Number of Member Organizations") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1))
      
    }
    
    else if (input$highlight == 8) {
      ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
        geom_bar (stat="count", show.legend = FALSE) + 
        scale_fill_manual(values=c(rep("dark grey", 6), "#267DC1", rep("dark grey",12))) +
        labs(title="WFTO Asia Member Organizations by Country") +
        labs(x="Country", y="Number of Member Organizations") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1))
      
    }
    
    else if (input$highlight == 9) {
      ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
        geom_bar (stat="count", show.legend = FALSE) + 
        scale_fill_manual(values=c(rep("dark grey", 7), "#267DC1", rep("dark grey",11))) +
        labs(title="WFTO Asia Member Organizations by Country") +
        labs(x="Country", y="Number of Member Organizations") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1))
      
    }
    
    else if (input$highlight == 10) {
      ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
        geom_bar (stat="count", show.legend = FALSE) + 
        scale_fill_manual(values=c(rep("dark grey", 8), "#267DC1", rep("dark grey",10))) +
        labs(title="WFTO Asia Member Organizations by Country") +
        labs(x="Country", y="Number of Member Organizations") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1))
      
    }
    
    else if (input$highlight == 11) {
      ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
        geom_bar (stat="count", show.legend = FALSE) + 
        scale_fill_manual(values=c(rep("dark grey", 9), "#267DC1", rep("dark grey",9))) +
        labs(title="WFTO Asia Member Organizations by Country") +
        labs(x="Country", y="Number of Member Organizations") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1))
      
    }
    
    else if (input$highlight == 12) {
      ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
        geom_bar (stat="count", show.legend = FALSE) + 
        scale_fill_manual(values=c(rep("dark grey", 10), "#267DC1", rep("dark grey",8))) +
        labs(title="WFTO Asia Member Organizations by Country") +
        labs(x="Country", y="Number of Member Organizations") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1))
      
    }
    
    else if (input$highlight == 13) {
      ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
        geom_bar (stat="count", show.legend = FALSE) + 
        scale_fill_manual(values=c(rep("dark grey", 11), "#267DC1", rep("dark grey",7))) +
        labs(title="WFTO Asia Member Organizations by Country") +
        labs(x="Country", y="Number of Member Organizations") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1))
      
    }
    
    else if (input$highlight == 14) {
      ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
        geom_bar (stat="count", show.legend = FALSE) + 
        scale_fill_manual(values=c(rep("dark grey", 12), "#267DC1", rep("dark grey",6))) +
        labs(title="WFTO Asia Member Organizations by Country") +
        labs(x="Country", y="Number of Member Organizations") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1))
    }
    
    else if (input$highlight == 15) {
      ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
        geom_bar (stat="count", show.legend = FALSE) + 
        scale_fill_manual(values=c(rep("dark grey", 13), "#267DC1", rep("dark grey",5))) +
        labs(title="WFTO Asia Member Organizations by Country") +
        labs(x="Country", y="Number of Member Organizations") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1))
      
    }
    
    else if (input$highlight == 16) {
      ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
        geom_bar (stat="count", show.legend = FALSE) + 
        scale_fill_manual(values=c(rep("dark grey", 14), "#267DC1", rep("dark grey",4))) +
        labs(title="WFTO Asia Member Organizations by Country") +
        labs(x="Country", y="Number of Member Organizations") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1))
      
    }
    
    else if (input$highlight == 17) {
      ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
        geom_bar (stat="count", show.legend = FALSE) + 
        scale_fill_manual(values=c(rep("dark grey", 15), "#267DC1", rep("dark grey",3))) +
        labs(title="WFTO Asia Member Organizations by Country") +
        labs(x="Country", y="Number of Member Organizations") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1))
      
    }
    
    else if (input$highlight == 18) {
      ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
        geom_bar (stat="count", show.legend = FALSE) + 
        scale_fill_manual(values=c(rep("dark grey", 16), "#267DC1", rep("dark grey",2))) +
        labs(title="WFTO Asia Member Organizations by Country") +
        labs(x="Country", y="Number of Member Organizations") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1))
      
    }
    
    else if (input$highlight == 19) {
      ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
        geom_bar (stat="count", show.legend = FALSE) + 
        scale_fill_manual(values=c(rep("dark grey", 17), "#267DC1", rep("dark grey",1))) +
        labs(title="WFTO Asia Member Organizations by Country") +
        labs(x="Country", y="Number of Member Organizations") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1))
    }
    
    else if (input$highlight == 20) {
      ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
        geom_bar (stat="count", show.legend = FALSE) + 
        scale_fill_manual(values=c(rep("dark grey", 18), "#267DC1")) +
        labs(title="WFTO Asia Member Organizations by Country") +
        labs(x="Country", y="Number of Member Organizations") +
        theme(text = element_text(size=20),
              axis.text.x = element_text(angle=45, hjust=1))
      
    }
    
    
  })
  
})
