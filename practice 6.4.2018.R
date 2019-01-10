
by_state <- zipcode %>% 
  group_by(state)

n1<- mtcars %>% 
  group_by(cyl) %>% 
  summarise(mean = mean(disp), n = n())

n2 <- zipcode %>% 
  group_by(state) %>% 
  summarise(n = n())

#number of membership organizations by country

ggplot (df, aes(df$CountryName)) +
  geom_histogram (stat="count") + 
  labs(title="Member Organizations by Country") +
  labs(x="Country", y="Number of Member Organizations") +
  

#let's make the histogram with ONE bin or bar colored/highlighted
  
x <- c(1, "red")
ggplot (df, aes(df$CountryName)) +
  geom_histogram (stat="count", fill= x ) + 
  labs(title="Member Organizations by Country") +
  labs(x="Country", y="Number of Member Organizations")

#number of membership organizations by country and membership type
#organization types https://wfto.com/members-and-products/who-can-apply 
ggplot (df, aes(df$CountryName, fill = df$`Membership Type`)) +
  geom_histogram (stat="count") + 
  labs(title="Member Organizations by Country") +
  labs(x="Country", y="Number of Member Organizations") 

#different bin colors
#https://stackoverflow.com/questions/10016316/how-to-manually-fill-colors-in-a-ggplot2-histogram
colors <- c(rep("red",7), rep("blue",4), rep("orange",3))

ggplot (df, aes(df$CountryName)) +
  geom_histogram (stat="count") + 
  labs(title="Member Organizations by Country") +
  labs(x="Country", y="Number of Member Organizations") 


ggplot(df, aes(df$CountryName, fill = df$CountryName)) +
  geom_histogram (stat="count", show.legend = FALSE) + 
  labs(title="Member Organizations by Country") +
  labs(x="Country", y="Number of Member Organizations") +
  theme(text = element_text(size=20),
          axis.text.x = element_text(angle=45, hjust=1))


#data grouped by country
by.country <- df %>% group_by(CountryName) %>% 
summarise(n = n())

#creating dataframe by.org
by.org <- select(df, `Organisation Name`= "Organisation Name", CountryName = "CountryName", 
            `Membership Type`= "Membership Type") 

#groupby countries ....

by.org %>% 
  group_by(CountryName) %>% 
  summarise(list(`Organisation Name`))


#types of WFTO organizations by membership type pie chart
library(scales)
library(ggthemes)
library(ggplot2)
library (ggrepel)


by.memtype <- df %>% group_by(`Membership Type`) %>% 
  summarise(n = n())

bp <- ggplot(by.memtype, aes(x="", y= n, fill = by.memtype$`Membership Type`))+
  geom_bar(width = 1, stat = "identity") +
labs(title="WFTO-Asia Member Organizations by Membership Type") +
  labs(x="", y="", fill = "MembershipType") 

pie <- bp + coord_polar("y", start=0) 

pie + theme(text= element_text(size=12), axis.text.x=element_blank())+
  geom_text_repel(aes(y = n/3+ c(0, cumsum(n)[-length(n)]), 
                      label = percent(n/153)), 
                      nudge_x = 2, 
                      segment.size = .3) 

#another way to make a pie chart, which way is best? how to change size of %s

library(scales)
library(ggthemes)
library(ggplot2)
library (ggrepel)


by.memtype <- df %>% group_by(`Membership Type`) %>% 
  summarise(n = n())

bp <- ggplot(by.memtype, aes(x="", y= n, fill = by.memtype$`Membership Type`))+
  geom_bar(width = 1, stat = "identity") +
  labs(title="WFTO-Asia Member Organizations by Membership Type") +
  labs(x="", y="", fill = "MembershipType") 

pie <- bp + coord_polar("y", start=0) 

pie + theme(text= element_text(size=12), axis.text.x=element_blank())+
  geom_text(aes(y = n/3 + c(0, cumsum(n)[-length(n)]), 
                label = percent(n/153)), size=5)

midpoint <- cumsum(by.memtype$n) - by.memtype$n/2
pie <- pie + 
  scale_y_continuous (breaks=midpoint, labels = percent (by.memtype$n/153))
                                 
pie 


#from server.R

library(shiny)

shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    ggplot (df, aes(df$CountryName)) +
      geom_histogram (stat="count") + 
      labs(title="Member Organizations by Country") +
      labs(x="Country", y="Number of Member Organizations") +
      theme(text = element_text(size=20),
            axis.text.x = element_text(angle=45, hjust=1))
  })
})



#trying to use hist()

barplot(df, aes(df$CountryName),
     geom_histogram (stat="count"), 
     main = "Member Organizations by Country",
     xlab = "Country",
     ylab = "Number of Member Organizations",
     theme(text = element_text(size=20),
      axis.text.x = element_text(angle=45, hjust=1))
)

#trying to create a reactive shinyApp

output$distPlot <- renderPlot({
  df = df
  if(input$country == 1) {
    data = df$CountryName
    title = "Member Countries"
    xlab = "Countries"
  }
  else if(input$country == 2) {
    data = df$`Membership Type`
    title = "Membership Type"
    xlab = "Countries"
  }
  breaks = seq(min(df), max(data), length.out = input$bins + 1)
  hist(data, breaks = breaks, col = "blue", main = title, xlab = xlab, ylab = "count")
})


#server dataTable 

output$selected_country <- renderDataTable ({
  
  if(input$country == 1) {
    df %>% 
      filter(CountryName == "Cambodia") %>% 
      select (`Organisation Name`)
  }
  else if (input$country == 2) {
    df %>% 
      filter(CountryName == "Laos") %>% 
      select (`Organisation Name`)
  }
  else if (input$country == 3) {
    df %>% 
      filter(CountryName == "Thailand") %>% 
      select (`Organisation Name`)
  }
  
  else if (input$country == 4) {
    df %>% 
      filter(CountryName == "Myanmar") %>% 
      select(`Organisation Name`)
  }
  
  else if (input$country == 5) {
    df %>% 
      filter(CountryName == "Vietnam") %>% 
      select (`Organisation Name`)
  }
})

#ui practice 


choices = c ("Cambodia" = 1, 
             "Laos" = 2, 
             "Thailand" = 3, 
             "Myanmar"= 4, 
             "Vietnam"= 5) #,
selected = "Thailand" #)


#reactivity how does this WORKKKKK please someone tell meeeee

idx = which(df$mag >= input$magnitude[1] 
      & df$mag <= input$magnitude[2] 
      & df$depth >= input$depth[1] 
      & df$depth <= input$depth[2])


#another way to ggplot

ggplot(data=df, aes(df$CountryName)) + 
  geom_histogram (stat= "count")

#embedd shinyapp into a webpage using #iframe
#http://docs.rstudio.com/shinyapps.io/applications.html 

# let's try to create an animated graph.../ didn't work something about devtools...
install.packages("cowplot")
devtools::install_github("dgrtwo/gganimate")
library(ggplot2)
library(gganimate)
library(gapminder)

#to update R packages 
update.packages(ask = FALSE)


# bar graph of countries by membership type

ggplot(wfto, aes(wfto$CountryName, fill = wfto$Membership.Type)) + 
  scale_fill_discrete(name="Membership Type", breaks=c("FTN", "FTO", "FTSO"), 
          labels=c("Fair Trade Network", "Fair Trade Organization", "Fair Trade Support Organization")) +
  geom_histogram (stat="count", show.legend = TRUE) + 
  labs(title="WFTO Asia Member Organizations by Membership Type") +
  labs(x="Country", y="Number of Member Organizations") +
  theme(text = element_text(size=20), 
        axis.text.x = element_text(angle=45, hjust=1))

# bar graph of countries by membership status

ggplot(wfto, aes(wfto$CountryName, fill = wfto$Guaranteed.)) + 
  scale_fill_discrete(name="Membership Status", labels=c("Provisional", "Guaranteed")) +
  geom_histogram (stat="count", show.legend = TRUE) + 
  labs(title="WFTO Asia Member Organizations by Membership Status") +
  labs(x="Country", y="Number of Member Organizations") +
  theme(text = element_text(size=20), 
        axis.text.x = element_text(angle=45, hjust=1))

# making each graph reactive reactive 

output$distPlot<- renderPlot({
  
  if(input$graphtype == 1) {
    ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
      geom_histogram (stat="count", show.legend = FALSE) + 
      labs(title="WFTO Asia Member Organizations by Country") +
      labs(x="Country", y="Number of Member Organizations") +
      theme(text = element_text(size=20),
            axis.text.x = element_text(angle=45, hjust=1))
  }
  else if(input$graphtype == 2) {
    ggplot(wfto, aes(wfto$CountryName, fill = wfto$Membership.Type)) + 
      scale_fill_discrete(name="Membership Type", breaks=c("FTN", "FTO", "FTSO"), 
                          labels=c("Fair Trade Network", "Fair Trade Organization", "Fair Trade Support Organization")) +
      geom_histogram (stat="count", show.legend = TRUE) + 
      labs(title="WFTO Asia Member Organizations by Membership Type") +
      labs(x="Country", y="Number of Member Organizations") +
      theme(text = element_text(size=20), 
            axis.text.x = element_text(angle=45, hjust=1))
  }
  ggplot(wfto, aes(wfto$CountryName, fill = wfto$Guaranteed.)) + 
    scale_fill_discrete(name="Membership Status", labels=c("Provisional", "Guaranteed")) +
    geom_histogram (stat="count", show.legend = TRUE) + 
    labs(title="WFTO Asia Member Organizations by Membership Status") +
    labs(x="Country", y="Number of Member Organizations") +
    theme(text = element_text(size=20), 
          axis.text.x = element_text(angle=45, hjust=1))
    
  }
)

# MAPPPPINGGGGG
# https://www.jessesadler.com/post/geocoding-with-r/

places <- wfto.map1 %>%  
  select(City)

places %>% 
  mutate_if(is.factor, as.character) -> places

location <- mutate_geocode(places, City)

location <- location %>% drop_na()

location_sf <- st_as_sf(location, coords = c("lon", "lat"), crs = 4326) #cannot have any NA's 
mapview(location_sf)

wfto_sf <- st_as_sf(wfto.map1, coords = c("Long", "Lat"), crs = 4326)
mapview(wfto_sf)



# creating different color bins when selected...

ggplot(wfto, aes(wfto$CountryName, color = "blue")) +
  geom_histogram (stat="count", show.legend = FALSE) + 
  labs(title="WFTO Asia Member Organizations by Country") +
  labs(x="Country", y="Number of Member Organizations") +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=45, hjust=1))


#Language stuff





#creating an add up function

added.up <- function(x)
  sum(x)

#creating tabs


mainPanel(
              
              plotOutput("distPlot"),
              br(), br(),
              tableOutput("selected_mekong")
  )

#let's graph with only one bar highlighted
# https://stackoverflow.com/questions/22894102/change-color-of-only-one-bar-in-ggplot 
# woooooow you can use the color codes #00000 to find the color you would like to input

ggplot(AA_Aug2018, aes(AA_Aug2018$CountryName, fill = AA_Aug2018$CountryName)) +
  geom_histogram (stat="count", show.legend = FALSE) + 
  labs(title="WFTO Asia Member Organizations by Country") +
  labs(x="Country", y="Number of Member Organizations") +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=45, hjust=1))

ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
  geom_bar (stat="count", show.legend = FALSE) + 
  scale_fill_manual(values=c(rep("dark grey", 8), "#267DC1", rep("dark grey",10))) +
  labs(title="WFTO Asia Member Organizations by Country") +
  labs(x="Country", y="Number of Member Organizations") +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=45, hjust=1)) 

ggplot(wfto, aes(wfto$CountryName, fill = wfto$CountryName)) +
  geom_bar (stat="count", show.legend = FALSE) + 
  scale_fill_manual(values=c(rep("#267DC1",19))) +
  labs(title="WFTO Asia Member Organizations by Country") +
  labs(x="Country", y="Number of Member Organizations") +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=45, hjust=1))


# how to add an image to Shiny App
img(src='myImage.png', align = "right") 

#MAP THAILAND 

# library ("ggmap")
geocode("thailand")
mapthailand <- get_googlemap(center = c(100.9925, 15.87003), zoom = 6)
ggmap(thailandmap)
ggmap(mapthailand)

# install.packages()
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("devtools")
install.packages("DT")
install.packages("RgoogleMaps")













