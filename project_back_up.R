#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Load libraries

library(shiny)
library(tidyverse)
library(jsonlite)
library(sf)    # spatial data
library(tigris) # geojoin
library(leaflet) #interactive maps
library(htmlwidgets) #interactive map labels
library(janitor) #clean names
library(tmap) #mapping package
options(scipen=999) #remove scientific notation 



#import data

nypd_shooting_historic <- fromJSON("https://data.cityofnewyork.us/resource/833y-fsy8.json?$limit=50000")

precinct_map<-read_sf("https://data.cityofnewyork.us/resource/kmub-vria.geojson")

nypd_shooting_ytd <- fromJSON("https://data.cityofnewyork.us/resource/5ucz-vwe8.json?$limit=5000")

#combining historical and year to date datasets
nypd_shooting <- bind_rows(nypd_shooting_historic,nypd_shooting_ytd)

#getting precinct names
precinct_names <-read.csv("https://raw.githubusercontent.com/angelhumano/our_first_project/5a1aee4448295461934988214c8f9344047b6899/NYZIP_precinct.csv")

precinct_names<- clean_names(precinct_names)
#merge nypd_shooting with precinct_names
precinct_names$precinct <- as.character(precinct_names$precinct)
nypd_shooting <- left_join(nypd_shooting , precinct_names, by="precinct")



#cleaning 

#change datatype
nypd_shooting$occur_date <-as.Date(nypd_shooting$occur_date)

#created a new column "year" from occur_date
nypd_shooting$year <- format(nypd_shooting$occur_date,format="%Y")

# created a new column "month" from occur_date
nypd_shooting$month <- format(nypd_shooting$occur_date,format="%m")

#created a new column for the "hour" of the day from occur_time
nypd_shooting$hour = str_sub(nypd_shooting$occur_time,1,2) 







#subset of data
shooting_data_subsect<-select(nypd_shooting, c(incident_key,occur_date,occur_time, boro, precinct,precinct_name,
                                               vic_age_group, vic_sex, vic_race,
                                               perp_age_group,perp_sex ,perp_race,
                                               year, month, hour))




# Interactive map shootings grouped by precinct
# Shootings grouped by precinct
shootings_precinct<-shooting_data_subsect %>%
  group_by(precinct,precinct_name) %>%
  count()

#rename n
shootings_precinct<- shootings_precinct%>% rename( number_of_shootings = n)


map<-precinct_map
#merge dataset
map_and_data<-inner_join(shootings_precinct,map)

#convert to st
map_and_data<-st_as_sf(map_and_data)



# EDA summary plots



#Number of shootings per borough

shoot_boro <- shooting_data_subsect%>%group_by(boro)%>%summarise(number_of_shootings= n())



#bar graph of shooting per Borough(trend) 

plot_shooting_boro <-ggplot(shoot_boro,
                            aes(x = reorder(boro, -number_of_shootings),
                                y = number_of_shootings))+
  geom_bar(stat = "identity",
           position = "dodge", fill="purple")+
  xlab("Number of shootings") +
  ylab("Borough")+  geom_text(aes(label = number_of_shootings), vjust = -0.2)


#Number of shooting per boro and precincts yearly

shoot_boroyear <- shooting_data_subsect%>%group_by(boro,year)%>%
  summarise(number_of_shootings=n())%>% rename( Borough = boro) %>%
  spread( key = year, value = number_of_shootings)



#bar graph of shooting per year 

#Number of shooting per year 
shoot_year <- shooting_data_subsect%>%group_by(year)%>%summarise(number_of_shootings=n())


shoot_year<- ggplot(shoot_year,                                      
                    aes(x = year,
                        y = number_of_shootings)) +
  geom_bar(stat = "identity",
           position = "dodge", fill="red") +
  ylab("Number of shootings") 


#Number of shooting per month(trend) 
shoot_month <- shooting_data_subsect%>%group_by(month)%>%summarise(number_of_shootings=n())

#bar graph of shooting per month(trend) 
shoot_month<-ggplot(shoot_month,                                      
                    aes(x = month,
                        y = number_of_shootings)) +
  geom_bar(stat = "identity",
           position = "dodge", fill="red")+
  ylab("Number of shootings") 



#Number of shooting per hours of the day

shoot_hour <- shooting_data_subsect%>%group_by(hour)%>%summarise(number_of_shootings=n())

shoot_hour<- ggplot(shoot_hour,                                      
                    aes(x = hour,
                        y = number_of_shootings)) +
  geom_bar(stat = "identity",
           position = "dodge", fill="purple")+
  ylab("Number of shootings") 


#Victims by year and gender

victims_gender_year <- shooting_data_subsect%>%group_by(year,vic_sex)%>%
  summarise(number_of_victims=n())

victims_gender_year<-ggplot(victims_gender_year,                                      
                            aes(x = year,
                                y = number_of_victims,
                                fill = vic_sex)) +
  geom_bar(stat = "identity",
           position = "dodge")+
  ylab("Number of victims") 




# Define UI for application



ui <- fluidPage(
  #Application title
  titlePanel("NYPD Shooting Incident Data (Historic)"),
  sidebarLayout(
    sidebarPanel(
      textOutput("ourdata"),
      tags$a(href ="https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Historic-/833y-fsy8", "NYPD Shooting Incident Data (Historic), ", target ="_blank"),
      tags$a(href ="https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Year-To-Date-/5ucz-vwe8", "NYPD Shooting Incident Data (Year To Date), ", target ="_blank"),
      tags$a(href ="https://data.cityofnewyork.us/Public-Safety/Police-Precincts/78dh-3ptz", "GIS data: Boundaries of Police Precincts", target ="_blank"),
      h5("Explore shooting incidents in NYC from  2006 to the most current data in the year-to-date dataset. The first tab provides an interactive map that shows the number of shootings by precincts. The historic and year-to-date datasets were merged with the boundaries of NYC police precincts to create the map. The second tab contains exploratory plots with key insights about the number of shootings by borough, gender of the victim, year, month, and hour. The third tab contains a time series of shooting over the years and a forecast. The last tab provides a dynamic table of the raw data of the merged historic and year-to-date shooting datasets.")
    ),
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel("Interactive map", tmapOutput("map")),
        tabPanel("Summary tables and plots", br(),textOutput("text"),br(),
                 plotOutput("shooting_boro", width = "400px"),br(),br(),
                 textOutput("text_boro_pre"),br(),
                 tableOutput("table_boro_pre"), br(),br(),
                 textOutput("text_shoot_year"),br(),
                 plotOutput("shoot_year", width = "800px"),br(),br(),
                 textOutput("text_shoot_month"),br(),
                 plotOutput("shoot_month", width = "800px"),br(),br(),
                 textOutput("text_shoot_hour"),br(),
                 plotOutput("shoot_hour", width = "800px"),br(),br(),
                 textOutput("text_victim"),br(),
                 plotOutput("victim", width = "800px"),br(),br(),
                 
                 
                 
        ),
        tabPanel("Time series analysis"),
        tabPanel("Dataset", dataTableOutput("dynamic"))
        
        
        
        
        
        
      )
    )
  )
)



# Define server logic
server <- function(input, output, session) {
  
  #function1
  output$ourdata <- renderText({
    paste("Datasets used:")
  })
  
  
  
  #Tab 1: function for interactive map
  output$map <- renderTmap({
    tm_shape(map_and_data)+
      tm_polygons(col="number_of_shootings", id ="precinct_name", title ="Number of shootings", n= 6)
  })
  
  
  #Tab 2: Summary plots functions
  
  
  output$text <- renderText({
    "Shootings incidents by borough: Brooklyn and the Bronx have the highest number of shooting incidents"
  })
  
  
  #shooting per boro bar chart
  
  output$shooting_boro <- renderPlot({plot_shooting_boro
  })
  
  
  output$text_boro_pre <- renderText({
    "Number of shootings by borough and precincts yearly"
  })
  
  
  # Number of shooting per precincts yearly 
  output$table_boro_pre <- renderTable(shoot_boroyear)
  
  #Number of shooting per year 
  output$text_shoot_year <- renderText({
    "Number of shootings by year"
  })
  
  output$shoot_year <- renderPlot({shoot_year
  })
  
  
  #Number of shooting per month
  output$text_shoot_month <- renderText({
    "Number of shootings aggregated by month"
  })
  
  output$shoot_month <- renderPlot({shoot_month
  })
  
  
  #Number of shooting per hours
  output$text_shoot_hour<- renderText({
    "Number of shootings aggregated by hours"
  })
  
  output$shoot_hour <- renderPlot({shoot_hour
  })
  
  #bar graph of Victims by year and gender 
  output$text_victim<- renderText({
    "Victims by year and gender"
  })
  
  output$victim <- renderPlot({victims_gender_year
  })
  
  
  
  
  
  
  #dataset
  
  output$dynamic <- renderDataTable(nypd_shooting, options = list(pageLength = 5))
  
  #test  side bar inside a panel
  output$text1 <- renderText({
    "under construction!"
  })
  
  
  
  
  
}



# tableOutput("table_boro"), br(),br(),

# output$table_boro <- renderTable(shoot_boro)


# Run the application 
shinyApp(ui = ui, server = server)

