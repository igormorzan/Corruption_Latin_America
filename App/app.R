## GOV 1005 Spring Final Project

# Loads necessary libraries for creating shiny app

library(shiny)
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(markdown)
library(DT)
library(leaflet)
library(shinythemes)

# downloads in the json file containing the data from the website

json_file <- 'https://datahub.io/core/corruption-perceptions-index/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

# get lists of all resources within the dataset

print(json_data$resources$name)

# this code is sourced from https://datahub.io/core/corruption-perceptions-index#r in order to 
# integrate it into R and be able to read it

# for loop to iterate through all the values in the dataset

for(i in 1:length(json_data$resources$datahub$type)){
  if(json_data$resources$datahub$type[i]=='derived/csv'){
    path_to_file = json_data$resources$path[i]
    # creates the corruption_data dataset that is referenced to create the plot
    
    new_data <- read.csv(url(path_to_file))
    # prints out tabular data
  }
}

corruption_data <- new_data %>%
  # filters for Latin American countries
  filter(Jurisdiction %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                             "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela"))

corruption_data_2015 <- corruption_data %>%
  # filters for Latin American countries
  filter(Jurisdiction %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                             "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela")) %>%
  # sets the X axis to the countries and y axis to data from 2015
  ggplot(aes(x = Jurisdiction, y = X2015)) +
  geom_point() +
  labs(title = "Latin American Countries' Corruption Perceptions Index for 2015", 
       subtitle = "Measures the degree that corruption is percieved, with higher values representing less corruption",
       caption = "Source: Transparency International") +
  xlab(NULL) +
  ylab("Corruption Perceptions Index")

# Define UI for application that draws a histogram

ui <- fluidPage(theme = shinytheme("flatly"),
  navbarPage("Corruption in Latin America",
             tabPanel("Overview",
                      tags$h3("A Visualization of Corruption in Latin America"),
                      tags$p("The map below places markers on each country in Latin America, and these markers indicate their rank relative to other countries in the index.
                             A country's score indicates the perceived level of public sector corruption on a scale of 0 (very corrupt) to 100 (very clean)."),
                      leafletOutput("latin_america", height = "600")),
             tabPanel("CPI Graph",
                      plotOutput("plot")
             ),
             tabPanel("Table",
                      DT::dataTableOutput("table")
                        ),
             tabPanel("About",
                    tags$h3("Data Sources"),             
                    tags$p("The data has been drawn from a collection of sources: Transparency International and the Latinobarometro Database. Through the ",
                                                       tags$a("Transparency International Dataset", href = "https://www.transparency.org/files/content/pages/2018_CPI_FullResults.zip"), "I was able to look
                                                       at each individuals' countries levels of percieved public sector corruption."))

  )
)


# Define server logic required to draw the graphs

server <- function(input, output) 
{
  output$plot <- renderPlot({
    plot(corruption_data_2015)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(corruption_data)
  })
  
  output$latin_america <- renderLeaflet({
    
    argentina_popup <- paste(sep = "<br/>",
                             "<b><a href='https://www.transparency.org/country/ARG'>Argentina</a></b>",
                             "Score: 40/100",
                             "Rank: 85/180"
    )
    
    bolivia_popup <- paste(sep = "<br/>",
                             "<b><a href='https://www.transparency.org/country/BOL'>Bolivia</a></b>",
                             "Score: 29/100",
                             "Rank: 132/180"
    )
    
    brazil_popup <- paste(sep = "<br/>",
                           "<b><a href='https://www.transparency.org/country/BRA'>Brazil</a></b>",
                           "Score: 35/100",
                           "Rank: 105/180"
    )

    chile_popup <- paste(sep = "<br/>",
                          "<b><a href='https://www.transparency.org/country/CHL'>Chile</a></b>",
                          "Score: 67/100",
                          "Rank: 27/180"
    )

    colombia_popup <- paste(sep = "<br/>",
                         "<b><a href='https://www.transparency.org/country/COL'>Colombia</a></b>",
                         "Score: 36/100",
                         "Rank: 99/180"
    )

    ecuador_popup <- paste(sep = "<br/>",
                            "<b><a href='https://www.transparency.org/country/ECU'>Ecuador</a></b>",
                            "Score: 34/100",
                            "Rank: 114/180"
    )

    paraguay_popup <- paste(sep = "<br/>",
                        "<b><a href='https://www.transparency.org/country/PRY'>Paraguay</a></b>",
                        "Score: 29/100",
                        "Rank: 132/180"
    )

    peru_popup <- paste(sep = "<br/>",
                     "<b><a href='https://www.transparency.org/country/PER'>Peru</a></b>",
                     "Score: 35/100",
                     "Rank: 105/180"
    )

    uruguay_popup <- paste(sep = "<br/>",
                        "<b><a href='https://www.transparency.org/country/URY'>Uruguay</a></b>",
                        "Score: 70/100",
                        "Rank: 23/180"
    )

    venezuela_popup <- paste(sep = "<br/>",
                           "<b><a href='https://www.transparency.org/country/VEN'>Venezuela</a></b>",
                           "Score: 18/100",
                           "Rank: 168/180"
    )
    
    leaflet() %>%
      addTiles() %>%
      
      # Setting an appropriate zoom on the graphs
      
      setView(lng = -63.549, lat = -16.28, zoom = 3.5) %>%
      
      # Adding a marker to the location I want
      
      addCircleMarkers(lng = -58.381592, lat = -34.603722, popup = argentina_popup) %>%
      addCircleMarkers(lng = -63.5887, lat = -16.2902, popup = bolivia_popup) %>%
      addCircleMarkers(lng = -51.9253, lat = -14.2350, popup = brazil_popup) %>%
      addCircleMarkers(lng = -71.5430, lat = -35.6751, popup = chile_popup) %>%
      addCircleMarkers(lng = -74.2973, lat = 4.5709, popup = colombia_popup) %>%
      addCircleMarkers(lng = -78.1834, lat = -1.8312, popup = ecuador_popup) %>%
      addCircleMarkers(lng = -58.4438, lat = -23.4425, popup = paraguay_popup) %>%
      addCircleMarkers(lng = -77.042793, lat = -12.046374, popup = peru_popup) %>%
      addCircleMarkers(lng = -55.7658, lat = -32.5228, popup = uruguay_popup) %>%
      addCircleMarkers(lng = -66.5897, lat = 6.4238, popup = venezuela_popup)
  })
}

# Run the application 

shinyApp(ui = ui, server = server)