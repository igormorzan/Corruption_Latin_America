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

cpi_2000 <- read_rds("./cpi_2000.rds")
cpi_2001 <- read_rds("./cpi_2001.rds")
cpi_2002 <- read_rds("./cpi_2002.rds")
cpi_2003 <- read_rds("./cpi_2003.rds")
cpi_2004 <- read_rds("./cpi_2004.rds")
cpi_2005 <- read_rds("./cpi_2005.rds")
cpi_2006 <- read_rds("./cpi_2006.rds")
cpi_2007 <- read_rds("./cpi_2007.rds")
cpi_2008 <- read_rds("./cpi_2008.rds")
cpi_2009 <- read_rds("./cpi_2009.rds")
cpi_2010 <- read_rds("./cpi_2010.rds")
cpi_2011 <- read_rds("./cpi_2011.rds")
cpi_2012<- read_rds("./cpi_2012.rds")
cpi_2013 <- read_rds("./cpi_2013.rds")
cpi_2014 <- read_rds("./cpi_2014.rds")
cpi_2015 <- read_rds("./cpi_2015.rds")

bar_institution <- read_rds("~/Desktop/Corruption_Latin_America/App/bar_institution.rds")
point_institution <- read_rds("~/Desktop/Corruption_Latin_America/App/point_institution.rds")

# Define UI for application that draws a histogram

ui <- fluidPage(theme = shinytheme("flatly"),
  navbarPage("Corruption in Latin America",
             tabPanel("Overview",
                      tags$h3("A Visualization of Corruption in Latin America"),
                      tags$p("The map below places markers on each country in Latin America, and these markers indicate their rank relative to other countries in the index.
                             A country's score indicates the perceived level of public sector corruption on a scale of 0 (very corrupt) to 100 (very clean)."),
                      leafletOutput("latin_america", height = "600"),
                      tags$h3("Defining Corruption and its Role"),
                      tags$p("Corruption is defined as the use of public goods for private benefit. In order to study corruption, it is important to identify the different types 
                             of corruption: clientelism, extortion, capture, bribery, etc. Different types of corruption are used in different situations, however the datasets used 
                             for this project look at perceived corruption among citizens, and this data is gathered through a survey. Corruption plagues much of Latin America, especially 
                             where politicians abuse their resources to retain power. Above all, corruption is difficult to quantify, and measuring corruption is not easy since it occurs in secret: 
                             costs are difficult to measure but they are definitely noticeable.")),
             tabPanel("CPI Graphs",
                      sidebarLayout
                      (
                        sidebarPanel
                        (
                          selectInput(inputId = "year",
                                      label = "Select a year",
                                      choices = c("2000", "2001", "2002", "2003", "2004", "2005",
                                                  "2006", "2007", "2008", "2009", "2010", "2011",
                                                  "2012", "2013", "2014", "2015"),
                                      multiple = FALSE,
                                      selected = "2000"),
                          radioButtons("plotType", "Select a plot type",
                                       c("Bar"="b", "Point"="p")
                          )
                        ),
                      mainPanel
                      (
                        plotOutput("cpi_graph"),
                        plotOutput("institution_corruption")
                      )
                    )),
             tabPanel("Table",
                      DT::dataTableOutput("table")
                     ),
             tabPanel("About",
                    tags$h3("Data Sources"),             
                    tags$p("The data has been drawn from a collection of sources: Transparency International and the Latinobarometro Database. Through the ",
                                                       tags$a("Transparency International Dataset", href = "https://www.transparency.org/files/content/pages/2018_CPI_FullResults.zip"), "I was able to look
                                                       at each individuals' countries levels of percieved public sector corruption. The", tags$a("Latinobarometro Database", href = "http://www.latinobarometro.org/latContents.jsp"),
                                                       "details the data collected from an annual public opinion survey involving roughly 20,000 interviews, representing more than 600 million people. The Latinobarometro
                                                       observes the development of democracies, economies and societies, using indicators of attitude, opinion and behavior."),
                    tags$h3("Project Details"),
                    tags$p("Growing up in Peru, the concept of corruption wasn't an unfamiliar aspect of my life. Bribes were especially prominent among police officers in my small town, and the lack of data
                           and attention to this problem inspired me to pursue this project. The datasets, although small, provide interesting insights to the levels of corruption in countries across Latin America."))
  )
)



# Define server logic required to draw the graphs

server <- function(input, output) 
{
  output$cpi_graph <- renderPlot({
    if(input$year == "2000") {
      cpi_2000
    }
    else if(input$year == "2001") {
      cpi_2001
    }
    else if(input$year == "2002") {
      cpi_2002
    }
    else if(input$year == "2003") {
      cpi_2003
    }
    else if(input$year == "2004") {
      cpi_2004
    }
    else if(input$year == "2005") {
      cpi_2005
    }
    else if(input$year == "2006") {
      cpi_2006
    }
    else if(input$year == "2007") {
      cpi_2007
    }
    else if(input$year == "2008") {
      cpi_2008
    }
    else if(input$year == "2009") {
      cpi_2009
    }
    else if(input$year == "2010") {
      cpi_2010
    }
    else if(input$year == "2011") {
      cpi_2011
    }
    else if(input$year == "2012") {
      cpi_2012
    }
    else if(input$year == "2013") {
      cpi_2013
    }
    else if(input$year == "2014") {
      cpi_2014
    }
    else if(input$year == "2015") {
      cpi_2015
    }
  })
  
  output$institution_corruption <- renderPlot({
    if(input$plotType == "b") {
      bar_institution
    }
    else if(input$plotType == "p") {
      point_institution
    }
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
      
      # Adding a marker to the capital of the Latin American country
      
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