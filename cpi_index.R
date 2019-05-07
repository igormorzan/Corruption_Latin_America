# loads the necessary libraries

library(jsonlite)
library(tidyverse)
library(ggplot2)
library(readr)
library(ggthemes)

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
    
    corruption_data <- read.csv(url(path_to_file))
    # prints out tabular data
    
    print(corruption_data)
  }
}

knitr::opts_chunk$set(echo = TRUE)


# Creates the CPI graph for 2000

cpi_2000 <- corruption_data %>%
  # filters for Latin American countries
  
  filter(Jurisdiction %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                             "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela")) %>%
  # sets the X axis to the countries and y axis to data
  
  ggplot(aes(x = Jurisdiction, y = as.numeric(X2000))) +
  geom_col() +
  # sets the limits on the y axis for the bar plot
  
  scale_y_continuous(limits = c(0, 100)) +
  # appropriate titles and lables for the graph
  
  labs(title = "Latin American Countries' Corruption Perceptions Index", 
       subtitle = "Measures the degree that corruption is percieved, with higher values representing less corruption",
       caption = "Source: Transparency International") +
  xlab(NULL) +
  ylab("Corruption Perceptions Index") +
  theme_economist()

# creates the rds file for each individual graph that is called in the shiny app

write_rds(cpi_2000, "~/Desktop/Corruption_Latin_America/App/cpi_2000.rds")


# Creates the CPI graph for 2001

cpi_2001 <- corruption_data %>%
  # filters for Latin American countries
  
  filter(Jurisdiction %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                             "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela")) %>%
  # sets the X axis to the countries and y axis to data
  
  ggplot(aes(x = Jurisdiction, y = as.numeric(X2001))) +
  geom_col() +
  # sets the limits on the y axis for the bar plot
  
  scale_y_continuous(limits = c(0, 100)) +
  # appropriate titles and lables for the graph
  
  labs(title = "Latin American Countries' Corruption Perceptions Index", 
       subtitle = "Measures the degree that corruption is percieved, with higher values representing less corruption",
       caption = "Source: Transparency International") +
  xlab(NULL) +
  ylab("Corruption Perceptions Index") +
  theme_economist()

# creates the rds file for each individual graph that is called in the shiny app

write_rds(cpi_2001, "~/Desktop/Corruption_Latin_America/App/cpi_2001.rds")


# Creates the CPI graph for 2002

cpi_2002 <- corruption_data %>%
  # filters for Latin American countries
  
  filter(Jurisdiction %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                             "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela")) %>%
  # sets the X axis to the countries and y axis to data
  
  ggplot(aes(x = Jurisdiction, y = as.numeric(X2002))) +
  geom_col() +
  # sets the limits on the y axis for the bar plot
  
  scale_y_continuous(limits = c(0, 100)) +
  # appropriate titles and lables for the graph
  
  labs(title = "Latin American Countries' Corruption Perceptions Index", 
       subtitle = "Measures the degree that corruption is percieved, with higher values representing less corruption",
       caption = "Source: Transparency International") +
  xlab(NULL) +
  ylab("Corruption Perceptions Index") +
  theme_economist()

# creates the rds file for each individual graph that is called in the shiny app

write_rds(cpi_2002, "~/Desktop/Corruption_Latin_America/App/cpi_2002.rds")


# Creates the CPI graph for 2003

cpi_2003 <- corruption_data %>%
  # filters for Latin American countries
  
  filter(Jurisdiction %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                             "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela")) %>%
  # sets the X axis to the countries and y axis to data
  
  ggplot(aes(x = Jurisdiction, y = as.numeric(X2003))) +
  geom_col() +
  # sets the limits on the y axis for the bar plot
  
  scale_y_continuous(limits = c(0, 100)) +
  # appropriate titles and lables for the graph
  
  labs(title = "Latin American Countries' Corruption Perceptions Index", 
       subtitle = "Measures the degree that corruption is percieved, with higher values representing less corruption",
       caption = "Source: Transparency International") +
  xlab(NULL) +
  ylab("Corruption Perceptions Index") +
  theme_economist()

# creates the rds file for each individual graph that is called in the shiny app

write_rds(cpi_2003, "~/Desktop/Corruption_Latin_America/App/cpi_2003.rds")


# Creates the CPI graph for 2004

cpi_2004 <- corruption_data %>%
  # filters for Latin American countries
  
  filter(Jurisdiction %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                             "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela")) %>%
  # sets the X axis to the countries and y axis to data
  
  ggplot(aes(x = Jurisdiction, y = as.numeric(X2004))) +
  geom_col() +
  # sets the limits on the y axis for the bar plot
  
  scale_y_continuous(limits = c(0, 100)) +
  # appropriate titles and lables for the graph
  
  labs(title = "Latin American Countries' Corruption Perceptions Index", 
       subtitle = "Measures the degree that corruption is percieved, with higher values representing less corruption",
       caption = "Source: Transparency International") +
  xlab(NULL) +
  ylab("Corruption Perceptions Index") +
  theme_economist()

# creates the rds file for each individual graph that is called in the shiny app

write_rds(cpi_2004, "~/Desktop/Corruption_Latin_America/App/cpi_2004.rds")

# Creates the CPI graph for 2005

cpi_2005 <- corruption_data %>%
  # filters for Latin American countries
  
  filter(Jurisdiction %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                             "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela")) %>%
  # sets the X axis to the countries and y axis to data
  
  ggplot(aes(x = Jurisdiction, y = as.numeric(X2005))) +
  geom_col() +
  # sets the limits on the y axis for the bar plot
  
  scale_y_continuous(limits = c(0, 100)) +
  # appropriate titles and lables for the graph
  
  labs(title = "Latin American Countries' Corruption Perceptions Index", 
       subtitle = "Measures the degree that corruption is percieved, with higher values representing less corruption",
       caption = "Source: Transparency International") +
  xlab(NULL) +
  ylab("Corruption Perceptions Index") +
  theme_economist()

# creates the rds file for each individual graph that is called in the shiny app

write_rds(cpi_2005, "~/Desktop/Corruption_Latin_America/App/cpi_2005.rds")

# Creates the CPI graph for 2006

cpi_2006 <- corruption_data %>%
  # filters for Latin American countries
  
  filter(Jurisdiction %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                             "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela")) %>%
  # sets the X axis to the countries and y axis to data
  
  ggplot(aes(x = Jurisdiction, y = as.numeric(X2006))) +
  geom_col() +
  # sets the limits on the y axis for the bar plot
  
  scale_y_continuous(limits = c(0, 100)) +
  # appropriate titles and lables for the graph
  
  labs(title = "Latin American Countries' Corruption Perceptions Index", 
       subtitle = "Measures the degree that corruption is percieved, with higher values representing less corruption",
       caption = "Source: Transparency International") +
  xlab(NULL) +
  ylab("Corruption Perceptions Index") +
  theme_economist()

# creates the rds file for each individual graph that is called in the shiny app

write_rds(cpi_2006, "~/Desktop/Corruption_Latin_America/App/cpi_2006.rds")


# Creates the CPI graph for 2007

cpi_2007 <- corruption_data %>%
  # filters for Latin American countries
  
  filter(Jurisdiction %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                             "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela")) %>%
  # sets the X axis to the countries and y axis to data
  
  ggplot(aes(x = Jurisdiction, y = as.numeric(X2007))) +
  geom_col() +
  # sets the limits on the y axis for the bar plot
  
  scale_y_continuous(limits = c(0, 100)) +
  # appropriate titles and lables for the graph
  
  labs(title = "Latin American Countries' Corruption Perceptions Index", 
       subtitle = "Measures the degree that corruption is percieved, with higher values representing less corruption",
       caption = "Source: Transparency International") +
  xlab(NULL) +
  ylab("Corruption Perceptions Index") +
  theme_economist()

# creates the rds file for each individual graph that is called in the shiny app

write_rds(cpi_2007, "~/Desktop/Corruption_Latin_America/App/cpi_2007.rds")


# Creates the CPI graph for 2008

cpi_2008 <- corruption_data %>%
  # filters for Latin American countries
  
  filter(Jurisdiction %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                             "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela")) %>%
  # sets the X axis to the countries and y axis to data
  
  ggplot(aes(x = Jurisdiction, y = as.numeric(X2008))) +
  geom_col() +
  # sets the limits on the y axis for the bar plot
  
  scale_y_continuous(limits = c(0, 100)) +
  # appropriate titles and lables for the graph
  
  labs(title = "Latin American Countries' Corruption Perceptions Index", 
       subtitle = "Measures the degree that corruption is percieved, with higher values representing less corruption",
       caption = "Source: Transparency International") +
  xlab(NULL) +
  ylab("Corruption Perceptions Index") +
  theme_economist()

# creates the rds file for each individual graph that is called in the shiny app

write_rds(cpi_2008, "~/Desktop/Corruption_Latin_America/App/cpi_2008.rds")


# Creates the CPI graph for 2009

cpi_2009 <- corruption_data %>%
  # filters for Latin American countries
  
  filter(Jurisdiction %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                             "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela")) %>%
  # sets the X axis to the countries and y axis to data
  
  ggplot(aes(x = Jurisdiction, y = as.numeric(X2009))) +
  geom_col() +
  # sets the limits on the y axis for the bar plot
  
  scale_y_continuous(limits = c(0, 100)) +
  # appropriate titles and lables for the graph
  
  labs(title = "Latin American Countries' Corruption Perceptions Index", 
       subtitle = "Measures the degree that corruption is percieved, with higher values representing less corruption",
       caption = "Source: Transparency International") +
  xlab(NULL) +
  ylab("Corruption Perceptions Index") +
  theme_economist()

# creates the rds file for each individual graph that is called in the shiny app

write_rds(cpi_2009, "~/Desktop/Corruption_Latin_America/App/cpi_2009.rds")


# Creates the CPI graph for 2010

cpi_2010 <- corruption_data %>%
  # filters for Latin American countries
  
  filter(Jurisdiction %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                             "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela")) %>%
  # sets the X axis to the countries and y axis to data
  
  ggplot(aes(x = Jurisdiction, y = as.numeric(X2010))) +
  geom_col() +
  # sets the limits on the y axis for the bar plot
  
  scale_y_continuous(limits = c(0, 100)) +
  # appropriate titles and lables for the graph
  
  labs(title = "Latin American Countries' Corruption Perceptions Index", 
       subtitle = "Measures the degree that corruption is percieved, with higher values representing less corruption",
       caption = "Source: Transparency International") +
  xlab(NULL) +
  ylab("Corruption Perceptions Index") +
  theme_economist()

# creates the rds file for each individual graph that is called in the shiny app

write_rds(cpi_2010, "~/Desktop/Corruption_Latin_America/App/cpi_2010.rds")


# Creates the CPI graph for 2011

cpi_2011 <- corruption_data %>%
  # filters for Latin American countries
  
  filter(Jurisdiction %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                             "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela")) %>%
  # sets the X axis to the countries and y axis to data
  
  ggplot(aes(x = Jurisdiction, y = as.numeric(X2011))) +
  geom_col() +
  # sets the limits on the y axis for the bar plot
  
  scale_y_continuous(limits = c(0, 100)) +
  # appropriate titles and lables for the graph
  
  labs(title = "Latin American Countries' Corruption Perceptions Index", 
       subtitle = "Measures the degree that corruption is percieved, with higher values representing less corruption",
       caption = "Source: Transparency International") +
  xlab(NULL) +
  ylab("Corruption Perceptions Index") +
  theme_economist()

# creates the rds file for each individual graph that is called in the shiny app

write_rds(cpi_2011, "~/Desktop/Corruption_Latin_America/App/cpi_2011.rds")


# Creates the CPI graph for 2012

cpi_2012 <- corruption_data %>%
  # filters for Latin American countries
  
  filter(Jurisdiction %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                             "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela")) %>%
  # sets the X axis to the countries and y axis to data
  
  ggplot(aes(x = Jurisdiction, y = as.numeric(X2012))) +
  geom_col() +
  # sets the limits on the y axis for the bar plot
  
  scale_y_continuous(limits = c(0, 100)) +
  # appropriate titles and lables for the graph
  
  labs(title = "Latin American Countries' Corruption Perceptions Index", 
       subtitle = "Measures the degree that corruption is percieved, with higher values representing less corruption",
       caption = "Source: Transparency International") +
  xlab(NULL) +
  ylab("Corruption Perceptions Index") +
  theme_economist()

# creates the rds file for each individual graph that is called in the shiny app

write_rds(cpi_2012, "~/Desktop/Corruption_Latin_America/App/cpi_2012.rds")


# Creates the CPI graph for 2013

cpi_2013 <- corruption_data %>%
  # filters for Latin American countries
  
  filter(Jurisdiction %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                             "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela")) %>%
  # sets the X axis to the countries and y axis to data
  
  ggplot(aes(x = Jurisdiction, y = as.numeric(X2013))) +
  geom_col() +
  # sets the limits on the y axis for the bar plot
  
  scale_y_continuous(limits = c(0, 100)) +
  # appropriate titles and lables for the graph
  
  labs(title = "Latin American Countries' Corruption Perceptions Index", 
       subtitle = "Measures the degree that corruption is percieved, with higher values representing less corruption",
       caption = "Source: Transparency International") +
  xlab(NULL) +
  ylab("Corruption Perceptions Index") +
  theme_economist()

# creates the rds file for each individual graph that is called in the shiny app

write_rds(cpi_2013, "~/Desktop/Corruption_Latin_America/App/cpi_2013.rds")


# Creates the CPI graph for 2014

cpi_2014 <- corruption_data %>%
  # filters for Latin American countries
  
  filter(Jurisdiction %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                             "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela")) %>%
  # sets the X axis to the countries and y axis to data
  
  ggplot(aes(x = Jurisdiction, y = as.numeric(X2014))) +
  geom_col() +
  # sets the limits on the y axis for the bar plot
  
  scale_y_continuous(limits = c(0, 100)) +
  # appropriate titles and lables for the graph
  
  labs(title = "Latin American Countries' Corruption Perceptions Index", 
       subtitle = "Measures the degree that corruption is percieved, with higher values representing less corruption",
       caption = "Source: Transparency International") +
  xlab(NULL) +
  ylab("Corruption Perceptions Index") +
  theme_economist()

# creates the rds file for each individual graph that is called in the shiny app

write_rds(cpi_2014, "~/Desktop/Corruption_Latin_America/App/cpi_2014.rds")


# Creates the CPI graph for 2015

cpi_2015 <- corruption_data %>%
  # filters for Latin American countries
  
  filter(Jurisdiction %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                             "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela")) %>%
  # sets the X axis to the countries and y axis to data
  
  ggplot(aes(x = Jurisdiction, y = as.numeric(X2015))) +
  geom_col() +
  # sets the limits on the y axis for the bar plot
  
  scale_y_continuous(limits = c(0, 100)) +
  # appropriate titles and lables for the graph
  
  labs(title = "Latin American Countries' Corruption Perceptions Index", 
       subtitle = "Measures the degree that corruption is percieved, with higher values representing less corruption",
       caption = "Source: Transparency International") +
  xlab(NULL) +
  ylab("Corruption Perceptions Index") +
  theme_economist()

# creates the rds file for each individual graph that is called in the shiny app

write_rds(cpi_2015, "~/Desktop/Corruption_Latin_America/App/cpi_2015.rds")