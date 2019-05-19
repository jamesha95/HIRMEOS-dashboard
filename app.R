# This script produces the shiny app that will be used to display HIRMEOS metrics

##---- Set up ----

# Load packages
library(shiny)
library(tidyverse)
library(rcrossref)
library(plotly)
library(shinyWidgets)
library(countrycode)


# Import local data as required
metrics_data <- read_csv("data/metrics.csv") %>% 
  as_tibble()

meta_data <- read_csv("data/metadata.csv") %>% 
  as_tibble() %>%
  mutate(work_uri = paste0("info:doi:", doi))

altmetrics_data <- read_csv("data/altmetrics.csv") %>% 
  as_tibble()

all_data <- metrics_data %>%
  left_join(meta_data, by = c("work_uri" = "work_uri")) %>% 
  left_join(altmetrics_data, by = c("work_uri" = "URI")) %>%
  filter(type %in% c("monograph", "book")) %>%  # for now, we will focus on book & monograph data
  select(measure_id, # this tells us what sort of metric we're looking at
         value.x, # the value of the metric   
         timestamp.x, # this tells us what date
         country_uri, # this tells us what country
         title, # this is the title
         publisher) %>%  # together, these are the data we'll analyse for now
# later we will include events (altmetrics)
  mutate(country_name = countrycode(substr(country_uri, start = 21, stop = 22), 
                                    origin = "iso2c", 
                                    destination = "country.name"))
# Have some data pre-processed and ready to go here, so that the entire datafile doesn't need uploading
# each time someone uses the app. 
titles <- all_data %>%
  filter(!is.na(title)) %>%
  pull(title) %>%
  unique()



# 

##---- The User Interface ----------------------------------------------------------------------------
ui <- fluidPage(theme = "journal.css",
                
                # Application title
                titlePanel("Readership metrics"),
                
                # a metric insensitive to title choice
                textOutput(outputId = "no_titles"), # consider using h2 and choosing a different font
                
                # things the user can interact with
                pickerInput(inputId = "title", 
                            label = "Choose a title or select all", 
                            selected = titles,
                            choices = titles,
                            options = list(`actions-box` = TRUE),
                            multiple = T),
                
                textOutput("total_access"),
                textOutput("no_countries_reached"),
                
                #plotOutput("metrics_table"), #table of access by different metrics
                plotOutput("monthly_access", height = "3600px"),
                
                
                #global dot map
                #top 10 countries bar chart
                #views per month for selected title, split by platform-metric 
                
                
                # some JS to recorc the window size
                tags$head(tags$script('
                        var width = 0;
                                      $(document).on("shiny:connected", function(e) {
                                      width = window.innerWidth;
                                      Shiny.onInputChange("width", width);
                                      });
                                      $(window).resize(function(e) {
                                      width = window.innerWidth;
                                      Shiny.onInputChange("width", width);
                                      });
                                      '))
                  
)


##---- The Server -------------------------------------------------------------------------------
server <- function(input, output) {
  output$no_titles <- renderText({
    no_titles <- length(titles)
    return(paste0("Titles published: ", no_titles))
  })
  
  output$total_access <- renderText({
    total_access <- all_data %>%
      filter(title %in% input$title, !is.na(value.x)) %>% 
      pull(value.x) %>% 
      sum()
    return(paste0("Total acccess: ", total_access))
  })
  
  output$no_countries_reached <- renderText({
    no_countries_reached <- all_data %>%
      filter(title %in% input$title, !is.na(value.x)) %>% 
      pull(country_name) %>% 
      unique() %>% 
      length
    return(paste0("Countries reached: ", no_countries_reached))
  })
  
  
  #output$monthly_access <- renderPlotly({
  #})
  
  #output$metrics_table <- renderTable()
  
  
}



##---- The final command ----
shinyApp(ui = ui, server = server)
