# This script produces the shiny app that will be used to display HIRMEOS metrics

##---- Set up ----

# Load packages
library(shiny)
library(tidyverse)
library(rcrossref)
library(plotly)
library(shinyWidgets)
library(countrycode)
library(scales)
library(zoo)
library(lubridate)

options(stringsAsFactors = FALSE)


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
                                    destination = "country.name")) %>%
  mutate(measure = case_when(measure_id == "https://metrics.operas-eu.org/classics-library/sessions/v1" ~ "sessions",
                             measure_id == "https://metrics.operas-eu.org/google-books/views/v1" ~ "views",
                             measure_id == "https://metrics.operas-eu.org/oapen/downloads/v1" ~ "downloads",
                             measure_id == "https://metrics.operas-eu.org/obp-html/sessions/v1" ~ "sessions",
                             measure_id == "https://metrics.operas-eu.org/obp-pdf/sessions/v1"  ~ "sessions", 
                             measure_id == "https://metrics.operas-eu.org/obp/downloads/v1" ~ "downloads",
                             measure_id == "https://metrics.operas-eu.org/open-edition/downloads/v1" ~ "downloads",
                             measure_id == "https://metrics.operas-eu.org/world-reader/users/v1" ~ "users"))
# Have some data pre-processed and ready to go here, so that the entire datafile doesn't need uploading
# each time someone uses the app. 
titles <- all_data %>%
  filter(!is.na(title)) %>%
  pull(title) %>%
  unique() %>% 
  sort()

measures <- all_data %>%
  filter(!is.na(measure)) %>%
  pull(measure) %>%
  unique()


# 

##---- The User Interface ----------------------------------------------------------------------------
ui <- fluidPage(theme = "journal.css",
                
                # Application title
                titlePanel("Readership metrics"),
                
                # re-arrange the elements of the dashboard
                
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
                
                tableOutput("metrics_table"), #table of access by different metrics
                
                pickerInput(inputId = "metric", 
                            label = "Choose a metric or select all", 
                            selected = measures,
                            choices = measures,
                            options = list(`actions-box` = TRUE),
                            multiple = T),
                plotOutput("monthly_access"),
                
                
                #global dot map
                #top 10 countries bar chart
                #views per month for selected title, split by platform/metric 
                
                
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
  
  output$metrics_table <- renderTable({
    this_data <- all_data %>%
      filter(title %in% input$title, !is.na(value.x)) %>%   
      group_by(measure)%>%
      summarise(values = as.integer(sum(value.x))) 
    return(this_data)
    
  })
  
  output$monthly_access <- renderPlot({
    this_data <- all_data %>%
      filter(title %in% input$title) %>% 
      filter(measure %in% input$metric) %>% 
      mutate(yq = as.yearqtr(timestamp.x)) %>% 
      group_by(measure, yq)%>%
      summarise(values = sum(value.x)) %>%
      arrange(yq)
    
    p <- ggplot(this_data, mapping = aes(x = yq, y = values))
    p <- p + geom_col(aes(fill = measure))
    p <- p + scale_y_continuous(labels = comma)
    p <- p + ggtitle("Readership over time")
    p <- p + ylab("Interactions")
    p <- p + xlab("")
    p <- p + scale_x_yearqtr(format = "%Y-Q%q")
    
    return(p) #consider return(ggplotly(p)) for interactivity, if we can get it to work
    # might need an API to publish plotly charts
  })
  
  # Map of the world
  
  # Top country bar chart
  
  # Line chart by platform
  
  
}



##---- The final command ----
shinyApp(ui = ui, server = server)
