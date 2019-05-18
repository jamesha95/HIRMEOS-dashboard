# This script produces the shiny app that will be used to display HIRMEOS metrics

##---- Set up ----

# Load packages
library(shiny)
library(tidyverse)
library(rcrossref)
library(plotly)


# Import local data as required
metrics_data <- read_csv("metrics.csv") %>% 
  as_tibble()

meta_data <- read_csv("metadata.csv") %>% 
  as_tibble() %>%
  mutate(work_uri = paste0("info:doi:", doi))

altmetrics_data <- read_csv("altmetrics.csv") %>% 
  as_tibble()

data <- metrics_data %>%
  left_join(meta_data, by = c("work_uri" = "work_uri")) %>% 
  left_join(altmetrics_data, by = c("work_uri" = "URI")) %>%
  filter(type == "monograph") # for now, we will focus on monograph data


# 

##---- The User Interface ----
ui <- fluidPage(theme = "journal.css",
                
                # Application title
                titlePanel("Readership metrics")
                
               
)


##---- The Server ----
server <- function(input, output) {}



##---- The final command ----
shinyApp(ui = ui, server = server)