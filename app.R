# This script produces the shiny app that will be used to display HIRMEOS metrics

#---- Set up ------------------------------------------------------------------------------

# Here's the packages that will be needed

list.of.packages <- c("ggplot2",
                      "shiny",
                      "tidyverse",
                      "rcrossref",
                      "plotly",
                      "shinyWidgets",
                      "countrycode",
                      "scales",
                      "zoo",
                      "lubridate",
                      "RColorBrewer",
                      "shinydashboard")

# This code checks for these packages and installs them if they need installing

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load packages

library(shiny)
library(shinydashboard)
library(tidyverse)
library(rcrossref)
#library(plotly)
library(shinyWidgets)
library(countrycode)
library(scales)
library(zoo)
library(lubridate)
library(RColorBrewer)

#We disable R's feature that automatically reads in strings as factors. In this work, strings are generally just strings

options(stringsAsFactors = FALSE)




# Import local data and tidy------------------------------------------------------------------------------

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
                             measure_id == "https://metrics.operas-eu.org/world-reader/users/v1" ~ "users")) %>% 
  mutate(yq = as.yearqtr(timestamp.x)) # We add year-quarters for the readership dates
# Have some data pre-processed and ready to go here, so that the entire datafile doesn't need uploading
# each time someone uses the app. 

titles <- all_data %>%
  filter(!is.na(title)) %>%
  pull(title) %>%
  unique() %>% 
  sort()

no_titles <- prettyNum(length(titles), big.mark = ",")

measures <- all_data %>%
  filter(!is.na(measure)) %>%
  pull(measure) %>%
  unique()





#---- The User Interface: Header and sidebar----------------------------------------------------------------------------

ui <- dashboardPage(skin = "black",
                    header = dashboardHeader(title = "Readership metrics"
                    ),
                    
                    sidebar = dashboardSidebar(
                      sidebarMenu(
                        # Setting id makes input$tabs give the tabName of currently-selected tab
                        id = "tabs",
                        menuItem("Summary", tabName = "Summary", icon = icon("dashboard")),
                        menuItem("Metrics by title", tabName = "Metrics_by_title", icon = icon("book")),
                        menuItem("Metrics by country", tabName = "Metrics_by_country", icon = icon("globe")),
                        
                        # We add a horizontal line, followed by the HIRMEOS logo
                        hr(),
                        
                        a(href = 'https://www.hirmeos.eu/',
                          img(src = 'HIRMEOS_LOGO_rect.png', width = "100%")
                        )
                      )
                    ),
                    
                    #---- The User Interface: Body----------------------------------------------------------------------------------                    
                    
                    body = dashboardBody(
                      
                      #Set a theme if desired - check if this works within the dashboardPage function
                      #theme = "journal.css",
                      
                      tabItems(
                        
                        #---- Tab 1: Summary----------------------------------------------------------------------------------                              
                        
                        tabItem(tabName = "Summary",
                                
                                fluidRow(
                                  column(6,
                                         # First interactive content: choosing book by title
                                         wellPanel(pickerInput(inputId = "title", 
                                                               label = "Choose a title or select all", 
                                                               selected = titles,
                                                               choices = titles,
                                                               options = pickerOptions(actionsBox = TRUE,
                                                                                       liveSearch = TRUE,
                                                                                       virtualScroll = TRUE),
                                                               multiple = TRUE)
                                                   )
                                         )
                                 ),
                                
                                fluidRow(
                                  # A static valueBox
                                  valueBox(no_titles, "Titles published", icon = icon("book-open"), width = 4),
                                  
                                  # Dynamic valueBoxes
                                 valueBoxOutput(outputId = "total_access", width = 4),
                                  
                                 valueBoxOutput(outputId = "no_countries_reached", width = 4)
                                ),
                                
                                # fluidRow(column(4,
                                #                 textOutput(outputId = "no_titles"), 
                                #                 textOutput("total_access"),
                                #                 textOutput("no_countries_reached")
                                # ),
                                
                                fluidRow( 
                                  #table of access by different metrics     
                                  box(tableOutput("metrics_table"), width = 6)
                                ), 
                                
                                wellPanel(checkboxGroupInput(inputId = "metric", 
                                                             label = "Choose metrics", 
                                                             selected = measures,
                                                             choices = measures),
                                          pickerInput(inputId = "title2", 
                                                      label = "Choose a title or select all", 
                                                      selected = titles,
                                                      choices = titles,
                                                      options = list(`actions-box` = TRUE),
                                                      multiple = T)),
                                box(plotOutput("monthly_access"), width = 12)
                        ),
                        
                        #---- Tab 2: Metrics by title---------------------------------------------------------------------------------- 
                        
                        tabItem(tabName = "Metrics_by_title",
                                wellPanel(pickerInput(inputId = "titletab_title", 
                                                      label = "Choose a title or select all", 
                                                      selected = titles,
                                                      choices = titles,
                                                      options = pickerOptions(liveSearch = TRUE,
                                                                              virtualScroll = TRUE))
                                )
                        ),
                        
                        #---- Tab 3: Metrics by country------------------------------------------------------------------------------
                        
                        tabItem(tabName = "Metrics_by_country",
                                h2("We'll put country charts here")
                                
                                
                                
                                
                                #global dot map
                                #top 10 countries bar chart
                                #views per month for selected title, split by platform/metric 
                                
                        )
                        # end of the tabbed items
                        
                      ) # end of tabItems()
                    ) # end of dashboardBody
)


##---- The Server -------------------------------------------------------------------------------
server <- function(input, output) {
  
  #---- Summary Tab: Value boxes
  output$total_access <- renderValueBox({
    total_access <- all_data %>%
      filter(title %in% input$title, !is.na(value.x)) %>% 
      pull(value.x) %>% 
      sum() %>% 
      prettyNum(big.mark = ",")
    valueBox(value = paste0(total_access), 
             subtitle = "Total Access", 
             icon = icon("book-reader"),
             color = "purple")
    
  })
  
  # output$total_access <- renderText({
  #   total_access <- all_data %>%
  #     filter(title %in% input$title, !is.na(value.x)) %>% 
  #     pull(value.x) %>% 
  #     sum() %>% 
  #     prettyNum(big.mark = ",")
  #   return(paste0("Total acccess: ", total_access))
  # })
  
  output$no_countries_reached <- renderValueBox({
    no_countries_reached <- all_data %>%
      filter(title %in% input$title, !is.na(value.x)) %>% 
      pull(country_name) %>% 
      unique() %>% 
      length()
    return(valueBox(paste0(no_countries_reached), "Countries reached", icon = icon("flag")))
  })
  
  output$metrics_table <- renderTable({
    this_data <- all_data %>%
      filter(title %in% input$title, !is.na(value.x)) %>%   
      group_by(measure)%>%
      summarise(value = prettyNum(sum(value.x),
                                  big.mark = ",")) %>%
      rename(metric = measure)
    return(this_data)
  }, align = "cr", digits = 0)
  
  output$monthly_access <- renderPlot({
    this_data <- all_data %>%
      filter(title %in% input$title2) %>% 
      filter(measure %in% input$metric) %>% 
      group_by(measure, yq)%>%
      summarise(values = sum(value.x)) %>%
      arrange(yq, measure) %>% 
      rename(metric = measure)
    
    p <- ggplot(this_data, mapping = aes(x = yq, y = values))
    p <- p + geom_col(aes(fill = metric))
    p <- p + scale_fill_manual(values = c("downloads" = brewer.pal(8, "YlGnBu")[5],
                                          "sessions" = brewer.pal(8, "YlGnBu")[6],
                                          "users" = brewer.pal(8, "YlGnBu")[7], 
                                          "views" = brewer.pal(8, "YlGnBu")[8]))
    p <- p + scale_y_continuous(labels = comma)
    p <- p + ggtitle("Readership over time")
    p <- p + ylab("Interactions")
    p <- p + xlab("")
    p <- p + scale_x_yearqtr(format = "%Y-Q%q")
    p <- p + theme_minimal()
    
    return(p) #consider return(ggplotly(p)) for interactivity, if we can get it to work
    # might need an API to publish plotly charts
  })
  
  # Map of the world
  
  # Top country bar chart
  
  # Line chart by platform
  
  
}



##---- The final command ----
shinyApp(ui = ui, server = server)
