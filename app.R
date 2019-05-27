# This script produces the shiny app that will be used to display HIRMEOS metrics

#---- Set up ------------------------------------------------------------------------------

# Here's the packages that will be needed

list.of.packages <- c("ggplot2",
                      "shiny",
                      "tidyverse",
                      "rcrossref",
                      "plotly",
                      "shinyWidgets",
                #      "countrycode",
                      "scales",
                      "zoo",
                      "lubridate",
                      "RColorBrewer",
                      "shinydashboard",
                      "leaflet")


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
#library(countrycode)
library(scales)
library(zoo)
library(lubridate)
library(RColorBrewer)
library(leaflet)

#We disable R's feature that automatically reads in strings as factors. In this work, strings are generally just strings

options(stringsAsFactors = FALSE)

# and let's define some important colours
hirmeos_orange <- "#F79C49"
hirmeos_blue <- "#0C5EA3"
operas_purple <- "#682661"
eu_blue <- '#094E97'
sidebar_blue <- "#3A80A7"
header_blue <- "#408EBA"


# Import local data and tidy------------------------------------------------------------------------------

metrics_data <- read_csv("data/metrics.csv") %>% 
  as_tibble()

meta_data <- read_csv("data/metadata.csv") %>% 
  as_tibble() %>%
  mutate(work_uri = paste0("info:doi:", doi))

altmetrics_data <- read_csv("data/altmetrics.csv") %>% 
  as_tibble()

country_geodata <- read_csv("data/country_centroids.csv") %>%
  select(country_name = admin,
         country_code =iso_a2, 
         longitude = Longitude, 
         latitude = Latitude)  # source: https://worldmap.harvard.edu/data/geonode:country_centroids_az8

all_data <- metrics_data %>%
  left_join(meta_data, by = c("work_uri" = "work_uri")) %>% 
  left_join(altmetrics_data, by = c("work_uri" = "URI")) %>%
  filter(type %in% c("monograph", "book")) %>%  # for now, we will focus on book & monograph data. If we allow chapters,
  # we'll need to generalise the search/filters in the UI to select for books/monograph/chapters
  select(measure_id, # this tells us what sort of metric we're looking at
         value.x, # the value of the metric   
         timestamp.x, # this tells us what date
         country_uri, # this tells us what country
         title, # this is the title
         publisher) %>%  # together, these are the data we'll analyse for now
  # later we will include events (altmetrics)
  mutate(country_code = ifelse(is.na(country_uri), 
                                     "missing",
                                     substr(country_uri, start = 21, stop = 22))) %>%
  left_join(country_geodata, by = "country_code") %>%
  mutate(country_name = ifelse(is.na(country_name), 
                               "No info", 
                               country_name)) %>% 
  # this step needs to be fixed either with regex, or by reference to the OPERAS measures webpage
  mutate(measure = case_when(measure_id == "https://metrics.operas-eu.org/classics-library/sessions/v1" ~ "sessions",
                             measure_id == "https://metrics.operas-eu.org/google-books/views/v1" ~ "views",
                             measure_id == "https://metrics.operas-eu.org/oapen/downloads/v1" ~ "downloads",
                             measure_id == "https://metrics.operas-eu.org/obp-html/sessions/v1" ~ "sessions",
                             measure_id == "https://metrics.operas-eu.org/obp-pdf/sessions/v1"  ~ "sessions", 
                             measure_id == "https://metrics.operas-eu.org/obp/downloads/v1" ~ "downloads",
                             measure_id == "https://metrics.operas-eu.org/open-edition/downloads/v1" ~ "downloads",
                             measure_id == "https://metrics.operas-eu.org/world-reader/users/v1" ~ "users")) %>% 
  mutate(yq = as.yearqtr(timestamp.x)) %>%  # We add year-quarters for the readership dates
  mutate(title_abbr = ifelse(nchar(title) > 125,
                             paste0(substr(title, start = 1, stop = 122), "..."),
                             title)) # Some titles are outrageously verbose; we tidy those here

# Pre-processed data (non-interactive)------------------------------------------------------------------
# Have some data pre-processed and ready to go here, so that the entire datafile doesn't need uploading
# each time someone uses the app. 

# Number of titles and names of titles in the dataset
titles <- all_data %>%
  filter(!is.na(title_abbr)) %>%
  pull(title_abbr) %>%
  unique() %>% 
  sort()

no_titles <- prettyNum(length(titles), big.mark = ",")

# Number of countries and readership by country for the dataset, and a barplot for top 10 countries

countries <- all_data %>% 
  filter(!is.na(value.x)) %>% 
  select(country_name, value.x) %>%
  group_by(country_name) %>% 
  summarise(country_access = sum(value.x))

no_countries_reached_static <- countries %>%
  pull(country_name) %>% 
  unique() %>% 
  length() %>% 
  prettyNum(big.mark = ",")

countries_top_10 <- countries %>%
  arrange(desc(country_access)) %>%
  top_n(10, wt = country_access)

countries_top_10 <- countries_top_10 %>%
  mutate(country_name = factor(x = pull(countries_top_10, country_name), 
                               levels = pull(countries_top_10, country_name),
                               ordered = T),
         country_percent = percent(round(country_access/sum(country_access), 2)))

p2 <- ggplot(countries_top_10, mapping = aes(x = country_name, y = country_access))
p2 <- p2 + geom_bar(stat = "identity", 
                    fill = hirmeos_blue)
p2 <- p2 + geom_text(aes(label = paste0(prettyNum(country_access, big.mark = ","),
                                        "  (",
                                        country_percent,
                                        ")")),
                     hjust = -0.1)
p2 <- p2 + coord_flip()
p2 <- p2 + theme_void()
p2 <- p2 + theme(axis.text.y = element_text(), 
                 title = element_blank())
p2 <- p2 + scale_x_discrete(limits = rev(levels(pull(countries_top_10,country_name))))
p2 <- p2 + scale_y_continuous(limits = c(0, 1.5*max(pull(countries_top_10, country_access))))



# Grouping metrics by measure and date, and creating a column plot over time

static_metrics_data <- all_data %>%
  filter(!is.na(measure)) %>%
  select(measure, value.x, yq) %>%
  group_by(measure, yq) %>% 
  summarise(value = sum(value.x))

total_access_static <- static_metrics_data %>% 
  pull(value) %>% 
  sum() %>% 
  prettyNum(big.mark = ",")

measures <- static_metrics_data %>%
  pull(measure) %>% 
  unique()

p1 <- ggplot(static_metrics_data, mapping = aes(x = yq, y = value))
p1 <- p1 + geom_col(aes(fill = measure))
p1 <- p1 + theme_minimal()
p1 <- p1 + scale_fill_manual(values = c("downloads" = brewer.pal(8, "YlGnBu")[5],
                                        "sessions" = brewer.pal(8, "YlGnBu")[6],
                                        "users" = brewer.pal(8, "YlGnBu")[7], 
                                        "views" = brewer.pal(8, "YlGnBu")[8]))
p1 <- p1 + scale_y_continuous(labels = comma)
p1 <- p1 + ylab("")
p1 <- p1 + xlab("")
p1 <- p1 + scale_x_yearqtr(format = "%Y-Q%q")
#p1 <- p1 + theme(legend.title = element_blank())




#---- The User Interface: Header and sidebar----------------------------------------------------------------------------

ui <- dashboardPage(
  skin = "blue",
  header = dashboardHeader(title = "Readership metrics"
  ),
  
  sidebar = dashboardSidebar(
    
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("Summary", tabName = "Summary", icon = icon("dashboard")),
      menuItem("Metrics by title", tabName = "Metrics_by_title", icon = icon("book")),
      menuItem("Global reach", tabName = "Metrics_by_country", icon = icon("globe")),
      
      # We add a horizontal line, followed by the HIRMEOS logo, OPERAS logo and EU logo
      hr(),
      
      box(
        a(href = 'https://www.hirmeos.eu/',
          img(src = 'HIRMEOS_LOGO_rect.png', 
              width = "100%")
        ), 
        width = 12, 
        solidHeader = TRUE
      ),
      
      
      br(),
      br(),
      br(),
      
      box(
        a(href = 'https://operas.hypotheses.org/',
          img(src = 'https://operas.hypotheses.org/files/2017/04/Logo-OPERAS.png', 
              width = "100%")
        ), 
        width = 12, 
        solidHeader = TRUE
        
        
      ),
      
      
      
      box(
        img(src = 'https://hirmeos.eu/wp-content/uploads/2017/03/logo-ce-horizontal-en-pantone-lr.png', 
            width = "100%"),
        width = 12, 
        solidHeader = TRUE
        
      )
    )
  ),
  
  #---- The User Interface: Body----------------------------------------------------------------------------------                    
  
  body = dashboardBody(
    
    #Set a theme if desired - either with the HTML below or using the shinythemes package
    
    # tags$head(
    #   tags$style(HTML("
    #                   .content-wrapper {
    #                   background-color: lightgrey !important;
    #                   }
    #                   .main-sidebar {
    #                   background-color: white !important;
    #                   }
    #                   "))
    #   ),
    
    tabItems(
      
      #---- Tab 1: Summary----------------------------------------------------------------------------------                              
      
      tabItem(tabName = "Summary",
              
              
              
              fluidRow(
                # A static valueBox for number of titles in database
                valueBox(value = no_titles, 
                         subtitle = "Titles published", 
                         icon = icon("book"),
                         color = "light-blue", 
                         width = 4),
                
                # A static valueBox for total access metrics
                valueBox(value = total_access_static, 
                         subtitle = "Total Access", 
                         icon = icon("book-reader"),
                         color = "teal",
                         width = 4),
                
                # A static valueBox for total number of countries reached
                valueBox(value = no_countries_reached_static, 
                         subtitle = "Countries reached", 
                         icon = icon("flag"),
                         color = "light-blue",
                         width = 4)
              ),
              
              
              fluidRow( 
                
                # adjacent to the table is the top 10 countries chart
                
                box(plotOutput("countries_barplot_all", height = "300px"), 
                    width = 8, 
                    title = "Top 10 countries by readership",
                    br(),
                    p("Note: Not all platforms provide country-level data.")
                ),
                
                #table of access by different metrics
                
                box(
                  tableOutput("metrics_table_all"),
                  br(),
                  p("Descriptions of each measure can be found ",
                    a(href = "https://metrics.operas-eu.org/measures", "here"),
                    "."),
                  
                  # Here we include the possibility of filtering by measure, but that would require reactivity
                  
                  # checkboxGroupInput(inputId = "metric1",choices = measures,
                  #                    selected = measures, label = "Select measure"
                  #                     ),
                  
                  width = 4)
              ), 
              
              # At the bottom, we have the readership by month chart
              
              fluidRow(box(plotOutput("monthly_access_all"), 
                           width = 12, 
                           title = "Quarterly readership"))
      ),
      
      #---- Tab 2: Metrics by title---------------------------------------------------------------------------------- 
      
      tabItem(tabName = "Metrics_by_title",
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
                       ),
                       offset = 0
                )
              ),
              
              fluidRow(
                
                # Dynamic valueBoxes
                valueBoxOutput(outputId = "no_titles_selected", width = 4),
                
                valueBoxOutput(outputId = "total_access", width = 4),
                
                valueBoxOutput(outputId = "no_countries_reached", width = 4)
              ),
              
              fluidRow(
                
                # adjacent to the table is the top 10 countries chart 
                box(plotOutput("countries_barplot", height = 300), 
                    title = "Top 10 countries by readership",
                    width = 8
                ),
                
                #table of access by different metrics
                
                       box(
                         tableOutput("metrics_table"),
                         width = 4
                       )
              ),
              
              fluidRow(
                
                box(
                  p("Descriptions of each measure can be found ",
                    a(href = "https://metrics.operas-eu.org/measures", "here"),
                    "."),
                  
                  checkboxGroupInput(inputId = "metric2",choices = measures,
                                     selected = measures, label = "Select measure"
                  ),
                  plotOutput("monthly_access"),
                  width = 12
                )
              )
      ),
      
      #---- Tab 3: Global reach------------------------------------------------------------------------------
      
      tabItem(tabName = "Metrics_by_country",
              wellPanel(pickerInput(inputId = "title2", 
                                    label = "Choose a title or select all", 
                                   # selected = titles,  # start with all or none selected
                                    choices = titles,
                                    options = pickerOptions(actionsBox = TRUE,
                                                            liveSearch = TRUE,
                                                            virtualScroll = TRUE),
                                    
                                    multiple = TRUE)
              ),
             fluidRow(
               box(leafletOutput("map"),
                   width = 12),
             p()
             )
              
              
              
              #global dot map
              #views per month for selected title, split by platform/metric 
              
      )
      # end of the tabbed items
      
    ) # end of tabItems()
  ) # end of dashboardBody
) # end of UI


##---- The Server -------------------------------------------------------------------------------
server <- function(input, output) {
  
  #---- Summary tab: static features-----------------
  output$metrics_table_all <- renderTable({static_metrics_data %>% 
      group_by(measure) %>% 
      summarise(Value = prettyNum(sum(value), big.mark = ",")) %>% 
      rename(Measure = measure)},
      digits = 0, 
      align = "cr"
  )
  
  output$countries_barplot_all <- renderPlot(p2)
  
  output$monthly_access_all <- renderPlot(p1)
  
  
  
  
  
  
  
  #---- Metrics by title tab-----------------------------------------------------------------
  
  # instead of filtering our dataset many times (costly), we create a reactive expression
  # that builds a new dataset with each update of input$title
  title_data <- reactive({
    all_data %>%
      filter(title_abbr %in% input$title, # this filters for the chosen title
             !is.na(value.x)) # this removes any measure with a missing value
  }) # note that title_data() is an expression that retrieves a dataset, so must be called like a function 
  
  
  output$no_titles_selected <- renderValueBox({
    title_data() %>% # note that title_data() is an expression that retrieves a dataset, hence the () 
      pull(title) %>% 
      unique() %>%
      length() %>%
      prettyNum(big.mark = ",") %>%
      valueBox( 
        subtitle = "Titles selected", 
        icon = icon("book"),
        color = "light-blue")
  })
  
  output$total_access <- renderValueBox({
    title_data() %>% # note that title_data() is an expression that retrieves a dataset, hence the () 
      pull(value.x) %>% 
      sum() %>% 
      prettyNum(big.mark = ",") %>%
      valueBox( 
        subtitle = "Total Access", 
        color = "teal")
  })
  
  
  
  output$no_countries_reached <- renderValueBox({
    title_data() %>%  # note that title_data() is an expression that retrieves a dataset, hence the () 
      pull(country_name) %>% 
      unique() %>% 
      length() %>% 
      valueBox(
        icon = icon("flag"),
        subtitle = "Countries reached",
        color = "light-blue")
  })
  
  output$metrics_table <- renderTable({
    title_data() %>%   
      group_by(measure)%>%
      summarise(value = prettyNum(sum(value.x),
                                  big.mark = ",")) %>%
      rename(metric = measure)
  }, 
  align = "cr", digits = 0
  )
  
  output$monthly_access <- renderPlot({
    this_data <- title_data() %>% 
      filter(measure %in% input$metric2) %>% 
      group_by(measure, yq)%>%
      summarise(values = sum(value.x)) %>%
      arrange(yq, measure) 
    p <- ""
    if(dim(this_data)[1] > 0){
      p <- ggplot(this_data, mapping = aes(x = yq, y = values))
      p <- p + geom_col(aes(fill = measure))
      p <- p + theme_minimal()
      p <- p + scale_fill_manual(values = c("downloads" = brewer.pal(8, "YlGnBu")[5],
                                            "sessions" = brewer.pal(8, "YlGnBu")[6],
                                            "users" = brewer.pal(8, "YlGnBu")[7], 
                                            "views" = brewer.pal(8, "YlGnBu")[8]))
      p <- p + scale_y_continuous(labels = comma)
      p <- p + ggtitle("Readership over time")
      p <- p + ylab("")
      p <- p + xlab("")
      p <- p + scale_x_yearqtr(format = "%Y-Q%q")
      
    }
    return(p) #consider return(ggplotly(p)) for interactivity, if we can get it to work
    # might need an API to publish plotly charts
  })
  
  # Top country bar chart
  output$countries_barplot <- renderPlot({
    countries_top_10 <- title_data() %>% 
      select(country_name, value.x) %>%
      group_by(country_name) %>% 
      summarise(country_access = sum(value.x)) %>% 
      arrange(desc(country_access)) %>%
      top_n(10, wt = country_access)
    
    countries_top_10 <- countries_top_10 %>%
      mutate(country_name = factor(x = pull(countries_top_10, country_name), 
                                   levels = pull(countries_top_10, country_name),
                                   ordered = T),
             country_percent = percent(country_access/sum(country_access),accuracy = 1))
    p2 <- "No titles selected"
    if(dim(countries_top_10)[1] > 0){
      p2 <- ggplot(countries_top_10, mapping = aes(x = country_name, y = country_access))
      p2 <- p2 + geom_bar(stat = "identity", 
                          fill = hirmeos_blue)
      p2 <- p2 + geom_text(aes(label = paste0(prettyNum(country_access, big.mark = ","),
                                              "  (",
                                              country_percent,
                                              ")")),
                           hjust = -0.1)
      p2 <- p2 + coord_flip()
      p2 <- p2 + theme_void()
      p2 <- p2 + theme(axis.text.y = element_text(), 
                       title = element_blank())
      p2 <- p2 + scale_x_discrete(limits = rev(levels(pull(countries_top_10,country_name))))
      p2 <- p2 + scale_y_continuous(limits = c(0, 1.5*max(pull(countries_top_10, country_access))))
    }
    return(p2)
    
  })
  
  ##---- Leaflet tab --------------------------------------------------------------------
  
  # Map of the world
  map_data <- reactive({
    all_data %>%
      filter(title_abbr %in% input$title2, # this filters for the chosen title
             !is.na(value.x),
             !is.na(longitude))%>%
      group_by(country_name) %>%
      summarise(total_access = sum(value.x),
                longitude = max(longitude, na.rm = TRUE),
                latitude = max(latitude, na.rm = TRUE)) # all longitude/latitude values should be the same for a given country
  }) 
  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>% 
      setView(lng = 0, lat = 0, zoom = 1)
  })
  
  observe({
    map_data <- map_data()
    
    leafletProxy("map", data = map_data()) %>%
      clearMarkers() %>%
      addCircleMarkers(radius = ~log(total_access), 
                 weight = 1, 
                 color = hirmeos_blue,
                 fillColor = hirmeos_blue, 
                 fillOpacity = 0.7, 
                 popup = ~paste0(country_name, ": ", total_access)
      )
  })
  
  # Line chart by platform
  
}



##---- The final command ----
shinyApp(ui = ui, server = server)
